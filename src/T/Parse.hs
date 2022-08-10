module T.Parse
  ( parse
  ) where

import           Control.Applicative ((<|>), liftA2)
import           Data.Bifunctor (second)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.Char as Char
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Scientific as Scientific
import           Data.String (fromString)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import           Prelude hiding (exp)
import           Text.Trifecta
import           Text.Parser.Expression (Assoc(..), Operator(..), buildExpressionParser)
import           Text.Parser.LookAhead (lookAhead)
import           Text.Parser.Token.Style (emptyOps)
import qualified Text.Regex.PCRE.Light as Pcre

import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..))
import           T.Exp.Ann (anned)


parse :: ByteString -> Either String Tmpl
parse str =
  case parseByteString parser mempty str of
    Failure errDoc ->
      Left (show errDoc)
    Success tmpl ->
      Right (cleanup tmpl)

cleanup :: Tmpl -> Tmpl
cleanup = \case
  Raw "" :*: x ->
    cleanup x
  x :*: Raw "" ->
    cleanup x
  If clauses ->
    If (fmap (second cleanup) clauses)
  For name it exp x y ->
    For name it exp (cleanup x) (fmap cleanup y)
  Let name exp x ->
    Let name exp (cleanup x)
  x :*: y ->
    cleanup x :*: cleanup y
  x ->
    x

parser :: Parser Tmpl
parser =
  go (\t -> t)
 where
  go acc =
    asum
      [ do set <- parseSet
           go (acc . (:*:) set)
      , do let_ <- parseLet
           go (acc . (:*:) let_)
      , do if_ <- parseIf
           go (acc . (:*:) if_)
      , do case_ <- parseCase
           go (acc . (:*:) case_)
      , do for <- parseFor
           go (acc . (:*:) for)
      , do exp <- parseExp
           go (acc . (:*:) exp)
        -- `parseRaw` succeeds on the empty string; so to avoid
        -- looping indefinitely, we check that the parser position
        -- moved at least a bit.
      , do pos0 <- position
           raw <- parseRaw
           pos1 <- position
           bool (go (acc . (:*:) raw)) (pure (acc raw)) (pos0 == pos1)
      ]

parseSet :: Parser Tmpl
parseSet =
  blockP "set" $ do
    name <- anned nameP
    _ <- symbol "="
    exp <- expP
    pure (Set name exp)

parseLet :: Parser Tmpl
parseLet = do
  (name, exp) <- blockP "let" $ do
    name <- anned nameP
    _ <- symbol "="
    exp <- expP
    pure (name, exp)
  tmpl <- parser
  _ <- blockP_ "endlet"
  pure (Let name exp tmpl)

parseIf :: Parser Tmpl
parseIf = do
  exp <- blockP "if" expP
  ifTmpl <- parser
  thenClauses <- many (liftA2 (,) (blockP "elif" expP) parser)
  elseTmplQ <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endif"
  let ifClause =
        (exp, ifTmpl)
      elseClauseQ =
        fmap (\tmpl -> (Lit (Bool True), tmpl)) elseTmplQ
  case elseClauseQ of
    Nothing ->
      pure (If (ifClause :| thenClauses))
    Just elseClause ->
      pure (If ((ifClause :| thenClauses) <> (elseClause :| [])))

parseCase :: Parser Tmpl
parseCase = do
  exp0 <- blockP "case" expP
  when : whens <- some (liftA2 (,) (blockP "when" expP) parser)
  elseTmplQ <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endcase"
  let clauses =
        fmap (\(exp1, tmpl) -> (App (App (Var "==") exp0) exp1, tmpl)) (when :| whens)
      elseClauseQ =
        fmap (\tmpl -> (Lit (Bool True), tmpl)) elseTmplQ
  case elseClauseQ of
    Nothing ->
      pure (If clauses)
    Just elseClause ->
      pure (If (clauses <> (elseClause :| [])))

parseFor :: Parser Tmpl
parseFor = do
  (name, it, exp) <- blockP "for" $ do
    name <- anned nameP
    it <- optional $ do
      _ <- symbol ","
      anned nameP
    _ <- symbol "in"
    exp <- expP
    pure (name, it, exp)
  forTmpl <- parser
  elseTmpl <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endfor"
  pure (For name it exp forTmpl elseTmpl)

parseExp :: Parser Tmpl
parseExp =
  between (string "{{" *> spaces) (spaces <* string "}}") (fmap Exp expP)

expP :: Parser Exp
expP =
  buildExpressionParser table expP'
 where
  table =
    [ [dotOp "."]
    , [prefixOp "!"]
    , [infixrOp "*", infixrOp "/"]
    , [infixrOp "+", infixrOp "-"]
    , [infixOp "==", infixOp "=~"]
    , [infixrOp "&&"]
    , [infixrOp "||"]
    ]
   where
    dotOp name =
      Infix
        (do reserve emptyOps name
            pure (\a b ->
              App
                (App
                  (Var (Name (fromString name)))
                  a)
                -- Property lookups have the syntax of variables, but
                -- we actually want them as strings.
                (case b of Var (Name b') -> Lit (String b'); _ -> b)))
        AssocLeft
    infixOp name =
      Infix
        (reserve emptyOps name *> pure (\a b -> App (App (Var (Name (fromString name))) a) b))
        AssocNone
    infixrOp name =
      Infix
        (reserve emptyOps name *> pure (\a b -> App (App (Var (Name (fromString name))) a) b))
        AssocRight
    prefixOp name =
      Prefix
        (reserve emptyOps name *> pure (\a -> App (Var (Name (fromString name))) a))
  expP' =
    asum
      [ litP
        -- We have to use `try` here because, unfortunately, function
        -- application `f(x)` shares a prefix with the standalone
        -- variable lookup `f`.
      , try appP
      , varP
      ]

appP :: Parser Exp
appP = do
  var <- varP
  arg :| args <-
    between (string "(" *> spaces) (spaces *> string ")") (sepByNonEmpty expP (symbol ","))
  pure (foldl' (\acc arg' -> App acc arg') (App var arg) args)

litP :: Parser Exp
litP =
  fmap Lit $ asum
    [ nullP
    , boolP
    , numberP
    , stringP
    , regexpP
    , arrayP
    , objectP
    ]
 where
  nullP =
    Null <$ symbol "null"
  boolP =
    asum
      [ Bool False <$ symbol "false"
      , Bool True <$ symbol "true"
      ]
  numberP =
    fmap (Number . either fromIntegral Scientific.fromFloatDigits) integerOrDouble
  stringP =
    fmap String stringLiteral
  arrayP =
    fmap (Array . Vector.fromList)
      (brackets (sepBy expP (symbol ",")))
  objectP =
    fmap (Object . HashMap.fromList)
      (braces (sepBy kv (symbol ",")))
   where
    kv = do
      k <- fmap fromString (some letter)
      _ <- symbol ":"
      v <- expP
      pure (k, v)

regexpP :: Parser Literal
regexpP = do
  str <- between (char '/') (char '/') (normalChar [])
  modifiers <- many modifier
  spaces
  either fail (pure . Regexp) (Pcre.compileM (Text.encodeUtf8 str) (Pcre.utf8 : modifiers))
 where
  normalChar acc = do
    c <- noneOf ['/']
    case c of
      '\\' -> excapedChar acc
      _ -> normalChar (c : acc)
   <|>
    pure (fromString (reverse acc))
  excapedChar acc = do
    c <- anyChar
    case c of
      '/' ->
        normalChar ('/' : acc)
      _ ->
        normalChar (c : '\\' : acc)
  modifier = do
    'i' <- char 'i'
    pure Pcre.caseless

varP :: Parser Exp
varP =
  fmap Var nameP

nameP :: Parser Name
nameP =
  fmap (Name . fromString) (liftA2 (:) firstL (many restL)) <* spaces
 where
  firstL =
    letter <|> char '_'
  restL =
    letter <|> digit <|> char '_'

parseRaw :: Parser Tmpl
parseRaw =
  fmap (Raw . fromString . reverse) (go [])
 where
  go acc =
    asum
      [ do _ <- eof
           pure acc
           -- Attempt to fish for a line block.
      , do _ <- lookAhead (try (spacesExceptNewline *> string "{%" *> manyTill anyChar (try (string "%}")) *> spacesExceptNewline *> newline))
           pure acc
           -- Attempt to fish for a inline block.
      , do _ <- lookAhead (string "{{" <|> string "{%")
           pure acc
      , do x <- anyChar
           go (x : acc)
      ]

blockP :: String -> Parser a -> Parser a
blockP name p =
  -- The whitespace is either significant or not, depending on
  -- what kind of a block we can parse. Line blocks "remove" all
  -- the whitespace from the result; inline blocks preserve all of it.
  try lineP <|> inlineP
 where
  lineP =
    between
      (spaces *> string ("{% " <> name) *> spaces)
      (spaces <* string "%}" <* spacesExceptNewline <* newline)
      p
  inlineP =
    between (string ("{% " <> name) *> spaces) (spaces <* string "%}") p

blockP_ :: String -> Parser ()
blockP_ name =
  blockP name (pure ())

spacesExceptNewline :: Parser String
spacesExceptNewline =
  many spaceExceptNewline

spaceExceptNewline :: Parser Char
spaceExceptNewline =
  satisfy (\c -> Char.isSpace c && (c /= '\n'))
