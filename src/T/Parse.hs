module T.Parse
  ( parse
  , parseFile
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Char qualified as Char
import Data.Foldable (asum)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Scientific qualified as Scientific
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Prelude hiding (exp)
import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import Text.Parser.Expression (Assoc(..), Operator(..), buildExpressionParser)
import Text.Parser.LookAhead (lookAhead)
import Text.Parser.Token.Style (emptyOps)
import Text.Regex.PCRE.Light qualified as Pcre

import T.Exp
  ( Cofree(..)
  , Exp
  , ExpF(..)
  , Literal(..)
  , Name(..)
  , (:+)(..)
  , Ann
  , litE
  , litE_
  , varE
  , ifE
  , appE
  , appE_
  )
import T.Exp.Ann (anning, anned)
import T.Exp.Macro qualified as Macro
import T.Tmpl qualified as Tmpl
import T.Tmpl (Tmpl((:*:)))


parseFile :: FilePath -> ByteString -> Either ErrInfo Tmpl
parseFile path =
  parseDelta (Directed (fromString path) 0 0 0 0)

parse :: ByteString -> Either ErrInfo Tmpl
parse =
  parseDelta mempty

parseDelta :: Delta -> ByteString -> Either ErrInfo Tmpl
parseDelta delta str =
  case parseByteString parser delta str of
    Failure errDoc ->
      Left errDoc
    Success tmpl ->
      Right (cleanup tmpl)

cleanup :: Tmpl -> Tmpl
cleanup = \case
  Tmpl.Raw "" :*: x ->
    cleanup x
  x :*: Tmpl.Raw "" ->
    cleanup x
  Tmpl.If clauses ->
    Tmpl.If (fmap (second cleanup) clauses)
  Tmpl.For name it exp x y ->
    Tmpl.For name it exp (cleanup x) (fmap cleanup y)
  Tmpl.Let name exp x ->
    Tmpl.Let name exp (cleanup x)
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
      [ do comment <- parseComment
           go (acc . (:*:) comment)
      , do set <- parseSet
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

parseComment :: Parser Tmpl
parseComment =
  fmap Tmpl.Comment commentP

parseSet :: Parser Tmpl
parseSet =
  blockP "set" $ do
    name <- nameP
    _ <- symbol "="
    exp <- expP
    pure (Tmpl.Set name exp)

parseLet :: Parser Tmpl
parseLet = do
  (name, exp) <- blockP "let" $ do
    name <- nameP
    _ <- symbol "="
    exp <- expP
    pure (name, exp)
  tmpl <- parser
  _ <- blockP_ "endlet"
  pure (Tmpl.Let name exp tmpl)

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
        fmap (\tmpl -> (litE_ (Bool True), tmpl)) elseTmplQ
  case elseClauseQ of
    Nothing ->
      pure (Tmpl.If (ifClause :| thenClauses))
    Just elseClause ->
      pure (Tmpl.If ((ifClause :| thenClauses) <> (elseClause :| [])))

parseCase :: Parser Tmpl
parseCase = do
  exp0 <- blockP "case" expP
  when : whens <- some (liftA2 (,) (blockP "when" expP) parser)
  elseTmplQ <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endcase"
  let clauses =
        fmap (\(exp1, tmpl) -> (appE_ "==" (fromList [exp0, exp1]), tmpl)) (when :| whens)
      elseClauseQ =
        fmap (\tmpl -> (litE_ (Bool True), tmpl)) elseTmplQ
  case elseClauseQ of
    Nothing ->
      pure (Tmpl.If clauses)
    Just elseClause ->
      pure (Tmpl.If (clauses <> (elseClause :| [])))

parseFor :: Parser Tmpl
parseFor = do
  (name, it, exp) <- blockP "for" $ do
    name <- nameP
    it <- optional $ do
      _ <- symbol ","
      nameP
    _ <- symbol "in"
    exp <- expP
    pure (name, it, exp)
  forTmpl <- parser
  elseTmpl <- optional (blockP_ "else" *> parser)
  _ <- blockP_ "endfor"
  pure (Tmpl.For name it exp forTmpl elseTmpl)

parseExp :: Parser Tmpl
parseExp =
  between (string "{{" *> spaces) (spaces <* string "}}") (fmap Tmpl.Exp expP)

expP :: Parser Exp
expP =
  fmap Macro.expand (buildExpressionParser table expP')
 where
  table =
    [ [dotOp "."]
    , [prefixOp "!"]
    , [infixrOp "*", infixrOp "/"]
    , [infixrOp "+", infixrOp "-"]
    , [ infixOp "=="
      , infixOp "=~"
      , infixOp ">"
      , infixOp ">="
      , infixOp "<"
      , infixOp "<="
      ]
    , [infixrOp "&&"]
    , [infixrOp "||"]
    , [infixlOp "|"]
    ]
   where
    dotOp name =
      Infix
        (do ann <- anning (reserve emptyOps name)
            pure (\a b ->
              appE ann
                (fromString name)
                (fromList
                  [ a
                  -- Property lookups have the syntax of variables, but
                  -- we actually want them as strings.
                  , (case b of ann' :< Var (_ :+ Name b') -> litE ann' (String b'); _ -> b)
                  ])))
        AssocLeft
    binaryOp name =
      Infix
        (do ann <- anning (reserve emptyOps name)
            pure (\a b -> appE ann (fromString name) (fromList [a, b])))
    infixOp name =
      binaryOp name AssocNone
    infixlOp name =
      binaryOp name AssocLeft
    infixrOp name =
      binaryOp name AssocRight
    prefixOp name =
      Prefix
        (do ann <- anning (reserve emptyOps name)
            pure (\a -> appE ann (fromString name) (fromList [a])))
  expP' =
    asum
      [ parens expP
      , litP
      , ifP
        -- We have to use `try` here because, unfortunately, function
        -- application `f(x)` shares a prefix with the standalone
        -- variable lookup `f`.
      , try appP
      , varP
      ]

litP :: Parser Exp
litP = do
  ann :+ lit <- anned $ asum
    [ nullP
    , boolP
    , numberP
    , stringP
    , regexpP
    , arrayP
    , objectP
    ]
  pure (litE ann lit)
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

ifP :: Parser Exp
ifP = do
  ann :+ (p, t, f) <- anned $ do
    _ <- symbol "if"
    p <- expP
    _ <- symbol "then"
    t <- expP
    _ <- symbol "else"
    f <- expP
    pure (p, t, f)
  pure (ifE ann p t f)

appP :: Parser Exp
appP = do
  ann :+ (name, args) <- anned $ do
    name <- nameP
    args <-
      between (symbol "(" *> spaces) (spaces *> symbol ")") (sepByNonEmpty expP (symbol ","))
    pure (name, args)
  pure (appE ann name args)

varP :: Parser Exp
varP = do
  name@(ann :+ _) <- nameP
  pure (varE ann name)

nameP :: Parser (Ann :+ Name)
nameP =
  anned (fmap fromString (liftA2 (:) firstL (many restL))) <* spaces
 where
  firstL =
    letter <|> char '_'
  restL =
    letter <|> digit <|> oneOf "_-?!"

parseRaw :: Parser Tmpl
parseRaw =
  fmap (Tmpl.Raw . fromString . reverse) (go [])
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

-- | Line blocks "remove" all the whitespace around the block
-- including the newline; inline blocks do not touch it.
--
-- this is a line block: ^{% set foo = 4 %}\n$
-- this is an inline block: ^foo{% set foo = 4 %}bar$
blockP :: String -> Parser a -> Parser a
blockP name p =
  try lineP <|> inlineP
 where
  -- the weird `"{%" <> " " <> name` construction avoids *a lot* of `try`ing,
  -- because all statements start with `{%`
  lineP =
    between
      (spaces *> string (beginWith <> " " <> name) *> spaces)
      (spaces <* string endWith <* spacesExceptNewline <* newline)
      p
  inlineP =
    between (string (beginWith <> " " <> name) *> spaces) (spaces <* string endWith) p
  (beginWith, endWith) =
    ("{%", "%}")

-- parse name-only blocks such as {% endlet %}
blockP_ :: String -> Parser ()
blockP_ name =
  blockP name (pure ())

commentP :: Parser Text
commentP =
  try lineP <|> inlineP
 where
  lineP = do
    -- unfortunately, this `between` isn't /symmetrical/, because
    -- `p` eats the ending "#}"
    between
      (spaces *> string beginWith *> spaces)
      (spacesExceptNewline <* newline)
      p
  inlineP =
    string beginWith *> spaces *> p
  (beginWith, endWith) =
    ("{#", "#}")
  p =
    fmap fromString (manyTill anyChar (try (spaces *> string endWith)))

spacesExceptNewline :: Parser String
spacesExceptNewline =
  many spaceExceptNewline

spaceExceptNewline :: Parser Char
spaceExceptNewline =
  satisfy (\c -> Char.isSpace c && (c /= '\n'))
