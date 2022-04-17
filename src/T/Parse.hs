module T.Parse
  ( parse
  ) where

import           Control.Applicative ((<|>), liftA2)
import           Data.Bifunctor (second)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Scientific as Scientific
import           Data.String (fromString)
import qualified Data.Vector as Vector
import           Prelude hiding (exp)
import           Text.Trifecta
import           Text.Parser.Expression (Assoc(..), Operator(..), buildExpressionParser)
import           Text.Parser.LookAhead (lookAhead)
import           Text.Parser.Token.Style (emptyOps)

import           T.Exp (Tmpl(..), Exp(..), Literal(..), Name(..))


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
      , do if_ <- parseIf
           go (acc . (:*:) if_)
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
    name <- nameP
    _ <- symbol "="
    exp <- expP
    pure (Set name exp)

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
      (between
        (string "[" *> spaces) (spaces <* string "]") (sepBy expP (symbol ",")))
  objectP =
    fmap (Object . HashMap.fromList)
      (between
        (string "{" *> spaces) (spaces <* string "}") (sepBy kv (symbol ",")))
   where
    kv = do
      k <- fmap fromString (some letter)
      _ <- symbol ":"
      v <- expP
      pure (k, v)

varP :: Parser Exp
varP =
  fmap Var nameP

nameP :: Parser Name
nameP =
  fmap (Name . fromString) (liftA2 (:) letter (many (digit <|> letter))) <* spaces

parseRaw :: Parser Tmpl
parseRaw =
  fmap (Raw . fromString . reverse) (go [])
 where
  go acc =
    asum
      [ do _ <- eof
           pure acc
      , do _ <- lookAhead (string "{{")
           pure acc
      , do _ <- lookAhead (string "{%")
           pure acc
      , do x <- anyChar
           go (x : acc)
      ]

blockP :: String -> Parser a -> Parser a
blockP name p =
  between (string ("{% " <> name) *> spaces) (spaces <* string "%}") p

blockP_ :: String -> Parser ()
blockP_ name =
  blockP name (pure ())
