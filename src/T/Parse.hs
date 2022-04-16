{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module T.Parse
  ( parse
  ) where

import           Data.Bifunctor (second)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Scientific as Scientific
import           Data.String (fromString)
import           Prelude hiding (exp)
import           Text.Trifecta
import           Text.Parser.LookAhead (lookAhead)

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
  between (string "{% set" *> spaces) (spaces <* string "%}") $ do
    name <- nameP
    _ <- symbol "="
    exp <- expP
    pure (Set name exp)

parseIf :: Parser Tmpl
parseIf = do
  exp <- ifBlock
  ifTmpl <- parser
  thenClauses <- many $ do
    exp' <- elifBlock
    elifTmpl <- parser
    pure (exp', elifTmpl)
  elseTmplQ <- optional $ do
    _ <- elseBlock
    parser
  _ <- endIfBlock
  let ifClause =
        (exp, ifTmpl)
      elseClauseQ =
        fmap (\tmpl -> (Lit (Bool True), tmpl)) elseTmplQ
  case elseClauseQ of
    Nothing ->
      pure (If (ifClause :| thenClauses))
    Just elseClause ->
      pure (If ((ifClause :| thenClauses) <> (elseClause :| [])))
 where
  ifBlock =
    between (string "{% if" *> spaces) (spaces <* string "%}") expP
  elifBlock =
    between (string "{% elif" *> spaces) (spaces <* string "%}") expP
  elseBlock =
    between (string "{% else" *> spaces) (spaces <* string "%}") (pure ())
  endIfBlock =
    between (string "{% endif" *> spaces) (spaces <* string "%}") (pure ())

parseExp :: Parser Tmpl
parseExp =
  between (string "{{" *> spaces) (spaces <* string "}}") (fmap Exp expP)

expP :: Parser Exp
expP =
  asum
    [ lit
    , var
    ]

lit :: Parser Exp
lit =
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
    fmap Array (between (string "[" *> spaces) (spaces <* string "]") (sepBy expP (symbol ",")))
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

var :: Parser Exp
var =
  fmap Var nameP

nameP :: Parser Name
nameP =
  fmap Name (sepByNonEmpty (fmap fromString (some letter)) (string ".")) <* spaces

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
