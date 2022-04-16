{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module T.Parse
  ( parse
  ) where

import           Data.Foldable (asum)
import           Data.ByteString (ByteString)
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
    x
  x :*: y ->
    x :*: cleanup y
  x ->
    x

parser :: Parser Tmpl
parser =
  go (\t -> t)
 where
  go acc =
    asum
      [ do _ <- eof
           pure (acc (Raw ""))
      , do exp <- parseExp
           go (acc . (:*:) exp)
      , do raw <- parseRaw
           go (acc . (:*:) raw)
      ]

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
      , do x <- anyChar
           go (x : acc)
      ]
