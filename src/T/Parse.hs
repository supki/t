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
  between (string "{{" *> spaces) (spaces <* string "}}") (fmap Exp exp)
 where
  exp :: Parser Exp
  exp =
    asum
      [ lit
      , var
      ]
  lit :: Parser Exp
  lit =
    fmap Lit $ asum
      [ null
      , bool
      , number
      , string
      ]
   where
    null =
      Null <$ symbol "null"
    bool =
      asum
        [ Bool False <$ symbol "false"
        , Bool True <$ symbol "true"
        ]
    number =
      fmap (Number . either fromIntegral Scientific.fromFloatDigits) integerOrDouble
    string =
      fmap String stringLiteral

  var :: Parser Exp
  var =
    fmap (Var . Name) (sepByNonEmpty (fmap fromString (some letter)) (string "."))

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
