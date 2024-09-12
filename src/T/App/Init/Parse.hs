module T.App.Init.Parse
  ( Stmt(..)
  , Parser
  , parseText
  ) where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Prelude hiding (lines)
import Text.Trifecta
  ( Result
  , anyChar
  , foldResult
  , manyTill
  , parseByteString
  , spaces
  , string
  , try
  )

import T.Tmpl (Tmpl)
import T.Parse qualified as T (parser)


data Stmt
  = Noop
  | Setup Tmpl
  | File FilePath Tmpl
    deriving (Show)

type Parser a =
  Text -> Either String a

parseText :: Parser [Stmt]
parseText str0 =
  pragma noop [] (Text.lines str0)
 where
  pragma parser0 acc (line : lines0) =
    case findParserByPrefix line parsers of
      -- finding a parser means that we've encountered a pragma,
      -- so we need to parse the accumulator with the previous parser
      Just parser -> do
        let
          str =
            Text.unlines (reverse acc)
        stmt <- parser0 str
        stmts <- pragma parser [line] lines0
        pure (stmt : stmts)
      Nothing ->
        pragma parser0 (line : acc) lines0
  pragma parser acc [] = do
    let
      str =
        Text.unlines (reverse acc)
    stmt <- parser str
    pure [stmt]

findParserByPrefix :: Text -> [(Text, Parser a)] -> Maybe (Parser a)
findParserByPrefix line =
  fmap snd . List.find (\(prefix, _p) -> Text.isPrefixOf prefix line)

parsers :: [(Text, Text -> Either String Stmt)]
parsers =
  [ ("{# NOOP", noop)
  , ("{# SETUP", setup)
  , ("{# FILE", file)
  ]

noop :: Parser Stmt
noop _str =
  pure Noop

setup :: Parser Stmt
setup str =
  fmap Setup (fromResult (parseByteString p mempty (Text.encodeUtf8 str)))
 where
  p = do
    _ <- string "{# SETUP #}\n"
    T.parser

file :: Parser Stmt
file str =
  fmap (uncurry File) (fromResult (parseByteString p mempty (Text.encodeUtf8 str)))
 where
  p = do
   _ <- string "{# FILE"
   spaces
   name <- manyTill anyChar (try (spaces *> string "#}\n"))
   tmpl <- T.parser
   pure (name, tmpl)

fromResult :: Result a -> Either String a
fromResult =
  foldResult (Left . show) pure