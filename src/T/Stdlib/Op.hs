{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
module T.Stdlib.Op
  ( Op(..)
  , PriorityMap
  , Fixity(..)
  , bindings
  , priorities
  , operators
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Text.Regex.PCRE.Light qualified as Pcre

import T.Embed (embed0)
import T.Name (Name)
import T.Value (Value(..))



data Op = Op
  { name     :: Name
  , binding  :: Name -> Value
  , fixity   :: Fixity
  , priority :: Int
  }

type PriorityMap = Map Int [(Name, Fixity)]

data Fixity
  = Prefix
  | Infix
  | Infixl
  | Infixr
    deriving (Show, Eq)

bindings :: [Op] -> [(Name, Value)]
bindings =
  map (\op -> (op.name, op.binding op.name))

priorities :: [Op] -> PriorityMap
priorities =
  Map.fromListWith (<>) . map (\op -> (op.priority, [(op.name, op.fixity)]))

operators :: [Op]
operators =
  [ Op "!" (flip embed0 not) Prefix 8

  , Op "==" (flip embed0 eq) Infix 4
  , Op "!=" (flip embed0 neq) Infix 4
  , Op "=~" (flip embed0 match) Infix 4

  , Op "+" (flip embed0 ((+) @Scientific)) Infixl 6
  , Op "-" (flip embed0 ((-) @Scientific)) Infixl 6
  , Op "*" (flip embed0 ((*) @Scientific)) Infixl 7
  , Op "/" (flip embed0 ((/) @Scientific)) Infixl 7

  , Op ">" (flip embed0 ((>) @Scientific)) Infix 4
  , Op ">=" (flip embed0 ((>=) @Scientific)) Infix 4
  , Op "<" (flip embed0 ((<) @Scientific)) Infix 4
  , Op "<=" (flip embed0 ((<=) @Scientific)) Infix 4

  , Op "<>" (flip embed0 ((<>) @Text)) Infixr 6
  ]

eq :: Value -> Value -> Bool
eq x y =
  case (x, y) of
    (Null, Null) ->
      True
    (Bool b0, Bool b1) ->
      b0 == b1
    (Number n0, Number n1) ->
      n0 == n1
    (String s0, String s1) ->
      s0 == s1
    (Array arr0, Array arr1)
      | Vector.length arr0 == Vector.length arr1 -> do
          Vector.and (Vector.zipWith eq arr0 arr1)
      | otherwise ->
          False
    (Object o0, Object o1)
      | HashMap.null (HashMap.difference o0 o1) &&
        HashMap.null (HashMap.difference o1 o0) ->
          HashMap.foldl' (&&) True (HashMap.intersectionWith eq o0 o1)
      | otherwise ->
          False
    (_, _) ->
      False

neq :: Value -> Value -> Bool
neq x y =
  not (eq x y)

match :: Text -> Pcre.Regex -> Bool
match str regexp =
  isJust (Pcre.match regexp (Text.encodeUtf8 str) [])
