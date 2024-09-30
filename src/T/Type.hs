module T.Type
  ( Type(..)
  ) where

import T.Prelude


data Type
  -- real types first
  = Null
  | Bool
  | Int
  | Double
  | String
  | Regexp
  | Array
  | Record
  | Fun
  -- then pseudo-types
  -- used in numeric operators
  | Number
  -- used in `for` and stdlib functions, polymorphic over
  -- all containers
  | Iterable
  -- used in rendering values into text
  | Renderable
    deriving (Show, Eq)
