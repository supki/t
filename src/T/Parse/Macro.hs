{-# LANGUAGE OverloadedLists #-}
module T.Parse.Macro
  ( Expansion
  , expand
  , ExpansionError(..)
  , badArity
  , badArgument
  , displayError
  ) where

import Data.List qualified as List

import T.Exp
  ( Exp
  , ExpF(..)
  , Ann
  , (:+)(..)
  )
import T.Name (Name)
import T.Prelude


type Expansion =
  Ann -> NonEmpty Exp -> Either ExpansionError Exp

data ExpansionError
  = BadArity Int Int
  | BadArgument Exp

expand :: [(Name, Expansion)] -> Exp -> Either ExpansionError Exp
expand ms =
  go
 where
  go = \case
    -- expand known macros
    ann :< App (_ann :+ name) args
      | Just m <- List.lookup name ms -> do
          exp <- m ann args
          go exp

    -- traverse the rest
    exp0@(_ :< Lit _) ->
      pure exp0
    exp0@(_ :< Var _) ->
      pure exp0
    ann :< If p0 t0 f0 -> do
      p <- go p0
      t <- go t0
      f <- go f0
      pure (ann :< If p t f)
    ann :< App name xs0 -> do
      xs <- traverse go xs0
      pure (ann :< App name xs)
    ann :< Idx exp0 expIdx0 -> do
      exp <- go exp0
      expIdx <- go expIdx0
      pure (ann :< Idx exp expIdx)
    ann :< Key exp0 key -> do
      exp <- go exp0
      pure (ann :< Key exp key)


badArity :: Int -> Int -> Either ExpansionError a
badArity expected actual =
  Left (BadArity expected actual)

badArgument :: Exp -> Either ExpansionError a
badArgument exp =
  Left (BadArgument exp)

displayError :: ExpansionError -> String
displayError = \case
  BadArity _expected _actual ->
    ""
  BadArgument _exp ->
    ""
