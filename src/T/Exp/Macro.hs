{-# LANGUAGE PatternSynonyms #-}
module T.Exp.Macro
  ( expand
  ) where

import Data.Maybe (catMaybes, listToMaybe)
import T.Exp
  ( Cofree(..)
  , Exp
  , ExpF(..)
  , Name
  , Ann
  , (:+)
  , ifE_
  , litE_
  , falseL
  , trueL
  )


expand :: Exp -> Exp
expand exp0 =
  case listToMaybe (catMaybes (fmap (\f -> f exp0) macros)) of
    Just exp1 ->
      expand exp1
    Nothing ->
      case exp0 of
        _ :< Lit _ ->
          exp0
        _ :< Var _ ->
          exp0
        ann :< If p t f ->
          ann :< If (expand p) (expand t) (expand f)
        ann :< App f x ->
          ann :< App (expand f) (expand x)
 where
  macros =
    [ macroAnd
    , macroOr
    ]

macroAnd :: Exp -> Maybe Exp
macroAnd = \case
  Op "&&" exp0 exp1 ->
    Just (ifE_ exp0 exp1 (litE_ falseL))
  _ ->
    Nothing

macroOr :: Exp -> Maybe Exp
macroOr = \case
  Op "||" exp0 exp1 ->
    Just (ifE_ exp0 (litE_ trueL) exp1)
  _ ->
    Nothing

pattern Op :: Ann :+ Name -> Exp -> Exp -> Exp
pattern Op name exp0 exp1 <-
  _ :< App (_ :< App (_ :< Var name) exp0) exp1
