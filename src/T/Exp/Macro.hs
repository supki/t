{-# LANGUAGE PatternSynonyms #-}
module T.Exp.Macro
  ( expand
  ) where

import T.Exp
  ( Cofree(..)
  , Exp
  , ExpF(..)
  , Name
  , Ann
  , (:+)
  , appE_
  , ifE
  , litE_
  , varE_
  , falseL
  , trueL
  )


expand :: Exp -> Exp
expand = \case
  -- 'and' macro
  ann :< Fun "&&" expl expr ->
    expand (ifE ann expl expr (litE_ falseL))

  -- 'or' macro
  ann :< Fun "||" expl expr ->
    expand (ifE ann expl (litE_ trueL) expr)

  -- 'coalesce' macro
  ann :< Fun "coalesce" expl expr ->
    expand (ifE ann (appE_ (varE_ "defined?") expl) expl expr)

  -- traverse the rest
  exp0@(_ :< Lit _) ->
    exp0
  exp0@(_ :< Var _) ->
    exp0
  ann :< If p t f ->
    ann :< If (expand p) (expand t) (expand f)
  ann :< App f x ->
    ann :< App (expand f) (expand x)

pattern Fun :: Ann :+ Name -> Exp -> Exp -> ExpF Exp
pattern Fun name exp0 exp1 <-
  App (_ :< App (_ :< Var name) exp0) exp1
