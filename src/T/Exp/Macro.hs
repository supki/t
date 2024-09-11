{-# LANGUAGE OverloadedLists #-}
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
  , appE
  , appE_
  , ifE
  , litE_
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

  -- 'function application' macro ({{ arg | f }})
  _ann :< Fun "|" expl (annf :< Var name) ->
    expand (appE annf name [expl])
  -- 'function application' macro ({{ arg | f(another-arg) }})
  _ann :< Fun "|" expl (annf :< App name args) ->
    expand (appE annf name (args <> [expl]))

  -- 'coalesce' macro
  ann :< App "coalesce" args ->
    expand (foldr1 (\arg acc -> ifE ann (appE_ "defined?" [arg]) arg acc) args)

  -- traverse the rest
  exp0@(_ :< Lit _) ->
    exp0
  exp0@(_ :< Var _) ->
    exp0
  ann :< If p t f ->
    ann :< If (expand p) (expand t) (expand f)
  ann :< App name xs ->
    ann :< App name (fmap expand xs)

pattern Fun :: Ann :+ Name -> Exp -> Exp -> ExpF Exp
pattern Fun name exp0 exp1 <-
  App name [exp0, exp1]
