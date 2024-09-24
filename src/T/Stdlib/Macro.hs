{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
module T.Stdlib.Macro
  ( Macro(..)
  , Op(..)
  , Expansion
  , macros
  , expansions
  , priorities
  , macroFun
  , macroOp
  , and
  , or
  , coalesce
  ) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map

import T.Exp
  ( Cofree(..)
  , ExpF(..)
  , appE
  , appE_
  , ifE
  , litE_
  , falseL
  , trueL
  )
import T.Name (Name)
import T.Parse.Macro
  ( Expansion
  , badArity
  , badArgument
  )
import T.Prelude
import T.Stdlib.Op qualified as Op (Fixity(..), PriorityMap)


data Macro = Macro
  { name      :: Name
  , expansion :: Expansion
  , op        :: Maybe Op
  }

data Op = Op
  { fixity   :: Op.Fixity
  , priority :: Int
  }

macros :: [Macro]
macros =
  [ macroOp "&&" and Op.Infixr 3
  , macroOp "||" or Op.Infixr 2
  , macroOp "|" legacyApp Op.Infixl 1
  , macroFun "coalesce" coalesce
  ]

macroFun :: Name -> Expansion -> Macro
macroFun name expansion =
  Macro name expansion Nothing

macroOp :: Name -> Expansion -> Op.Fixity -> Int -> Macro
macroOp name expansion fixity priority =
  Macro name expansion (Just (Op fixity priority))

expansions :: [Macro] -> [(Name, Expansion)]
expansions =
  map (\macro -> (macro.name, macro.expansion))

priorities :: [Macro] -> Op.PriorityMap
priorities =
    Map.fromListWith (<>)
  . mapMaybe (\macro -> map (\op -> (op.priority, [(macro.name, op.fixity)])) macro.op)

-- lazy 'and' macro:
--
-- {{ x && y }} -> {{ if x then y else false }}
and :: Expansion
and ann [expl, expr] =
  Right (ifE ann expl expr (litE_ falseL))
and _ann args =
  badArity 2 (List.length args)

-- lazy 'or' macro:
--
-- {{ x || y }} -> {{ if x then true else y }}
or :: Expansion
or ann [expl, expr] =
  Right (ifE ann expl (litE_ trueL) expr)
or _ann args =
  badArity 2 (List.length args)

-- function application macro:
--
-- {{ x || f }} -> {{ f(x) }}
-- {{ y || f(x) }} -> {{ f(x, y) }}
legacyApp :: Expansion
legacyApp _ann = \case
  [expl, annf :< Var name] ->
     Right (appE annf name [expl])
  [expl, annf :< App name args] ->
     Right (appE annf name (args <> [expl]))
  [_expl, expr] ->
    badArgument expr
  args ->
    badArity 2 (List.length args)

-- COALESCE macro;
--
-- {{ coalesce(x, y, ...) }} -> {{ if defined?(x) then x else (if defined? (y) then y else ...) }}
coalesce :: Expansion
coalesce ann args =
  Right (foldr1 (\arg acc -> ifE ann (appE_ "defined?" [arg]) arg acc) args)
