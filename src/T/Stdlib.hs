{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T.Stdlib
  ( Stdlib(..)
  , Fun(..)
  , Op(..)
  , Op.Fixity(..)
  , Macro
  , with
  , def
  , bindings
  , Macro.macroFun
  , Macro.macroOp
  ) where

import Data.HashMap.Strict qualified as HashMap

import T.Name (Name)
import T.Prelude
import T.Stdlib.Fun (Fun(..))
import T.Stdlib.Fun qualified as Fun
import T.Stdlib.Macro (Macro(..))
import T.Stdlib.Macro qualified as Macro
import T.Stdlib.Op (Op(..))
import T.Stdlib.Op qualified as Op
import T.Value (Value)


data Stdlib = Stdlib
  { ops    :: [Op]
  , funs   :: [Fun]
  , macros :: [Macro]
  }

def :: Stdlib
def =
  with Op.operators Fun.functions Macro.macros

with :: [Op] -> [Fun] -> [Macro] -> Stdlib
with ops funs macros = Stdlib {..}

bindings :: Stdlib -> HashMap Name Value
bindings stdlib =
  HashMap.fromList (Op.bindings stdlib.ops <> Fun.bindings stdlib.funs)
