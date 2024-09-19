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
  , with
  , def
  , bindings
  ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

import T.Name (Name)
import T.Stdlib.Fun (Fun(..))
import T.Stdlib.Fun qualified as Fun
import T.Stdlib.Op (Op(..))
import T.Stdlib.Op qualified as Op
import T.Value (Value)


data Stdlib = Stdlib
  { ops  :: [Op]
  , funs :: [Fun]
  }

def :: Stdlib
def =
  with Op.operators Fun.functions

with :: [Op] -> [Fun] -> Stdlib
with ops funs = Stdlib {..}

bindings :: Stdlib -> HashMap Name Value
bindings stdlib =
  HashMap.fromList (Op.bindings stdlib.ops <> Fun.bindings stdlib.funs)
