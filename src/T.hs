module T
  ( Tmpl
  , Env
  , Name(..)
  , Error

  , parse
  , render
  , mkDefEnv
  , mkEnv

  , Stdlib
  , stdlib

  , Embed(..)
  , Eject(..)
  ) where

import T.Embed (Embed(..), Eject(..))
import T.Error (Error)
import T.Exp (Name(..))
import T.Tmpl (Tmpl)
import T.Parse (parse)
import T.Render (Env, render, mkDefEnv, mkEnv)
import T.Stdlib (Stdlib, def)


stdlib :: Stdlib
stdlib = def
