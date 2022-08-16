module T
  ( Tmpl
  , Env
  , Error

  , parse
  , render
  , mkDefEnv
  , mkEnv
  , stdlib

  , Embed(..)
  , Eject(..)
  ) where

import           T.Error (Error)
import           T.Tmpl (Tmpl)
import           T.Parse (parse)
import           T.Render (Env, render, mkEnv)
import           T.Stdlib (stdlib)
