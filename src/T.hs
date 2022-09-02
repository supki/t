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

import           T.Embed (Embed(..), Eject(..))
import           T.Error (Error)
import           T.Tmpl (Tmpl)
import           T.Parse (parse)
import           T.Render (Env, render, mkDefEnv, mkEnv)
import           T.Stdlib (stdlib)
