module T
  ( Tmpl
  , Env
  , Error

  , parse
  , render

  , envFromJson
  ) where

import           T.Error (Error)
import           T.Tmpl (Tmpl)
import           T.Parse (parse)
import           T.Render (Env, render, envFromJson)
