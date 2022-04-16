module T
  ( Tmpl
  , Env

  , parse
  , render

  , envFromJson
  ) where

import           T.Exp (Tmpl)
import           T.Parse (parse)
import           T.Render (Env, render, envFromJson)
