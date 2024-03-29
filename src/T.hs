module T
  ( Tmpl
  , Env
  , Name(..)
  , Error(..)
  , prettyError
  , Warning(..)
  , prettyWarning

  , parse
  , parseFile
  , render
  , mkDefEnv
  , mkEnv

  , Stdlib
  , stdlib

  , Embed(..)
  , Eject(..)
  ) where

import T.Embed (Embed(..), Eject(..))
import T.Error (Error(..), Warning(..), prettyError, prettyWarning)
import T.Exp (Name(..))
import T.Tmpl (Tmpl)
import T.Parse (parse, parseFile)
import T.Render (Env, render, mkDefEnv, mkEnv)
import T.Stdlib (Stdlib, def)


stdlib :: Stdlib
stdlib = def
