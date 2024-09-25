module T
  ( Tmpl
  , Env
  , Scope(..)
  , Value
  , Name(..)
  , Error(..)
  , prettyError
  , Warning(..)
  , prettyWarning

  , parse
  , parseFile
  , render
  , embedAeson

  , Stdlib
  , stdlib

  , Embed(..)
  , Eject(..)
  ) where

import T.Embed (Embed(..), Eject(..))
import T.Error (Error(..), Warning(..), prettyError, prettyWarning)
import T.Name (Name(..))
import T.Tmpl (Tmpl)
import T.Parse (parse, parseFile)
import T.Render (Env, Scope(..), render)
import T.Stdlib (Stdlib, def)
import T.Value (Value, embedAeson)


stdlib :: Stdlib
stdlib = def
