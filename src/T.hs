module T
  ( Tmpl
  , Scope(..)
  , Exp
  , Value
  , Rendered(..)
  , Name(..)
  , Error(..)
  , prettyError
  , Warning(..)
  , prettyWarning

  , ParseError(..)
  , parseFile
  , parseText
  , parseBytes
  , render
  , embedAeson

  , Stdlib
  , stdlib
  , emptyScope

  , Embed(..)
  , Eject(..)
  ) where

import T.Embed (Embed(..), Eject(..))
import T.Error
  ( Error(..)
  , Warning(..)
  , prettyError
  , prettyWarning
  )
import T.Exp (Exp)
import T.Name (Name(..))
import T.Tmpl (Tmpl)
import T.Parse
  ( ParseError(..)
  , parseFile
  , parseText
  , parseBytes
  )
import T.Prelude
import T.Render (Rendered(..), Scope(..), render)
import T.Stdlib (Stdlib, def)
import T.Value (Value, embedAeson)


stdlib :: Stdlib
stdlib = def

emptyScope :: Scope
emptyScope =
  Scope mempty
