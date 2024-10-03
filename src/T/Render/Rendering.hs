module T.Render.Rendering
  ( Rendering(..)
  ) where

import Data.Set (Set)
import Data.Text.Lazy.Builder (Builder)

import T.Exp.Ann (Ann)
import T.Error (Warning(..))
import T.Name (Name(..))
import T.Prelude
import T.Value (Value)


-- | The state of the renderer.
data Rendering = Rendering
  { scope    :: HashMap Name (Ann, Value)
  , result   :: Builder
  , warnings :: Set (Ann, Warning)
  }
