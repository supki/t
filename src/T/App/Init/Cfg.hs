{-# LANGUAGE StrictData #-}
module T.App.Init.Cfg
  ( Cfg(..)
  ) where

import T qualified


data Cfg = Cfg
  { init        :: FilePath
  , env         :: T.Env
  , rootDir     :: FilePath
  , skipTestRun :: Bool
  }
