{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
-- Silly terminal monad for testing purposes - look at SDLIO to see the real thing

module Screen.DummyIO where

import Screen.LowLevelWidgets
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Data.Text

instance Widget Panel () where
  renderAt _ _ w = liftIO $ print w

instance Widget Text () where
  renderAt _ _ w = liftIO $ print w
