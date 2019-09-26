{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main where

import Color
import Linear
import Data.Text hiding (any)
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad

import UI.PicoUI.PicoUIMonad
import UI.PicoUI.EventLoop
import UI.PicoUI.Middle.PureHandlers
import UI.PicoUI.Middle.Handlers
import UI.PicoUI.Middle.AbstractWidgets

-- import Data.Foldable

import qualified SDL as SDL

-- import SDL.Raw.Types (Rect(..))
testProgram :: SDLIO ()
testProgram = 
  registerWidgetWithHandlers 
    testLabel 
    compositeHandler -- pure handler
    [filteredHandlerSDLIO isHover (\e -> liftIO $ print e) ] -- IO handler for hover
  >> registerWidgetWithHandlers testLabel2 hndlEditText [setFocusOnClick]
  >> pure ()
    

main :: IO ()
main = runSDLIO testProgram >> pure ()

testLabel = Label {
  fontData = FontDataDefault,
  text = "Hello",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 40 40 200 60,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 0 0 0 0
}

testLabel2 = Label {
  fontData = FontDataDefault,
  text = "Stretch horizontal stick to bottom",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_SHB 20 60 20 50,
  background = BGColor $ mGrey 900, 
  cacheRect = V4 0 0 0 0
}




