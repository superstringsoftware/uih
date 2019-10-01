{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, 
  RecursiveDo, ScopedTypeVariables #-}
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
import UI.PicoUI.Raw.Events (timestamp, source, Event, pos)

-- import Data.Foldable

import qualified SDL as SDL

import UI.PicoUI.Reactive.Internal.StatefulSignals
import UI.PicoUI.Reactive.ReactiveWidgets

import PreludeFixes


test_widgets :: SDLIO ()
test_widgets = mdo
    sdlSource <- allEvents <$> gets eventSources
    eHover <- filterS isHover sdlSource
    let logSink e = liftIO (putStrLn ("[HOVER EVENT][" ++ show (timestamp $ source e) ++ "]") >> putStrLn (show e))
    -- Ok, turns out it works PERFECTLY via basic primitives!!! Just fmap and accum in this case.
    -- So, define behavior signals, union them into a->a function signal, and accum on the pure widget - 
    -- you got a widget with behavior.
    w <- tr2 <^$> eHover >>= accum testLabel3
    let logW wi = liftIO $ putStrLn $ "Widget is: " ++ (unpack $ text (wi::AbstractWidget) )
    -- sink w logW
    -- clickW <- onClickE w
    let logClick e = liftIO $ putStrLn $ "Click event: " ++ show e
    -- sink clickW logClick
    onClickAct w logClick
    -- liftIO $ putStrLn "Testing reactive"

-- fmapM :: MonadIO m => (Event -> (Widget -> Widget) ) -> StatefulSignal m Event -> m (StatefulSignal m (Widget -> Widget))

tr2 :: Event -> Widget -> Widget
tr2 e w = if tr1 e w then (setText "Hovering!" w) else (setText "NOT Hovering :(" w)

setText :: Text -> Widget -> Widget
setText txt w = w { text = txt }

tr1 :: Event -> Widget -> Bool
tr1 e w = let (V2 x y) = pos $ source e
          in  if isInWidget x y w then True else False
-- w { text = "Hovering!" } else w { text = "NOT Hovering :(" }


-- import SDL.Raw.Types (Rect(..))
testProgram :: SDLIO ()
testProgram = 
  registerWidgetWithHandlers 
    testLabel 
    compositeHandler -- pure handler that changes bg colors
    [filteredHandlerSDLIO isStoppedHover (\e -> liftIO $ putStrLn $ "Stopped hovering on: " ++ show e), setFocusOnClick ] -- IO handler for hover
  >> registerWidgetWithHandler testML pure  -- multiline widget
  >> registerWidgetWithHandlers testLabel2 hndlEditText [setFocusOnClick] -- editable widget
  >> test_widgets
  >> pure ()
    
redOn2Click = filteredHandler (isLeftClick 2) (changeBackground $ BGColor $ mRed 500)
blueOnClick = filteredHandler (isLeftClick 1) (changeBackground $ BGColor $ mBlue 500)
-- composite handler combining the 2
-- compositeHandler w = redOn2Click w >>= blueOnClick

-- This is a typical button that highlights when hovered upon
greyOnStopHover = filteredHandler isStoppedHover (changeBackground $ BGColor $ mGrey 700)
blueOnHover     = filteredHandler isHover        (changeBackground $ BGColor $ mBlue 500)
compositeHandler w = greyOnStopHover w >>= blueOnHover

main :: IO ()
main = runSDLIO testProgram >> pure ()

testML = SimpleMultilineText { -- very basic multiline text, all in one style
  fontData = FontDataDefault,
  halign = CenterAlign,
  layout = l_SHT 20 120 20 400,
  background = BGColor $ mGrey 900, 
  cursorCol = 0, cursorLine = 0,
  cacheRect = V4 0 0 0 0,
  textLines = [
    "Line 1",
    "Line 2 that is a little bit longer"
  ]
        
} 

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
  background = BGColor $ mGrey 700, 
  cacheRect = V4 0 0 0 0
}

testLabel3 = Label {
  fontData = FontDataDefault,
  text = "Test Reactive",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 140 40 200 60,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 140 40 200 60
}





