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

-- import qualified UI.PicoUI.Raw.Widgets as Raw
-- import qualified UI.PicoUI.Raw.WidgetCompiler as Raw

-- import Data.Foldable

import qualified SDL as SDL

import UI.PicoUI.Reactive.Internal.StatefulSignals
import UI.PicoUI.Reactive.ReactiveWidgets

import PreludeFixes

{-
EventSources {
    allEvents      :: StatefulSignal SDLIO Event
  , clickEvents    :: StatefulSignal SDLIO Event
  , textEvents     :: StatefulSignal SDLIO Event
  , keyboardEvents :: StatefulSignal SDLIO Event
} 
-}
test_widgets :: SDLIO ()
test_widgets = mdo
    sdlSource <- allEvents <$> gets eventSources
    eHover <- filterS isHover sdlSource
    eClick <- clickEvents <$> gets eventSources
    -- eResized <- filterS isWindowResized sdlSource
    let logSink e = liftIO (putStrLn ("[HOVER EVENT][" ++ show (timestamp $ source e) ++ "]") >> putStrLn (show e))
    -- Ok, turns out it works PERFECTLY via basic primitives!!! Just fmap and accum in this case.
    -- So, define behavior signals, union them into a->a function signal, and accum on the pure widget - 
    -- you got a widget with behavior.
    -- w <- tr2 <^$> eHover >>= accum testLabel3
    -- creating a widget with behavior
    w <- unionsM [ filterHoverApply (setText "Hovering!") (setText "NOT Hovering :(") <^$> eHover, -- handle hover
                   filterHoverApply (setText "CLICKED!") (setText "NOT Clicked :(") <^$> eClick, -- handle click
                   -- pureTextEdit <^$> sdlSource, -- handle text edit
                   reactiveResize -- handle resizes
                 ] >>= accum testLabel3
    w1 <- unionsM [ filterHoverApply (changeBackground $ BGColor $ mBlue 500) (changeBackground $ BGColor $ mGrey 700) <^$> eHover, -- handle hover
                    reactiveResize
                  ] >>= accum testLabel
    -- creating Raw widget that runs compilation each time a widget is changed
    registerReactiveWidget w
    registerReactiveWidget w1
    -- make it so that it can receive focus
    makeFocusable w
    makeFocusable w1
    let logW wi = liftIO $ putStrLn $ "Widget is: " ++ (unpack $ text (wi::AbstractWidget) )
    -- sink w logW
    -- clickW <- onClickE w
    let logClick e = liftIO $ putStrLn $ "Click event: " ++ show e
    -- sink clickW logClick
    onClickAct w logClick
    -- liftIO $ putStrLn "Testing reactive"

-- fmapM :: MonadIO m => (Event -> (Widget -> Widget) ) -> StatefulSignal m Event -> m (StatefulSignal m (Widget -> Widget))



-- import SDL.Raw.Types (Rect(..))
testProgram :: SDLIO ()
testProgram = 
  {-
  registerWidgetWithHandlers 
    testLabel 
    compositeHandler -- pure handler that changes bg colors
    [filteredHandlerSDLIO isStoppedHover (\e -> liftIO $ putStrLn $ "Stopped hovering on: " ++ show e), setFocusOnClick ] -- IO handler for hover
  -}
  registerWidgetWithHandler testML pure  -- multiline widget
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
  ],
  isFocus = False
        
} 

testLabel = Label {
  fontData = FontDataDefault,
  text = "Hello",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 40 40 200 60,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 40 40 200 60,
  isFocus = False
}

testLabel2 = Label {
  fontData = FontDataDefault,
  text = "Stretch horizontal stick to bottom",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_SHB 20 60 20 50,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 0 0 0 0,
  isFocus = False
}

testLabel3 = Label {
  fontData = FontDataDefault,
  text = "Test Reactive",
  valign = CenterAlign, halign = LeftAlign,
  layout = l_TL 280 40 200 60,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 280 40 200 60,
  isFocus = False
}





