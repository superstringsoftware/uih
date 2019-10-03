{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists,
    TypeSynonymInstances, RecordWildCards #-}
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

import UI.PicoUI.Reactive.Internal.StatefulSignals
import UI.PicoUI.Reactive.ReactiveWidgets

import UI.PicoUI.Raw.Events (timestamp, source, Event, pos)

import PreludeFixes

setText :: Text -> Widget -> Widget
setText txt w = w { text = txt }



test_widgets :: SDLIO ()
test_widgets = do
    sdlSource <- allEvents <$> gets eventSources
    eHover <- filterS isHover sdlSource
    eClick <- clickEvents <$> gets eventSources
    eResized <- filterS isWindowResized sdlSource
    let logSink e = liftIO (putStrLn ("[HOVER EVENT][" ++ show (timestamp $ source e) ++ "]") >> putStrLn (show e))
    -- Ok, turns out it works PERFECTLY via basic primitives!!! Just fmap and accum in this case.
    -- So, define behavior signals, union them into a->a function signal, and accum on the pure widget - 
    -- you got a widget with behavior.
    -- w <- tr2 <^$> eHover >>= accum testLabel3
    -- creating a widget with behavior
    w <- unionsM [ filterHoverApply (setText "Hovering!") (setText "NOT Hovering :(") <^$> eHover, -- handle hover
                   filterHoverApply (setText "CLICKED!") (setText "NOT Clicked :(") <^$> eClick, -- handle click
                   -- pureTextEdit <^$> sdlSource, -- handle text edit
                   reactiveResize-- handle resizes
                 ] >>= accum fig1
    w1 <- unionsM [ filterHoverApply (changeBackground $ BGColor $ mBlue 500) (changeBackground $ BGColor $ mGrey 700) <^$> eHover, -- handle hover
                    reactiveResize
                  ] >>= accum fig2
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

main :: IO ()
main = runSDLIO test_widgets >> pure ()

fig1 = Label {
  fontData = FontDataDefault,
  text = "Up",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 40 40 60 40,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 0 0 0 0,
  isFocus = False
}

fig2 = Label {
  fontData = FontDataDefault,
  text = "Down",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 120 40 60 40,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 0 0 0 0,
  isFocus = False
}

but = Label {
  fontData = FontDataDefault,
  text = "0",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 40 100 140 60,
  background = BGColor $ mGrey 500, 
  cacheRect = V4 0 0 0 0,
  isFocus = False
}






