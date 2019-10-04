{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists,
    TypeSynonymInstances, RecordWildCards, ScopedTypeVariables #-}
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

{-
Reusing basic example from Reactive Banana: +/- buttons and a counter
-}

test_widgets :: SDLIO ()
test_widgets = do
    sdlSource <- allEvents <$> gets eventSources
    eHover <- filterS isHover sdlSource
    w1 <- unionsM [ filterHoverApply (changeBackground $ BGColor $ mBlue 500) (changeBackground $ BGColor $ mGrey 700) <^$> eHover, -- handle hover
                    reactiveResize
                  ] >>= accum fig1
    w  <- unionsM [ filterHoverApply (changeBackground $ BGColor $ mRed 500) (changeBackground $ BGColor $ mGrey 700) <^$> eHover, -- handle hover
                    reactiveResize
                  ] >>= accum fig2    
    cw  <- onClick w
    cw1 <- onClick w1
    -- let counter :: StatefulSignal SDLIO Int
    (counter :: StatefulSignal SDLIO Int) <- unionsM [ (+1) <^$ cw1, (subtract 1) <^$ cw ] >>= accum 0
    ci <- readVal counter
    w2 <- unionsM [ reactiveResize, (\ci -> setText (pack $ show ci)) <^$> counter
                  ] >>= accum but
    -- creating Raw widget that runs compilation each time a widget is changed
    registerReactiveWidgets [w, w1, w2]
    

-- fmapM :: MonadIO m => (Int -> (Widget->Widget)) -> StatefulSignal m Int -> m (StatefulSignal m (Widget->Widget))
    

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






