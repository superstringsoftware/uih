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
import UI.PicoUI.Middle.AbstractWidgets

-- import Data.Foldable

import qualified SDL

import UI.PicoUI.Reactive.Internal.StatefulSignals
import UI.PicoUI.Reactive.ReactiveWidgets

import UI.PicoUI.Charts.DataSeries

import UI.PicoUI.Raw.Events (timestamp, source, Event, pos)

import PreludeFixes

{-
Reusing basic example from Reactive Banana: +/- buttons and a counter
-}

test_widgets :: PicoUIM u ()
test_widgets = do
    eHover <- hoverEvents <$> gets eventSources
    w1 <- filterHoverApply (changeBackground $ BGColor $ mdOrange 500) 
                           (changeBackground $ BGColor $ mdBlueGray 700) 
                           <^$> eHover >>= accum fig1
    w  <- filterHoverApply (changeBackground $ BGColor $ mdTeal  500) 
                           (changeBackground $ BGColor $ mdBlueGray 700) 
                           <^$> eHover >>= accum fig2    
    (counter :: PicoSignal u Int) <- 
        unionsM [ (+1) <^$^ onClick w1, subtract 1 <^$^ onClick w ] >>= accum 0
    w2 <- setTextShow <^$> counter >>= accum but
    wch1 <- createReactiveWidget ch1
    registerReactiveWidgets [w, w1, w2, wch1]
    

main :: IO ()
main = runSDLIO test_widgets >> pure ()

fig1 = defaultLabel {
  text = "Up",
  layout = l_CHT (-40) 40 60 40, 
  background = BGColor $ mdBlueGray 700  
}

fig2 = defaultLabel {
  text = "Down",
  layout = l_CHT 40 40 60 40, 
  background = BGColor $ mdBlueGray 700  
}

but = defaultLabel {
  text = "0",
  layout = l_CHT 0 90 100 60, 
  background = BGColor $ mdBlueGray 500  
}

ch1 = Chart {
  layout = l_CHT 0 200 500 500,
  background = BGColor $ mdBlueGray 900,
  cacheRect = V4 0 0 0 0,
  dataSeries = sampleFunc 100 (-10) 10 (\x -> x * cos x) 
}






