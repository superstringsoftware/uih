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

setTextShow :: Show a => a -> Widget -> Widget
setTextShow a w = w { text = pack $ show a }

{-
Reusing basic example from Reactive Banana: +/- buttons and a counter
-}

test_widgets :: SDLIO ()
test_widgets = do
    sdlSource <- allEvents <$> gets eventSources
    eHover <- filterS isHover sdlSource
    w1 <- filterHoverApply (changeBackground $ BGColor $ mBlue 500) 
                           (changeBackground $ BGColor $ mGrey 700) 
                           <^$> eHover >>= accum fig1
    w  <- filterHoverApply (changeBackground $ BGColor $ mRed  500) 
                           (changeBackground $ BGColor $ mGrey 700) 
                           <^$> eHover >>= accum fig2    
    cw  <- onClick w
    cw1 <- onClick w1
    (counter :: StatefulSignal SDLIO Int) <- unionsM [ (+1) <^$ cw1, (subtract 1) <^$ cw ] >>= accum 0
    w2 <- setTextShow <^$> counter >>= accum but
    registerReactiveWidgets [w, w1, w2]
    

main :: IO ()
main = runSDLIO test_widgets >> pure ()

fig1 = defaultLabel {
  text = "Up",
  layout = l_CHT (-40) 40 60 40, 
  background = BGColor $ mGrey 700  
}

fig2 = defaultLabel {
  text = "Down",
  layout = l_CHT 40 40 60 40, 
  background = BGColor $ mGrey 700  
}

but = defaultLabel {
  text = "0",
  layout = l_CHT 0 90 100 60, 
  background = BGColor $ mGrey 500  
}






