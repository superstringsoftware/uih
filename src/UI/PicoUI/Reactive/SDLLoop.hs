{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.Reactive.SDLLoop where

-- redefinition of event loop using our reactive engine

import Control.Monad.Trans.State.Strict
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Exception
import SDL as SDL hiding (get)
import SDL.Font
import Data.Text hiding (any)
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Data.Foldable as Map (length) 

import Color

import UI.PicoUI.Raw.Widgets
import UI.PicoUI.Raw.Rendering
import UI.PicoUI.Raw.WidgetCompiler
import UI.PicoUI.Raw.Events as P
import UI.PicoUI.PicoUIMonad as Pico

import UI.PicoUI.Reactive.Internal.StatefulSignals

emptySource :: SDL.Event -> EventSource
emptySource evt = EventSource []  (V2 0 0) (SDL.eventTimestamp evt)
sourceId i  evt = EventSource [i] (V2 0 0) (SDL.eventTimestamp evt)


