{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, 
TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module UI.PicoUI.Middle.Handlers where

-- Impure handlers in SDLIO monad, based on PureHandlers

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Control.Exception
import Data.Text hiding (any)
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Data.Foldable as Map (length) 

import Color

import UI.PicoUI.Middle.AbstractWidgets
import UI.PicoUI.Middle.PureHandlers
import UI.PicoUI.Raw.Events

import UI.PicoUI.PicoUIMonad

import Data.Vector.Generic

import PreludeFixes

-- type PureHandler e = AbstractWidget -> Reader Event AbstractWidget 

-- simply runs an SDLIO action if a filter is ok
filteredHandlerSDLIO :: (Event -> Bool) -> EventHandler -> Event -> SDLIO ()
filteredHandlerSDLIO filt handler event = if filt event then handler event else pure ()

-- sets focus id to the first element in source ids list if a filter is true
setFocusOn :: (Event -> Bool) -> Event -> SDLIO ()
setFocusOn filt = 
    filteredHandlerSDLIO filt 
        (\evt -> let EventSource{..} = source evt in setCurFocusId (Prelude.head widgetIds) )

-- shortcut
setFocusOnClick = setFocusOn (isLeftClick 1)   

-- MAIN WINDOW HANDLER - gets called on all events that do not originate in widgets, e.g. for resetting focus
mHndlMainWindow = filteredHandlerSDLIO isSourceMainWindow 
    (filteredHandlerSDLIO (isLeftClick 1) (\e -> setCurFocusId (-1)))
    
    