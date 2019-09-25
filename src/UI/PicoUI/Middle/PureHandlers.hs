{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, FlexibleInstances #-}

module UI.PicoUI.Middle.PureHandlers where

-- SDL Event handlers for widgets that are pure - transform widget to widget

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Exception
import Data.Text hiding (any)
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Data.Foldable as Map (length) 

import Color

import UI.PicoUI.Middle.AbstractWidgets
import UI.PicoUI.Raw.Events

import Data.Vector.Generic

import PreludeFixes

-- First, we define different kinds of transformations for widgets
-- Then, we create various event handlers by combining events with these transformations

-- We want to make pure handlers composable and have less clutter in function definitions, 
-- since they transform widget to widget in response to an
-- event. So, making it a Reader monad with Event as environment!



-- reader monad     
-- type PureHandlerM = Reader Event

-- composable transformers with context e - simply reader monad actions
type PureHandler = AbstractWidget -> Reader Event AbstractWidget 

instance Show PureHandler where
    show _ = "[Pure Handler]"


-- generic handler creation - give a filter :: Event -> Bool to it
-- and it will execute the function if any of them is true
-- genericHandler :: (Event -> Bool) -> (Event -> Widget -> Widget)
-- monadic variant
filteredHandler :: (Event -> Bool) -> (AbstractWidget -> AbstractWidget) -> AbstractWidget -> Reader Event AbstractWidget
filteredHandler filt pureHandler widget = do
    event <- ask
    if filt event then return $ pureHandler widget
    else return widget

-- version that takes a monadic transformer:
filteredHandlerM :: (Event -> Bool) -> PureHandler -> AbstractWidget -> Reader Event AbstractWidget
filteredHandlerM filt pureHandlerM widget = do
    event <- ask
    if filt event then pureHandlerM widget
    else return widget


-- TRANSFORMERS: pure alteration of AbstractWidgets
-- They can be used as a basis to build handlers

-- EXAMPLES FIRST
-- change background on single click and double click
redOn2Click = filteredHandler (isLeftClick 2) (changeBackground $ BGColor $ mRed 500)
blueOnClick = filteredHandler (isLeftClick 1) (changeBackground $ BGColor $ mRed 500)
-- composite handler combining the 2
compositeHandler w = redOn2Click w >>= blueOnClick

-- change background to the new one
changeBackground :: Background -> AbstractWidget -> AbstractWidget
changeBackground bg widg = widg { background = bg }


-- filters for events. They remain explicitly dependent on event in case we may want to us them elsewhere.

-- Hover: simply return True for any mouse motion event, as main event loop checks whether coordinates
-- are correct
isHover :: Event -> Bool
isHover (MouseHover _ ) = True
isHover _ = False

isLeftClick :: Word8 -> Event -> Bool
isLeftClick i (LeftClick _ num) = if (num == i) then True else False
isLeftClick _ _ = False

