{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, FlexibleInstances #-}

module UI.PicoUI.Raw.PureHandlers where

-- SDL Event handlers for widgets that are pure - transform widget to widget

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Exception
import SDL as SDL hiding (get,el)
import SDL.Font
import Data.Text hiding (any)
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Data.Foldable as Map (length) 

import Color

import UI.PicoUI.Raw.Widgets

import Data.Vector.Generic

import PreludeFixes

-- First, we define different kinds of transformations for widgets
-- Then, we create various event handlers by combining events with these transformations

-- We want to make pure handlers composable and have less clutter in function definitions, 
-- since they transform widget to widget in response to an
-- event. So, making it a Reader monad with Event as environment!



-- reader monad     
type PureHandlerM = Reader Event
type PureHandler = Widget -> PureHandlerM Widget -- composable event handlers

instance Show PureHandler where
    show _ = "[Pure Handler]"


-- generic handler creation - give a list of filters :: Event -> Bool to it
-- and it will execute the function if any of them is true
-- genericHandler :: (Event -> Bool) -> (Event -> Widget -> Widget)
genericHandler filt pureHandler event widget = if filt event then pureHandler event widget else widget
-- monadic variant
genericHandlerM filt pureHandlerM widget = do
    event <- ask
    if filt event then pureHandlerM widget
    else return widget

-- filters for events. They remain explicitly dependent on event in case we may want to us them elsewhere.

-- Hover: simply return True for any mouse motion event, as main event loop checks whether coordinates
-- are correct
isHover :: Event -> Bool
isHover (Event _ (MouseMotionEvent _)) = True
isHover _ = False

isLeftClick :: Word8 -> Event -> Bool
isLeftClick i (Event _ (MouseButtonEvent mb)) =
    let motion = SDL.mouseButtonEventMotion mb 
        button = SDL.mouseButtonEventButton mb
        clicks = SDL.mouseButtonEventClicks mb
    in if (motion == Released) && (button == ButtonLeft) && (clicks == i) then True else False
isLeftClick _ _ = False

-- replacing i-th element of a widget with a modifier function Event -> SDLElement -> SDLElement
-- record destruction makes me cry, might want to move to mutable stuff for efficiency eventually
replaceWidgetElement :: Int
     -> (e -> SDLElement -> SDLElement)
     -> (e -> Widget -> Widget)
replaceWidgetElement i modifier evt w@Widget{..} = 
    let newElem  = modifier evt (elements!i.-el)
        newVElem = (elements!i) { el = newElem }
        newVec   = elements // [(i,newVElem)]
    in w { elements = newVec }

-- monadic: takes a monadic element modifier, returns monadic widget transformer
replaceWidgetElementM :: Int -> (SDLElement -> PureHandlerM SDLElement)
                             -> (Widget -> PureHandlerM Widget)
replaceWidgetElementM i modifierM w@Widget{..} = do
    newElem <- modifierM (elements!i.-el)
    let newVElem = (elements!i) { el = newElem }
    let newVec   = elements // [(i,newVElem)]
    return $ w { elements = newVec }

-- monadic handlers - now we can compose them!!!
boundingBoxReplacerM newBox = replaceWidgetElementM 0 (\el -> return newBox)
blueOnClickM = genericHandlerM (isLeftClick 1) (boundingBoxReplacerM (SDLBox $ rgbaToV4Color (mdBlue 500) ))
redOn2ClickM = genericHandlerM (isLeftClick 2) (boundingBoxReplacerM (SDLBox $ rgbaToV4Color (mdRed 500) ))
compositeHandler w = blueOnClickM w >>= redOn2ClickM


-- replaces 0-th element to a new box    
boundingBoxReplacer newBox = replaceWidgetElement 0 (\e el -> newBox)
-- replaces 0-th element to a new box in responce to hover events
hoverReplacer newBox = genericHandler isHover (boundingBoxReplacer newBox)
-- and finally, make a red box on hover and blue on click handlers:
redOnHover  = genericHandler isHover         (boundingBoxReplacer (SDLBox $ rgbaToV4Color (mdRed 500) ))
blueOnClick = genericHandler (isLeftClick 1) (boundingBoxReplacer (SDLBox $ rgbaToV4Color (mdBlue 500) ))
redOn2Click = genericHandler (isLeftClick 2) (boundingBoxReplacer (SDLBox $ rgbaToV4Color (mdRed 500) ))
