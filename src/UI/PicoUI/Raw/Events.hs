{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, FlexibleInstances #-}

module UI.PicoUI.Raw.Events where

-- Mapping from raw SDL Events to our events

import qualified SDL as SDL
import Data.Text hiding (any)
import Foreign.C.Types (CInt, CFloat)
import Data.Word

import Linear

import PreludeFixes

import UI.PicoUI.Raw.Widgets

-- event source: widgetId and potentially mouse (touch etc) coordinates - undefined for keyboard events, and a timestamp
-- list of widget ids since an event can originate from multiple widgets (??? - better algorithm to find smallest widget on top??)
data EventSource = EventSource {
    widgetIds :: [WidgetId], 
    pos :: V2 Int, 
    timestamp :: SDL.Timestamp
} deriving (Show, Eq)
data Event = RawSDLEvent { source :: EventSource, sdlPayload :: SDL.EventPayload }
    | MouseHover { source :: EventSource }
    | LeftClick { source :: EventSource, times :: Word8} -- how many times clicked
    | RightClick { source :: EventSource, times :: Word8} -- how many times clicked
    | Quit
    deriving (Show, Eq)

castV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

