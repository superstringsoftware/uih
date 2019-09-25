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
type EventSource = (WidgetId, V2 Int, SDL.Timestamp)
data Event = RawSDLEvent { source :: EventSource, sdlPayload :: SDL.EventPayload }
    | MouseHover { source :: EventSource }
    | LeftClick { source :: EventSource, times :: Word8} -- how many times clicked
    | RightClick { source :: EventSource, times :: Word8} -- how many times clicked
    deriving Show

castV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

sdlEvent2Event :: WidgetId -> SDL.Event -> Event
sdlEvent2Event i event = 
    case SDL.eventPayload event of
            SDL.MouseButtonEvent mb -> 
                -- MouseButtonEventData {mouseButtonEventWindow = Just (Window 0x00007f9500c38fb0), 
                --    mouseButtonEventMotion = Released, mouseButtonEventWhich = Mouse 0, mouseButtonEventButton = ButtonLeft, 
                --    mouseButtonEventClicks = 1, mouseButtonEventPos = P (V2 407 170)}
                -- Pressed or Released. Analogous to key presses we will only think about Released as "clicks"
                -- SDL helpfully reports # of clicks = 0 in case the mouse was moved after pressing.
                -- This way, we can handle proper clicks as well as "press a button, drag, release" type of thing eventually
                let motion = SDL.mouseButtonEventMotion mb 
                    button = SDL.mouseButtonEventButton mb
                    clicks = SDL.mouseButtonEventClicks mb
                    SDL.P pos  = SDL.mouseButtonEventPos mb
                    cons   = if button == SDL.ButtonLeft then LeftClick else RightClick
                --let P (V2 x y) = SDL.mouseButtonEventPos mb
                in  cons (i, castV2 pos, SDL.eventTimestamp event) clicks
            p -> RawSDLEvent (i, V2 0 0, SDL.eventTimestamp event) p
