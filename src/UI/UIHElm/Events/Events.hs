{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UI.UIHElm.Events.Events where

import qualified SDL
import Data.Text (Text)
import qualified Data.Map as Map
import Foreign.C.Types (CInt)

import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget

-- | System events that can occur
data SystemEvent
  = MouseClick Position     -- Mouse clicked at position
  | MouseMove Position      -- Mouse moved to position  
  | KeyPress KeyCode        -- Key was pressed
  | WindowResize Size       -- Window was resized
  | WindowClose             -- User wants to close window
  deriving (Show, Eq)

-- | Keyboard key codes
data KeyCode = KeyEscape | KeyEnter | KeySpace | KeyBackspace 
             | KeyLeft | KeyRight | KeyUp | KeyDown
             | KeyTab | KeyOther CInt
  deriving (Show, Eq, Ord)

-- | Event map for spatial click detection
data EventMap msg = EventMap
  { emClickHandlers :: [(Rect, msg)]  -- Areas that can be clicked and their messages
  , emKeyHandlers   :: Map.Map KeyCode msg  -- Global key handlers
  } deriving Show

-- | Layout information for a positioned widget (define here to avoid circular imports)
data LayoutWidget msg = LayoutWidget
  { lwWidget :: Widget msg
  , lwRect   :: Rect
  }

-- | Build event map from positioned widgets
buildEventMap :: [LayoutWidget msg] -> EventMap msg
buildEventMap layoutWidgets = EventMap
  { emClickHandlers = concatMap extractClickHandlers layoutWidgets
  , emKeyHandlers = Map.empty  -- TODO: Add key handlers
  }
  where
    extractClickHandlers :: LayoutWidget msg -> [(Rect, msg)]
    extractClickHandlers (LayoutWidget widget rect) = case widget of
      Button _ msg _ -> [(rect, msg)]
      _ -> []

-- | Find which message should be sent for a click at the given position
findClickHandler :: Position -> EventMap msg -> Maybe msg
findClickHandler pos EventMap{..} = 
  case filter (positionInRect pos . fst) emClickHandlers of
    [] -> Nothing
    ((_, msg):_) -> Just msg  -- Return first match
  where
    positionInRect :: Position -> Rect -> Bool
    positionInRect (Position px py) (Rect rx ry rw rh) =
      px >= rx && px <= (rx + rw) && py >= ry && py <= (ry + rh)

-- | Convert SDL event to system event
sdlEventToSystemEvent :: SDL.Event -> Maybe SystemEvent
sdlEventToSystemEvent event = case SDL.eventPayload event of
  SDL.MouseButtonEvent mb -> 
    if SDL.mouseButtonEventMotion mb == SDL.Released
    then let SDL.P (SDL.V2 x y) = SDL.mouseButtonEventPos mb
             pos = Position (fromIntegral x) (fromIntegral y)
         in Just (MouseClick pos)
    else Nothing
    
  SDL.MouseMotionEvent mm ->
    let SDL.P (SDL.V2 x y) = SDL.mouseMotionEventPos mm
        pos = Position (fromIntegral x) (fromIntegral y)
    in Just (MouseMove pos)
    
  SDL.KeyboardEvent kb ->
    if SDL.keyboardEventKeyMotion kb == SDL.Pressed
    then Just (KeyPress $ convertKeyCode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym kb)
    else Nothing
    
  SDL.WindowResizedEvent wr ->
    let SDL.V2 w h = SDL.windowResizedEventSize wr
        size = Size (fromIntegral w) (fromIntegral h)
    in Just (WindowResize size)
    
  SDL.QuitEvent -> Just WindowClose
  
  _ -> Nothing

-- | Convert SDL KeyCode to our KeyCode
convertKeyCode :: SDL.Keycode -> KeyCode
convertKeyCode key = case key of
  SDL.KeycodeEscape    -> KeyEscape
  SDL.KeycodeReturn    -> KeyEnter
  SDL.KeycodeSpace     -> KeySpace
  SDL.KeycodeBackspace -> KeyBackspace
  SDL.KeycodeLeft      -> KeyLeft
  SDL.KeycodeRight     -> KeyRight
  SDL.KeycodeUp        -> KeyUp
  SDL.KeycodeDown      -> KeyDown
  SDL.KeycodeTab       -> KeyTab
  other                -> KeyOther 0  -- TODO: Properly convert SDL keycode

-- | Process system event and return message if any
processSystemEvent :: SystemEvent -> EventMap msg -> Maybe msg
processSystemEvent sysEvent eventMap = case sysEvent of
  MouseClick pos -> findClickHandler pos eventMap
  KeyPress key -> Map.lookup key (emKeyHandlers eventMap)
  _ -> Nothing  -- Other events don't generate messages for now