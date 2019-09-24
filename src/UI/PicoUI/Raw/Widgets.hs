{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists #-}
module UI.PicoUI.Raw.Widgets where

-- 
import Data.Text
import Foreign.C.Types (CInt)
import Data.Word
import SDL hiding (Vector)
import SDL.Font

import Data.Vector
import Data.IORef

import Color


-- represents styled piece of text
-- font data is provided separately
data SDLStyledText = SDLStyledText {
    text :: !Text,
    color :: V4 Word8,
    styles :: [Style],
    -- if not empty, we use the "shaded" rendering method to produce backround box in one go
    -- can be used to highlight the text etc
    bgColor :: Maybe (V4 Word8) 
}

-- low level SDL widgets used for caching
data SDLElement = SDLBox { -- simply a colored box (eventually need to add with an image)
    bgColor :: V4 Word8
} | SDLText { -- text without any background
    text :: Text,
    font :: Font, -- SDL font object to render with
    color :: V4 Word8, -- color to render text with
    cursorPos :: !Int
} | SDLTextLine { -- text line with different styles but the same font and size
    font :: Font,
    texts :: Vector SDLStyledText,
    cursorPos :: !Int
}

-- elements to render in turn + shift from the parent in terms of position
data WidgetElement = WidgetElement {
    el :: !SDLElement,
    offset :: V2 CInt
}

data Widget = WidgetVec {
    isVisible :: Bool,
    collider :: V4 CInt, -- bounding box
    elements :: Vector WidgetElement -- list of elements
} | Widget {
    isVisible :: Bool,
    collider :: V4 CInt, -- bounding box
    element :: WidgetElement -- 1 element
}

isInWidget x y widg = 
    let (V4 a b w h) = collider widg
    in  if ( (x>a) && (x < a + w) && (y>b) && (y<b+h) ) then True else False


type Handler a = a -> IO ()
type EventHandler = Handler Event

-- takes initial SDLElement, pure transformation function and returns stateful computation
mkStatefulElement :: SDLElement -> (Event -> SDLElement -> SDLElement) -> IO EventHandler
mkStatefulElement tl f = do
    elementState <- newIORef tl
    let resfunc evt = modifyIORef' elementState (f evt)
    return resfunc



