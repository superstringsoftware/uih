{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module UIH.SDL2.SDLWidgets where

-- 
import Data.Text
import Foreign.C.Types (CInt)
import Data.Word
import SDL
import SDL.Font

import Color

import UIH.UI.AbstractWidgets

-- since AbstractWidget may be converted to a bunch of low-level widgets,
-- we need to map ALL of them to the same ID to implement caching and partial redraw
-- So, here we simply render all SDLWidgets in order into the FIRST texture
-- May need to change from List to some other container if performance is not good

-- mirroring tree structure of the CompositeWidget - needed for dimensions recalculations,
-- arguably the most expensive operation
-- Then the flow is: 
-- - top level SDLComplexWidgets have their coordinates relative to the screen
-- - the rest relative to the parent
-- data SDLComplexWidget = SDLComplexWidget SDLWidget [SDLComplexWidget]

-- simply rendering everything in order, 1st widget serves as target texture
type SDLComplexWidget = [SDLWidget]

-- low level SDL widgets used for caching
data SDLWidget = SDLBox { -- simply a colored box (eventually need to add with an image)
    bgColor :: V4 Word8,
    cachedRect :: V4 CInt, -- cached bounding box dimensions
    tex :: Maybe Texture -- cached texture
} | SDLText { -- text without any background
    text :: Text,
    font :: Font, -- SDL font object to render with
    color :: V4 Word8, -- color to render text with
    cachedRect :: V4 CInt, -- cached bounding box dimensions
    tex :: Maybe Texture -- cached texture
} | SDLTextBox { -- text with box as a background
    text :: Text,
    font :: Font, -- SDL font object to render with
    color :: V4 Word8, -- color to render text with
    cachedRect :: V4 CInt, -- cached bounding box dimensions
    bgColor :: V4 Word8, -- background color for the box
    paddingRect :: V4 CInt, -- padding for the text texture relative to the bounding box
    tex :: Maybe Texture -- cached texture
} 

rect2CInt (V4 x y w h) = V4 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

background2Color (BGColor clr) = clr
background2Color _ = rgbaToV4Color $ mdRed 500



