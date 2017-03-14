{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Screen.RawWidgets where

import Color
import Linear

import Foreign.C.Types (CInt)

-- type holding all widget types to process child / parent etc stuff
data Widget = WBox Box | WPanel Panel | WTextLabel TextLabel | EmptyWidget
data WidgetParent = WidgetParent Widget [Widget]

-- line styles used in borders - "none" will be Nothing
data LineStyle = Solid | Dotted | Dashed deriving (Show, Eq)

data Shadow = Shadow {
    offsetX :: !Int,
    offsetY :: !Int,
    color   :: RGBA
} deriving (Show, Eq)

-- need to follow CSS eventually
data Border = Border {
    width :: !Int,       -- computed width, ALWAYS in pixels
    color :: RGBA,
    style :: LineStyle
} deriving (Show, Eq)

-- helper function to calculate rendering
convertBorderWidth :: Maybe Border -> CInt
convertBorderWidth Nothing = 0
convertBorderWidth (Just b) = fromIntegral $ width (b :: Border)

-- box without borders and other fancy stuff for quickly putting stuff on screen
data Box = Box {
    globalX :: !Int,
    globalY :: !Int, -- top-left corner coordinates relative to the root window
    parentX :: !Int,
    parentY :: !Int, -- top-left corner coordinates relative to the parent
    width :: !Int,   -- computed width, ALWAYS in pixels
    height :: !Int,  -- computed height, ALWAYS in pixels
    color :: RGBA
} deriving (Show, Eq)

-- fancier box with shadow, borders and padding options
data Panel = Panel {
    box :: Box,
    shadow :: Maybe Shadow,
    borderTop :: Maybe Border,
    borderRight :: Maybe Border,
    borderBottom :: Maybe Border,
    borderLeft :: Maybe Border,
    padding :: V4 Int -- internal padding for the elements
} deriving (Show, Eq)

-- basic text label
data TextLabel = TextLabel {
    textBox :: Box,
    text :: String,
    fontSize :: !Int
} deriving (Show, Eq)
