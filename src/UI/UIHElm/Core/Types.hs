{-# LANGUAGE OverloadedStrings #-}

module UI.UIHElm.Core.Types where

import Data.Word (Word8)
import Data.Text (Text)

-- | Basic geometric types
data Rect = Rect
  { rectX      :: !Int
  , rectY      :: !Int  
  , rectWidth  :: !Int
  , rectHeight :: !Int
  } deriving (Show, Eq)

data Position = Position
  { posX :: !Int
  , posY :: !Int
  } deriving (Show, Eq)

data Size = Size
  { sizeWidth  :: !Int
  , sizeHeight :: !Int
  } deriving (Show, Eq)

-- | Color type (RGBA)
data Color = Color
  { colorRed   :: !Word8
  , colorGreen :: !Word8
  , colorBlue  :: !Word8
  , colorAlpha :: !Word8
  } deriving (Show, Eq)

-- | Common colors
black, white, red, green, blue, transparent :: Color
black = Color 0 0 0 255
white = Color 255 255 255 255
red = Color 255 0 0 255
green = Color 0 255 0 255
blue = Color 0 0 255 255
transparent = Color 0 0 0 0

-- | Spacing and layout types
data Padding = Padding
  { paddingTop    :: !Int
  , paddingRight  :: !Int
  , paddingBottom :: !Int
  , paddingLeft   :: !Int
  } deriving (Show, Eq)

-- | Alignment types
data HAlign = HAlignLeft | HAlignCenter | HAlignRight
  deriving (Show, Eq)

data VAlign = VAlignTop | VAlignCenter | VAlignBottom  
  deriving (Show, Eq)

-- | Font specification
data FontSpec = FontSpec
  { fontFamily :: Text
  , fontSize   :: !Int
  } deriving (Show, Eq, Ord)

-- | Helper functions
rectFromPosSize :: Position -> Size -> Rect
rectFromPosSize (Position x y) (Size w h) = Rect x y w h

rectCenter :: Rect -> Position
rectCenter (Rect x y w h) = Position (x + w `div` 2) (y + h `div` 2)

rectContains :: Rect -> Position -> Bool
rectContains (Rect x y w h) (Position px py) =
  px >= x && px <= (x + w) && py >= y && py <= (y + h)

-- | Default values
noPadding :: Padding
noPadding = Padding 0 0 0 0

defaultPadding :: Padding
defaultPadding = Padding 8 8 8 8

defaultFont :: FontSpec
defaultFont = FontSpec "Roboto-Light" 16