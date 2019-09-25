{-# LANGUAGE OverloadedStrings #-}
module Color where
import Linear
import Data.Word

-- Materal design colors implemented as pattern-matched functions
type Color = V4 Word8
-- RGBA color
data RGBA = RGBA {r :: !Word8, g :: !Word8, b :: !Word8, a :: !Word8} deriving (Show, Eq)
-- data RGB = RGB {r :: !Word8, g :: !Word8, b :: !Word8} deriving (Show, Eq)

-- toRGBA rgb = RGBA {r = r rgb, g = g rgb, }
--rgbaToSDLColor :: RGBA -> Raw.Color
rgbaToV4Color c = V4 (r c) (g c) (b c) (a c)

mBlack = V4 0 0 0 255
mWhite = V4 255 255 255 255

-- mGrey :: Int -> RGBA
mGrey 500 = V4 0x9E 0x9E 0x9E 255
mGrey 50  = V4 0xFA 0xFA 0xFA 255
mGrey 100 = V4 0xF5 0xF5 0xF5 255
mGrey 300 = V4 0xE0 0xE0 0xE0 255
mGrey 700 = V4 0x61 0x61 0x61 255
mGrey 900 = V4 0x21 0x21 0x21 255
mGrey _   = V4 0x9E 0x9E 0x9E 255

-- mBlue :: Int -> RGBA
mBlue 500 = V4 0x21 0x96 0xF3 255
mBlue _   = V4 0x21 0x96 0xF3 255

-- mRed :: Int -> RGBA
mRed 500  = V4 0xF4 0x43 0x36 255
mRed  _   = V4 0xF4 0x43 0x36 255



mdBlack = RGBA 0 0 0 255
mdWhite = RGBA 255 255 255 255

mdGrey :: Int -> RGBA
mdGrey 500 = RGBA 0x9E 0x9E 0x9E 255
mdGrey 50  = RGBA 0xFA 0xFA 0xFA 255
mdGrey 100 = RGBA 0xF5 0xF5 0xF5 255
mdGrey 300 = RGBA 0xE0 0xE0 0xE0 255
mdGrey 700 = RGBA 0x61 0x61 0x61 255
mdGrey 900 = RGBA 0x21 0x21 0x21 255
mdGrey _   = RGBA 0x9E 0x9E 0x9E 255

mdBlue :: Int -> RGBA
mdBlue 500 = RGBA 0x21 0x96 0xF3 255
mdBlue _   = RGBA 0x21 0x96 0xF3 255

mdRed :: Int -> RGBA
mdRed 500  = RGBA 0xF4 0x43 0x36 255
mdRed  _   = RGBA 0xF4 0x43 0x36 255
