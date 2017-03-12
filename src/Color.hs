{-# LANGUAGE OverloadedStrings #-}
module Color where

import Data.Word

-- Materal design colors implemented as pattern-matched functions

-- RGBA color
data RGBA = RGBA {r :: !Word8, g :: !Word8, b :: !Word8, a :: !Word8} deriving (Show, Eq)
-- data RGB = RGB {r :: !Word8, g :: !Word8, b :: !Word8} deriving (Show, Eq)

-- toRGBA rgb = RGBA {r = r rgb, g = g rgb, }

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
