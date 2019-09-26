{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Color where
import Linear
import Data.Word

import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- Materal design colors implemented as pattern-matched functions
type Color = V4 Word8
-- RGBA color
data RGBA = RGBA {r :: !Word8, g :: !Word8, b :: !Word8, a :: !Word8} deriving (Show, Eq)
-- data RGB = RGB {r :: !Word8, g :: !Word8, b :: !Word8} deriving (Show, Eq)

-- toRGBA rgb = RGBA {r = r rgb, g = g rgb, }
--rgbaToSDLColor :: RGBA -> Raw.Color
rgbaToV4Color c = V4 (r c) (g c) (b c) (a c)

mBlack :: Color
mBlack = V4 0 0 0 255
mWhite :: Color
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

-- | Convert strings into hexadecimal and back.
class Hex t where
    -- | Convert string into hexadecimal.
    hex   :: t -> t
    -- | Convert from hexadecimal and fail on invalid input.
    unhex :: Monad m => t -> m t


instance Hex String where
    hex = Prelude.concatMap w
        where w ch = let s = "0123456789ABCDEF"
                         x = fromEnum ch
                     in [s !! div x 16,s !! mod x 16]
    unhex []      = return []
    unhex (a:b:r) = do x <- c a
                       y <- c b
                       liftM (toEnum ((x * 16) + y) :) $ unhex r
    unhex [_]      = fail "Non-even length"


c :: Monad m => Char -> m Int
c '0' = return 0
c '1' = return 1
c '2' = return 2
c '3' = return 3
c '4' = return 4
c '5' = return 5
c '6' = return 6
c '7' = return 7
c '8' = return 8
c '9' = return 9
c 'A' = return 10
c 'B' = return 11
c 'C' = return 12
c 'D' = return 13
c 'E' = return 14
c 'F' = return 15
c 'a' = return 10
c 'b' = return 11
c 'c' = return 12
c 'd' = return 13
c 'e' = return 14
c 'f' = return 15
c _   = fail "Invalid hex digit!"

instance Hex B.ByteString where
    hex = B.pack . hex . B.unpack
    unhex x = liftM B.pack $ unhex $ B.unpack x

instance Hex L.ByteString where
    hex = L.pack . hex . L.unpack
    unhex x = liftM L.pack $ unhex $ L.unpack x