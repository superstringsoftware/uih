{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module CSS.Box () where

import Color
import Linear
import GHC.Real

import Foreign.C.Types (CInt)

-- width, length, height etc based on CSS
data Dimension = -- EM Int | -- em
                 PX !Int | -- px
                 PC !Int | -- percentage
                 Auto     -- auto
                 deriving (Show, Eq)

-- convert dimension to pixels given a parent pixel size
dimToPixel :: Int -> Dimension -> Int
dimToPixel _ (PX x) = x
dimToPixel p (PC x) = round $ fromIntegral (p * x) / 100.0

-- line styles used in borders - "none" will be Nothing
data LineStyle = Solid | Dotted | Dashed deriving (Show, Eq)

data Shadow = Shadow {
    offsetX :: !Int,
    offsetY :: !Int,
    color   :: RGBA
} deriving (Show, Eq)

-- need to follow CSS eventually
data Border = Border {
    widthI :: Dimension, -- initial width, can be in differen dimensions
    width :: !Int,       -- computed width, ALWAYS in pixels
    color :: RGBA,
    style :: LineStyle
} deriving (Show, Eq)

-- helper function to calculate rendering
convertBorderWidth :: Maybe Border -> CInt
convertBorderWidth Nothing = 0
convertBorderWidth (Just b) = fromIntegral $ width (b :: Border)

data Box = Box {
    width :: !Int,   -- computed width, ALWAYS in pixels
    height :: !Int,  -- computed height, ALWAYS in pixels
    color :: RGBA,
    shadow :: Maybe Shadow,
    borderTop :: Maybe Border,
    borderRight :: Maybe Border,
    borderBottom :: Maybe Border,
    borderLeft :: Maybe Border,
    padding :: V4 Int, -- internal padding for the elements
    margin :: V4 Int  -- external margin with other elements (as in CSS)
} deriving (Show, Eq)
