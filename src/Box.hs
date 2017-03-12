{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Box where

import Color
import Linear

data Shadow = Shadow {
    offsetX :: !Int,
    offsetY :: !Int,
    color   :: RGBA
} deriving (Show, Eq)

data Border = Border {
    width :: !Int,
    color :: RGBA
} deriving (Show, Eq)

data Box = Box {
    width :: !Int,
    height :: !Int,
    color :: RGBA,
    shadow :: Maybe Shadow,
    border :: Maybe Border,
    padding :: V4 Int
} deriving (Show, Eq)
