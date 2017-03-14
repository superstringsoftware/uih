{-# LANGUAGE OverloadedStrings #-}
module Widgets where

import Data.Text
import CSS.Box

-- we want to make it a Type Class?
data Widget =
    TextLabel { -- single line text
        text :: Text,
        box :: Box
    }
    | TextPanel { -- multiline text
        text :: Text,
        box :: Box
    }
    | Panel { -- simply a bounding box like a div
        box :: Box
    }

-- data WidgetTree = 
