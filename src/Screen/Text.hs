{-
Module handling low-level independent representation of different text styles. Can be used for rendering on different low-level
representations - e.g., Terminal or SDL based or potentially browser-based.
-}

{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Screen.Text where

import Color
import Linear
import Data.Text

data TextStyles = Bold | Italic | Strikethrough | Underline

-- piece of text that can be decorated by different colors
data DecoratedText = DecoratedText
    {
        text :: Text
      , fontName :: Text
      , fontSize :: !Int
      , textStyles :: [TextStyles]
      , color :: RGBA
    }

-- a line of text - this means, the text itself has to be pre-processed for *newlines* and split into these lines
-- also, TABS MUST be converted to SPACES before going here
-- this means, in an editor - we'll have source text and then decorated thingy split into lines etc - not very space efficient?
-- can we treat it differently? E.g., have text split into lines and then STYLE DEMARKATIONS if they need to be applied at a certain place?
-- This way, we can have a whole file as Vector Text (to be able to index) and style with index: line num, char num
type DecoratedTextLine = [DecoratedText]

-- now mixing source data and representation - this is not good and needs to be separated, but that's for later
data TextBox = TextBox 
    {
        topLeft :: V2 Int -- top left corner coordinates relative to the parent
      , dimensions :: V2 Int -- width and height
      , textLines :: [Text] -- text we are editing
      , cursorPosition :: V2 Int -- line number, char number
    }