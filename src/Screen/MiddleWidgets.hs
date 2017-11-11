{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Screen.MiddleWidgets where

import Color
import Linear
import Data.Text
import Data.Monoid
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

data Widget = Panel {
    x :: !Int,
    y :: !Int, -- top-left corner coordinates relative to the parent
    width :: !Int,   -- computed width, ALWAYS in pixels
    height :: !Int,  -- computed height, ALWAYS in pixels
    shadow :: Maybe Shadow,
    borders :: [Border],
    color :: RGBA
  } | TextLabel {
    x :: !Int
  , y :: !Int -- top-left corner coordinates relative to the parent
  , text  :: Text
  , font  :: Text
  , size  :: !Int
  , color :: RGBA
  } | Image {
    x :: !Int
  , y :: !Int -- top-left corner coordinates relative to the parent
  , altText  :: Text
  , filePath :: Text
  } | Canvas {
    name :: Text
  } | Complex Widget [Widget] -- represents a composite widget: Panel (should always root!! - use GADT?) with children elements
    | EmptyWidget
    deriving Show

-- helper function to get dimensions of the Widget (needed for easier rendering if it's a complex Widget)
xOfWidget (Complex w _) = x w
xOfWidget w = x w
yOfWidget (Complex w _) = y w
yOfWidget w = y w

{-
instance Show Widget where
  show p@(Panel {})  = "Panel (" ++ show (x p) ++ ", " ++ show (y p) ++")"
  show p@(TextLabel {})  = show (text p) ++ " (" ++ show (x p) ++ ", " ++ show (y p) ++")"
  show (Complex w xs) = show w ++ " ---> " ++ show xs ++ " | "
-}

-- some convenience initialization functions
emptyPanel = Panel 0 0 0 0 Nothing [] mdBlack
emptyTextLabel = TextLabel 0 0 "" "__DEFAULT__" 12 mdBlack

basicInfoBox x y w h txt = Complex (emptyPanel { x = x, y = y, width = w, height = h, color = mdGrey 500 }) [emptyTextLabel {text = txt}]
basicButton  x y w h txt = Complex (emptyPanel
                        { x = x, y = y, width = w, height = h, color = mdGrey 700,
                          borders = [solidBorder, solidBorder, solidBorder, solidBorder] })
                          [emptyTextLabel {text = txt, x = 6, y = 6, color = mdRed 11}]

-- It's not really a monoid!!!! - associativity doesnt work properly, needs fixing
instance Monoid Widget where
  mempty = EmptyWidget
  mappend EmptyWidget x = x
  mappend x EmptyWidget = x
  mappend (Complex w ws) nw = Complex w (ws ++ [nw]) -- if already complex on the left, simply adding to list
  mappend w nw = Complex w [nw] -- otherwise, simply building Complex from an element


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

solidBorder = Border 1 mdBlack Solid
