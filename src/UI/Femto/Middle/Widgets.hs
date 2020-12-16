{-# LANGUAGE OverloadedStrings, DuplicateRecordFields
  , RecordWildCards
  , MultiParamTypeClasses
  , TypeFamilies
   #-}

module UI.Femto.Middle.Widgets where

import Data.Text
import Color

import Linear
import Foreign.C.Types (CInt)

import UI.Femto.Middle.Events

data TextStyle = TextStyle {
    fontName  :: Text
  , fontSize  :: !Int
  , fontColor :: Color
} deriving (Show, Eq)

data VAlign = VAlignTop  | VAlignMiddle | VAlignBottom deriving (Show, Eq)
data HAlign = HAlignLeft | HAlignMiddle | HAlignRight  deriving (Show, Eq)

-- types of backgrounds - color or image, maybe add gradient
data Background = BGColor Color | BGImage Text deriving (Show, Eq)

-- checks if (x,y) is inside the rectangle (x',y',w,h)
isInsideRectangle :: V2 CInt -> V2 CInt -> V2 CInt -> Bool
isInsideRectangle (V2 x y) (V2 x' y') (V2 w h) = (x >= x') && (x <= (x' + w)) && (y >= y') && (y <= (y'+h))

isInsideWSkeleton :: V2 CInt -> WidgetSkeleton -> Bool
isInsideWSkeleton point WidgetSkeleton{..} = isInsideRectangle point pos size

isInsideWidget :: V2 CInt -> Widget -> Bool
isInsideWidget point Widget{..} = if element == WENone then False else isInsideWSkeleton point (wsk element) 

-- Used to keep track of the info common to all widgets - sizes, states etc
data WidgetSkeleton = WidgetSkeleton {
    wid :: Text
  , pos  :: V2 CInt
  , size :: V2 CInt
  , isInFocus :: Bool
  , isHovering :: Bool
} deriving (Show, Eq)


data WidgetElement = 
  WEBox {
      wsk :: WidgetSkeleton
    , background :: Background
  } |
  WETextLabel {
      wsk :: WidgetSkeleton
    , text :: Text
    , textAlign :: (VAlign, HAlign)
    , textStyle :: Maybe TextStyle
  } |
  WENone
  deriving (Show, Eq)

-- Tree for our widgets
data Widget = Widget {
    element  :: WidgetElement
  , children :: [Widget]
}

emptyWidget = Widget WENone []

-- then, example of complex widgets:

mkButton eb@WEBox{..} etl@WETextLabel{} = Widget {
    element  = eb 
  , children = [Widget {element = etl, children=[]}]
}

-- type ComponentData s p = (s,p) -- state and props

-- Other approach: tree of monadic actions
data RenderTree m = RenderTree {
    rtElement :: m WidgetElement,
    rtChildren :: [m (RenderTree m)],
    eventHandlers :: [Event -> m ()]
}

-- Basic Combinators:

