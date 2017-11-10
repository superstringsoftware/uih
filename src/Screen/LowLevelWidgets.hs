{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ExistentialQuantification, StandaloneDeriving #-}
module Screen.LowLevelWidgets where

import Color
import Linear
import Data.Text
import Data.Monoid

-- box without borders and other fancy stuff for quickly putting stuff on screen
data Box = Box {
    globalX :: !Int,
    globalY :: !Int, -- top-left corner coordinates relative to the root window
    parentX :: !Int,
    parentY :: !Int, -- top-left corner coordinates relative to the parent
    width :: !Int,   -- computed width, ALWAYS in pixels
    height :: !Int  -- computed height, ALWAYS in pixels
} deriving (Show, Eq)

data MakeWidget a = MkWidget {
    box       :: Box -- bounding box
  , widget    :: a   -- whatever we are rendering inside
  } deriving Show

class Show a => Widget a where
    render :: a -> IO ()

-- Tree of Widgets that holds our UI, with RootWidget being a dummy node
data WidgetTree = forall a. Widget a => WT (MakeWidget a) [WidgetTree] | RootWidget
deriving instance Show WidgetTree -- need this because doesn't work inline for some reason

-- making WidgetTree an instance of Widget so that we can render it properly
instance Widget WidgetTree where
  render RootWidget = return ()
  render (WT w xs) = render (widget w) >> mapM_ render xs

injectWidget :: forall a. Widget a => MakeWidget a -> WidgetTree
injectWidget w = WT w []

instance Monoid WidgetTree where
  mempty = RootWidget
  mappend RootWidget x = x
  mappend x RootWidget = x
  mappend (WT w xs)  x = WT w (x:xs)

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

-- fancier box with shadow, borders and padding options
data Panel = Panel {
    shadow :: Maybe Shadow,
    borderTop :: Maybe Border,
    borderRight :: Maybe Border,
    borderBottom :: Maybe Border,
    borderLeft :: Maybe Border,
    padding :: V4 Int, -- internal padding for the elements
    color :: RGBA
} deriving (Show, Eq)

-- very basic text label without and borders, background color or boundary checks
type TextLabelWidget = MakeWidget Text
-- fancy box with decorations
type PanelWidget = MakeWidget Panel
-- button
type ButtonWidget = MakeWidget (Panel, Text)
