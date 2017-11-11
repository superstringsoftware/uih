{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ExistentialQuantification,
StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, RankNTypes, AllowAmbiguousTypes #-}
module Screen.LowLevelWidgets where

import Color
import Linear
import Data.Text
import Data.Monoid
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

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

-- very important typeclass that handles all rendering via state monad
-- s is a State type in the state monad we are using
class Show a => Widget a s where
    renderAt :: Int -> Int -> a -> StateT s IO ()
    render   :: a -> StateT s IO ()
    render x = renderAt 0 0 x

-- to render a widget we need to renderAt with coordinates set by bounding box
instance Widget a s => Widget (MakeWidget a) s where
  renderAt x y w = renderAt (x + globalX (box w)) (y + globalY (box w)) (widget w)

-- making WidgetTree an instance of Widget so that we can render it properly
instance Widget (WidgetTree s) s where
  -- renderAt _ _ RootWidget = return ()
  renderAt x y (WT w xs) = renderAt x y w >> mapM_ (renderAt x y) xs


-- Tree of Widgets that holds our UI, with RootWidget being a dummy node
-- paramtetrized by s - which is a State type in the State monad above
-- Becomes complicated I know
data WidgetTree s = forall a. Widget a s => WT (MakeWidget a) [(WidgetTree s)] | RootWidget
deriving instance Show (WidgetTree s) -- need this because doesn't work inline for some reason

injectWidget :: Widget a s => MakeWidget a -> WidgetTree s
injectWidget w = WT w []

instance Monoid (WidgetTree s) where
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
