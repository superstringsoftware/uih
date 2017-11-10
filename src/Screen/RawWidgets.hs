{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ExistentialQuantification, StandaloneDeriving #-}
module Screen.RawWidgets where

import Color
import Linear
import Data.Text

import Foreign.C.Types (CInt)

import Control.Monad
import Control.Applicative

import Data.Monoid

-- The logic below is - put all Widgets in the Tree, render them. Very easy.
-- Dancing with types is not as obvious though...

-- type class that defines an interface to our widgets -
-- we will implement its' functions with different bindings - Terminal, SDL, OpenGL etc
class Show a => Widget a where
    render :: a -> IO ()

-- type holding all widget types to process child / parent etc stuff
-- "a" - is widget type, Int is the index of the widget to id it, Text - either name or textual id, need to think
-- id is needed for re-rendering etc
data MakeWidget a = MakeWidget a !Int Text deriving Show

-- Some useful instance declarations
instance Functor MakeWidget where
  -- fmap :: (a -> b) -> f a -> f b
  -- should we increment i??? --> no, b/c then we'll break /fmap id = id/ law
  fmap f (MakeWidget w i t) = MakeWidget (f w) i t

instance Applicative MakeWidget where
  -- pure :: a -> f a
  pure w = MakeWidget w 0 ""
  -- (<*>) :: f (a -> b) -> f a -> f b
  (MakeWidget f _ _) <*> (MakeWidget w i t) = MakeWidget (f w) i t

instance Monad MakeWidget where
  -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
  (MakeWidget w i t) >>= f = f w

-- Tree of Widgets that holds our UI, with RootWidget being a dummy node
data WidgetTree = forall a. Widget a => WT (MakeWidget a) [WidgetTree] | RootWidget
deriving instance Show WidgetTree -- need this because doesn't work inline for some reason

instance Monoid WidgetTree where
  mempty = RootWidget
  mappend RootWidget x = x
  mappend x RootWidget = x
  mappend (WT w xs)  x = WT w (x:xs)

-- it's mappend
{-
addChildUI :: WidgetTree -> WidgetTree -> WidgetTree
addChildUI RootWidget x = x
addChildUI (WT w xs)  x = WT w (x:xs)
-}

data WidgetState = WidgetState {
    currentIndex :: !Int -- last index of the widget
  , uiTree       :: WidgetTree -- all widgets in the current ui
  } deriving Show

-- line styles used in borders - "none" will be Nothing
data LineStyle = Solid | Dotted | Dashed deriving (Show, Eq)

data FontRep = FontRep {
    fontName :: Text,
    fontSize :: !Int
} deriving (Show, Eq)

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

-- helper function to calculate rendering
convertBorderWidth :: Maybe Border -> CInt
convertBorderWidth Nothing = 0
convertBorderWidth (Just b) = fromIntegral $ width (b :: Border)

-- box without borders and other fancy stuff for quickly putting stuff on screen
data Box = Box {
    globalX :: !Int,
    globalY :: !Int, -- top-left corner coordinates relative to the root window
    parentX :: !Int,
    parentY :: !Int, -- top-left corner coordinates relative to the parent
    width :: !Int,   -- computed width, ALWAYS in pixels
    height :: !Int,  -- computed height, ALWAYS in pixels
    color :: RGBA
} deriving (Show, Eq)

-- fancier box with shadow, borders and padding options
data Panel = Panel {
    box :: Box,
    shadow :: Maybe Shadow,
    borderTop :: Maybe Border,
    borderRight :: Maybe Border,
    borderBottom :: Maybe Border,
    borderLeft :: Maybe Border,
    padding :: V4 Int -- internal padding for the elements
} deriving (Show, Eq)

-- basic text label
data TextLabel = TextLabel {
    textBox :: Box,
    text :: Text,
    fontSize :: !Int
} deriving (Show, Eq)

-- 'fancy' text label - using Panel instead of plain Box
-- can be used for Buttons or decorated text etc
data FancyTextLabel = FancyTextLabel {
    textPanel :: Panel,
    text :: String,
    fontSize :: !Int
} deriving (Show, Eq)
