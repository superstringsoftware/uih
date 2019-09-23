{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , DataKinds
    , TypeApplications
    , FlexibleContexts
    , RankNTypes
    , BlockArguments
     #-}

module UIH.UI.AbstractWidgets where

-- high-level, rendering independent widgets description

-- import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text as T hiding (map)
import Linear
import Color

import UIH.UI.SimpleTree

-- what we use as index into widgets - needs to be the same for ManagerMonad and SDLIO
type WidgetId = Int

data FontStyle = Normal | Bold | Italic
data TextAlign = CenterAlign | LeftAlign | RightAlign
data FontData = FontData {
    fontName :: !Text,
    fontSize :: !Int,
    fontStyle :: FontStyle,
    fontColor :: Color
} | FontDataDefault

data Background = 
    BGColor Color
    | BGImage Text -- will be changed
    | BGGradient Text -- will be chnaged
    | BGTransparent

-- composite widget is a tree of AbstractWidgets
-- I'd discourage nesting too deep?
-- In this tree, all children layouts are relative to the parent
-- need a polymorphic layout recalculation function that handles automatic layouts!!!
-- (V4 Int) here is a rectangle where dimensions have been converted to absolute coordinates relative to the parent
-- or Screen, if there's no parent, following the convention of layoutToRectangle function, thus:
-- V4 x y w h
-- This calculation needs to take place AT THE START of the program, with all RESIZE events,
-- and with all ADD / DELETE widget events.

-- this is used as initial tree to setup the interface by converting into flat + separate event handlers
-- event handlers are added AUTOMATICALLY when we convert Widget to Manager Monad representation
-- data Widget = SimpleWidget AbstractWidget | CompositeWidget (MultiTree AbstractWidget)
-- starting with FLAT STRUCTURE OF ABSTRACT WIDGETS!!!!!
type Widget = AbstractWidget

type AbstractWidgetTransformer = (AbstractWidget -> AbstractWidget)

-- some pure handler helpers to manipulate abstract widgets
-- can be turned into handlers eventually easy enough
hndlBackspace :: AbstractWidget -> (AbstractWidget, Bool)
hndlBackspace w = if (text w) /= "" then (w { text = T.init (text w) }, True) else (w, False)

hndlAppendText :: Text -> AbstractWidget -> AbstractWidget
hndlAppendText txt w = w { text = (text w) <> txt }

hndlPrependText :: Text -> AbstractWidget -> AbstractWidget
hndlPrependText txt w = w { text = txt <> (text w) }

-- helper function; calculates dimensions of all children *relative to the parent* 
-- so for top level widgets will be relative to the screen
calculateCacheRect :: Int -> Int -> Widget -> Widget
calculateCacheRect w h widg = widg { cacheRect = layoutToRectangle (layout widg) (V4 0 0 w h) }

isInWidget x y widg = 
    let (V4 a b w h) = cacheRect widg
    in  if ( (x>a) && (x < a + w) && (y>b) && (y<b+h) ) then True else False

{-
-- ok not sure if recursion terminates here... should be with empty list, right?
calculateDimensions :: CompositeWidget -> CompositeWidget
calculateDimensions (CompositeWidget parent rect@(V4 _ _ w h) children) = 
    CompositeWidget parent rect (map fn children) where
        fn (CompositeWidget widget crect cchildren) = 
            CompositeWidget widget 
                            (calculateDimsRelToParent w h widget)
                            (map calculateDimensions cchildren)
-}

-- SCREEN is used for top level, root definition of the UI tree.
data AbstractWidget = 
    -- Text label that CANNOT be edited
    Label {
        fontData :: FontData,
        text :: Text,
        valign, halign :: TextAlign,
        layout :: Layout,
        background :: Background, 
        cacheRect :: V4 Int
    } |
    -- Text label that CAN be edited - we are separating the 2 b/c this one will need event handlers 
    -- in the implmentation level
    InputText {
        fontData :: FontData,
        text :: Text,
        valign, halign :: TextAlign,
        layout :: Layout,
        background :: Background,
        cacheRect :: V4 Int
    } |
    Panel {
        layout :: Layout,
        background :: Background,
        cacheRect :: V4 Int
    } |
    SCREEN -- used for setting up CompositeWidget tree 

-- different layout options and the rectangle conventions corresponding to them
{-
Absolute -- x y w h (same as StickT, StickL)
    -- stick to top: x (delta to top) w h
    | StickT 
    -- stick to bottom: x h w (delta to bottom)
    | StickB
    -- stick to left: (delta to left) y w h
    | StickL 
    -- stick to right: w y (delta to right) h
    | StickR 
-}
data Layout = 
    -- EXPLICIT:
    TopLeft (V4 Int)          -- x y w h (same as StickT, so top only)
    | TopRight (V4 Int)       -- (delta right) y w h (same as StickR, so right only)
    | BotLeft (V4 Int)        -- x (delta to bottom) w h
    | BotRight (V4 Int)       -- (delta right) (delta bottom) w h
    | StretchAll (V4 Int)     -- (delta left) (delta top) (delta right) (delta bottom)
    | StretchH_Top (V4 Int)   -- (delta left) (delta top) (delta right) h
    | StretchH_Bot (V4 Int)   -- (delta left) (delta bottom) (delta right) h
    | StretchV_Left (V4 Int)  -- (delta left) (delta top) w (delta bottom)
    | StretchV_Right (V4 Int) -- (delta right) (delta top) w (delta bottom)
    deriving (Show, Eq)

-- helper functions to create layouts so that conventions do not mess up peoples brains
-- the only help is mnemonics - dr is delta right etc
l_TL  x y w h = TopLeft  $ V4 x y w h
l_TR dr y w h = TopRight $ V4 dr y w h
l_BL x db w h = BotLeft  $ V4 x db w h
l_BR dr db w h = BotRight $ V4 dr db w h
l_SA dl dt dr db = StretchAll $ V4 dl dt dr db
l_SHT dl dt dr h = StretchH_Top $ V4 dl dt dr h
l_SHB dl db dr h = StretchH_Bot $ V4 dl db dr h
l_SVL dl dt w db = StretchV_Left $ V4 dl dt w db
l_SVR dr dt w db = StretchV_Right $ V4 dr dt w db

-- default layout - centering
defaultCenterLayout = l_SA 8 4 8 4

-- converts layout to absolute coordinates given an enclosing rectangle coords 
-- Rectangles are (x,y,w,h) 
-- The idea is - we setup hierarchy of widgets with given layouts, then there's a function pass
-- over the whole UI that recalculates everything into screen coords
layoutToRectangle :: Layout -> V4 Int -> V4 Int
layoutToRectangle (TopLeft  (V4 l t r b)) (V4 x y w h)  = V4 (x+l)     (y+t) r b
layoutToRectangle (TopRight (V4 l t r b)) (V4 x y w h)  = V4 (x+w-l-r) (y+t) r b
layoutToRectangle (BotLeft  (V4 l t r b)) (V4 x y w h)  = V4 (x+l)     (y+h-t-b) r b
layoutToRectangle (BotRight (V4 l t r b)) (V4 x y w h)  = V4 (x+w-l-r) (y+h-t-b) r b

layoutToRectangle (StretchAll     (V4 l t r b)) (V4 x y w h)  = V4 (x+l) (y+t) (w-l-r) (h-t-b)
layoutToRectangle (StretchH_Top   (V4 l t r b)) (V4 x y w h)  = V4 (x+l) (y+t) (w-l-r) b
layoutToRectangle (StretchH_Bot   (V4 l t r b)) (V4 x y w h)  = V4 (x+l) (y+h-t-b) (w-l-r) b
layoutToRectangle (StretchV_Left  (V4 l t r b)) (V4 x y w h)  = V4 (x+l) (y+t) r       (h-t-b)
layoutToRectangle (StretchV_Right (V4 l t r b)) (V4 x y w h)  = V4 (x+w-l-r) (y+t) r   (h-t-b)

-- ok this is crazy confusing so need to check
__lt lay res = do
    let res' = layoutToRectangle lay (V4 0 0 200 100)
    let fl =  res' == res
    if fl then putStrLn $ "[PASSED] " ++ show lay
    else putStrLn $ "[FAILED] " ++ show res' ++ " /= " ++ show res

__run__layout_tests = do
    __lt (TopLeft  $ V4 10 20 50 30) (V4 10 20 50 30)
    __lt (TopRight $ V4 10 20 50 30) (V4 140 20 50 30)
    __lt (BotLeft  $ V4 10 20 50 30) (V4 10 50 50 30)
    