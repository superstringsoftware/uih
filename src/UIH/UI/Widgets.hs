{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , DataKinds
    , TypeApplications
    , FlexibleContexts
    , ExistentialQuantification
     #-}

module UIH.UI.Widgets where

-- rendering-independent widgets
import Color
import Data.Text

import GHC.Records

-- polymorphic boxing for any kind of widgets supporting Renderable interface in a monad m
data Monad m => PolyWidget m = forall a. Renderable m a => PolyWidget a (Text -> a -> PolyWidget m)

updateButtonText :: (Monad m, Renderable m Button) => Text -> Button -> PolyWidget m
updateButtonText txt btn = PolyWidget (btn { text = txt } :: Button ) updateButtonText

data Collider = CollRect {
        x,y,w,h :: !Int
    } | CollCircle {
        x,y,r :: !Int
    } deriving (Show, Eq)

isInCollider :: Int -> Int -> Collider -> Bool
isInCollider x y (CollRect a b w h) = 
    if ( (x>a) && (x < a + w) && (y>b) && (y<b+h) ) then True else False
isInCollider x y (CollCircle a b r) = 
    if (x-a)^2 + (y-b)^2 < r^2 then True else False

isInWidget :: Monad m => Int -> Int -> PolyWidget m -> Bool
isInWidget x y (PolyWidget a _) = isInCollider x y (getCollider a)

data Box = Box {
    coll :: Collider,
    color :: Color
} deriving (Show, Eq)

data TextLabel = TextLabel {
    coll :: Collider,
    color :: Color,
    fontName :: Text,
    text :: Text
} deriving (Show, Eq)

data Button = Button {
    coll :: Collider,
    bgColor :: Color,
    -- text label position relative to the Collider
    txtX, txtY :: !Int,
    fontColor :: Color,
    fontName :: Text,
    text :: Text
} deriving (Show, Eq)

testButton = Button {
    coll = CollRect 100 100 100 50,
    bgColor = (rgbaToV4Color $ mdGrey 700),
    txtX = 20, txtY = 8,
    fontColor = (rgbaToV4Color $ mdWhite),
    fontName = "whatevs",
    text = "I'm a Button Clipping!"
}

class HasField "coll" a Collider => HasCollider a where
    -- polymorphic getCollider function
    getCollider ::  a -> Collider
    getCollider r = getField @"coll" r

instance HasCollider Button
instance HasCollider Box
instance HasCollider TextLabel

class (Monad m, HasCollider a) => Renderable m a where
    -- what we return from *intermediary* rendering function, e.g. Texture in SDL
    type Res m a
    -- Intermediate render function, returning Res
    render :: a -> m (Res m a)
    -- Final render function, drawing to screen and returning whatever pure data we were rendering, updated if needed
    renderScreen :: a -> m Collider
    
    