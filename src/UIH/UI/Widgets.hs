{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , DataKinds
    , TypeApplications
    , FlexibleContexts
    , ExistentialQuantification
    , OverloadedLists
     #-}

{-
This whole ordeal shows how ANNOYING haskell's type system is at times.
If only we could extend SumTypes with having generic collections defined with the parent subtype,
the issue of adding new widgets would cease to exist.

As things are, we have to do this crazy type-fu with existentials, multiparameter typeclass families and such,
while still having to compromise on flexibility.

We either have to have a pre-defined set of widgets, put them all in one sum type and then have full access to update
functions etc, but then it becomes impossible for users to create their own widgets.

Alternative is this polymorphic existential type, but then we are facing big issues with updating the state with
flexible data, in essence moving the problem from one type to another type - update events in this case.

I don't think the solution exists in Haskell.

Probably the best compromise is very well designed low-level small set of basic widgets, where larger widgets are
constructed from this set.

Exploring this idea further here. 
-}
module UIH.UI.Widgets where

-- rendering-independent widgets
import Color
import Data.Text
import Data.Sequence

import GHC.Records

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

isInWidget x y w = isInCollider x y (getCollider w)

data BasicWidget = Box {
    coll :: Collider,
    color :: Color
} | TextLabel {
    coll :: Collider,
    color :: Color,
    fontName :: Text,
    text :: Text
} | Button {
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
    
instance HasCollider BasicWidget

class (Monad m, HasCollider a) => Renderable m a where
    -- what we return from *intermediary* rendering function, e.g. Texture in SDL
    type Res m a
    -- Intermediate render function, returning Res
    render :: a -> m (Res m a)
    -- Final render function, drawing to screen and returning whatever pure data we were rendering, updated if needed
    renderScreen :: a -> m ()
    
    