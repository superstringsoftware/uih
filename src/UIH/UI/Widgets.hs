{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies #-}
module UIH.UI.Widgets where

-- rendering-independent widgets
import Color
import Data.Text

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

class Monad m => Renderable m a where
    -- what we return from *intermediary* rendering function, e.g. Texture in SDL
    type Res m a
    -- Intermediate render function, returning Res
    render :: a -> m (Res m a)
    -- Final render function, drawing to screen and returning whatever pure data we were rendering, updated if needed
    renderScreen :: a -> m a
    