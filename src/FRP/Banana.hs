{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NamedFieldPuns, 
    OverloadedLabels, RecordWildCards, ScopedTypeVariables  #-}

module FRP.Banana where

import Reactive.Banana
import qualified SDL as SDL

data Rect = Rect !Int !Int !Int !Int
data Point = Point !Int !Int

type EMousePosition = Event Point
type EMouseEnter = Event ()

mouse :: EMousePosition
mouse = never

isInRect :: Point -> Rect -> Bool
isInRect (Point x y) (Rect a b w h) = 
    if ( (x>a) && (x < a + w) && (y>b) && (y<b+h) ) then True else False

-- trying to model a very simple UI element - rectangle that checks it's own Hover event (mouse is inside)
-- and changes how it's displayed respectively

data SimpleButton = SimpleButton {
    text :: String,
    isHover :: Bool,
    rect :: Rect
}

sb :: SimpleButton
sb = SimpleButton "ok" False (Rect 0 0 100 20)

