{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.Reactive.ReactiveWidgets where

-- redefinition of event loop using our reactive engine

import SDL as SDL hiding (get)
import SDL.Font
import Data.Text hiding (any)
import Data.Word

import Color

-- import UI.PicoUI.Raw.Widgets

import Control.Monad.Trans.State.Strict (gets)
import Control.Monad.IO.Class (liftIO, MonadIO)

import UI.PicoUI.Raw.Events as P
import UI.PicoUI.PicoUIMonad as Pico
import UI.PicoUI.Middle.AbstractWidgets
import UI.PicoUI.Middle.PureHandlers

import UI.PicoUI.Reactive.Internal.StatefulSignals

type ReactiveWidget = StatefulSignal SDLIO AbstractWidget

data EventfulWidget = EventfulWidget {
    widget :: AbstractWidget,
    hovering :: Bool,
    isFocus :: Bool
}


-- Widget is: different fmaps defining Signal of Widget->Widget functions, and then accum on the initial value of the widget


-- Signal m Widget
-- Signal m Event
-- fmapM isHovering: (Event -> (Widget->Bool)) -> Signal Event -> Signal (Widget->Bool)
-- filterApply: Signal (Widget->Bool) -> Signal Widget -> Signal Widget
-- returns a signal that only lets click events on the given widget through!
-- Ok, we need a better way to do it. !!!
-- This works, but signal contains a widget, and we probably want an event
onClick :: ReactiveWidget -> SDLIO ReactiveWidget
onClick w = do
    events <- clickEvents <$> gets eventSources
    -- let logE e = liftIO $ putStrLn $ "Click Event: " ++ show e
    -- sink events logE
    ret <- fmapM isHovering events >>= \s -> filterApplyE s w
    -- ret <- fmapM isHoveringW w >>= \s -> filterApplyE s events
    return ret

-- give 2 functions to apply to widget in case event was hovering or not and it will pick which one to use
-- can be used to create widgets like:
-- w <- unionsM [ filterHoverApply (setText "Hovering!") (setText "NOT Hovering :(") <^$> eHover,
--                filterHoverApply (setText "CLICKED!") (setText "NOT Clicked :(") <^$> eClick
--              ] >>= accum testLabel3
filterHoverApply :: (Widget -> Widget) -> (Widget -> Widget) -> P.Event -> Widget -> Widget
filterHoverApply ftrue ffalse e w = 
    if isHovering e w then ftrue w else ffalse w 

isHovering :: P.Event -> Widget -> Bool
isHovering e w = let (V2 x y) = pos $ source e
                 in  if isInWidget x y w then True else False

isHoveringW :: Widget -> P.Event -> Bool
isHoveringW w e = isHovering e w

isHoveringE :: P.Event -> Widget -> Maybe (P.Event)
isHoveringE e w = let (V2 x y) = pos $ source e
                 in  if isInWidget x y w then Just e else Nothing

-- returns a signal with click events for a specific widget
onClickE :: ReactiveWidget -> SDLIO (StatefulSignal SDLIO (Maybe P.Event))
onClickE w = do    
    events <- clickEvents <$> gets eventSources
    ret <- fmapM isHoveringE events >>= \f -> applyE f w >>= filterJust
    return ret      
    
-- add a listener for on click events    
onClickAct :: ReactiveWidget -> (P.Event -> SDLIO ()) -> SDLIO ()
onClickAct w act = do
    sig <- onClickE w
    let conn (Just e) = act e
    (addListener sig) conn


{-
Overall logic for the widgets should be:

- tag incoming SDL events with (widget -> widget) pure functions depending on the event type
- combine them into one source
- accumulate

So it will be something like

rWidget <- unionsM [  (changeBackground red) <^$ ehover
                        , (changeBackground white) <^$ eStopHover
                        , ...
                       ] >>= accum initialWidget

-- Ok, one scenario to work through the flow:

- sdlSource fires "Click" event
- a widget reacts and changes the background
- but ALSO sends a "Click" event with itself as a source -- how?
fmapM of some sort?

clickSignal <- sendClick <^$> (filterS isClick sdlSource)

filterS isClick sdlSource --> sig1, with event inside
now need to filter it with *current* widget coordinates. So need a combination function that takes a value from the event,
current value of the signal, and combines them somehow, like:

let conn = (\x -> (modifyVal ret) ((f x)) )
(addListener sig) conn

-}
