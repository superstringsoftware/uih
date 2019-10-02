{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.Reactive.ReactiveWidgets where

-- redefinition of event loop using our reactive engine

import SDL as SDL hiding (get)
import SDL.Font
import Data.Text as T hiding (any)
import Data.Word

import Color

-- import UI.PicoUI.Raw.Widgets

import Control.Monad.Trans.State.Strict (gets)
import Control.Monad.IO.Class (liftIO, MonadIO)

import UI.PicoUI.Raw.Events as P
import UI.PicoUI.Raw.WidgetCompiler as P
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
-- Compilation to low-level widgets should be handled via Signals as well - we just create an
-- fmapM-ed signal from a Widget with a compile function, then we will recompile at each change automatically.
-- Then, rendering can be done via events as well - have a "Render" event source, subscribe
-- render functions with all low-level widgets (signals) to it, and send "Render" events at needed intervals.

-- The only (possible) drawback - need to be careful to run the network in 1 thread, but that was the initial design anyway.

-- For now, we are simply adding raw widget signals to the map in the monad and running render on them periodically
registerReactiveWidget :: StatefulSignal SDLIO Widget -> SDLIO ()
registerReactiveWidget w = do
    rawW <- fmapMM (P.compile2Widget False) w
    insertRawWidget rawW 

-- isBackspace (alterText (\txt -> if txt == "" then txt else init txt))  
-- alterText :: (Text -> Text) -> AbstractWidget -> AbstractWidget  
-- filterS :: MonadIO m => (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
-- filterApplyE :: MonadIO m => StatefulSignal m (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
-- reactiveBackspace :: P.Event -> 
reactiveBackspace e = do
    s1 <- filterS isBackspace e -- Signal Event, need to get to Signal (Widget -> Widget) -- how?
    ret <- createStatefulSignal (alterText (\txt -> if txt == "" then txt else T.init txt))
    let conn ev = (modifyVal ret) $ const (alterText (\txt -> if txt == "" then txt else T.init txt))
    (addListener s1) conn
    return ret

reactiveAppend sig = do
    e <- readVal sig
    ret <- createStatefulSignal (_txtAppend e)
    let conn ev = (modifyVal ret) $ const (_txtAppend ev)
    (addListener sig) conn
    return ret


_txtAppend evt w = case evt of
                        ETextInput{..} -> alterText (\txt0 -> txt0 <> txt) w
                        _              -> w
                

-- Signal m Widget
-- Signal m Event
-- fmapM isHovering: (Event -> (Widget->Bool)) -> Signal Event -> Signal (Widget->Bool)
-- filterApply: Signal (Widget->Bool) -> Signal Widget -> Signal Widget
-- returns a signal that only lets click events on the given widget through!
-- Ok, we need a better way to do it. !!!
-- This works, but signal contains a widget, and we probably want an event
-- see below for that.
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



