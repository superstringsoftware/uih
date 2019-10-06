{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
    RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.Reactive.ReactiveWidgets where

-- redefinition of event loop using our reactive engine

import SDL hiding (get)
import SDL.Font
import Data.Text as T hiding (any)
import Data.Word

import Color

-- import UI.PicoUI.Raw.Widgets

import Control.Monad.Trans.State.Strict (gets, modify')
import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Monad (join)

import UI.PicoUI.Raw.Events as P
import UI.PicoUI.Raw.WidgetCompiler as P
import UI.PicoUI.PicoUIMonad as Pico hiding (calculateCacheRect)
import UI.PicoUI.Middle.AbstractWidgets
import UI.PicoUI.Middle.PureHandlers

import UI.PicoUI.Reactive.Internal.StatefulSignals

-- Widget is: different fmaps defining Signal of Widget->Widget functions, and then accum on the initial value of the widget
-- Compilation to low-level widgets should be handled via Signals as well - we just create an
-- fmapM-ed signal from a Widget with a compile function, then we will recompile at each change automatically.
-- Then, rendering can be done via events as well - have a "Render" event source, subscribe
-- render functions with all low-level widgets (signals) to it, and send "Render" events at needed intervals.

-- The only (possible) drawback - need to be careful to run the network in 1 thread, but that was the initial design anyway.

createReactiveWidget :: Widget -> SDLIO (StatefulSignal SDLIO Widget)
createReactiveWidget = createStatefulSignal

-- For now, we are simply adding raw widget signals to the map in the monad and running render on them periodically
registerReactiveWidgets ::[StatefulSignal SDLIO Widget] -> SDLIO ()
registerReactiveWidgets = mapM_ registerReactiveWidget

registerReactiveWidget :: StatefulSignal SDLIO Widget -> SDLIO ()
registerReactiveWidget w = do
    rawW <- fmapMM (\pw -> P.compile2Widget (isWidgetInFocus pw) pw) w
    insertRawWidget rawW 
    addReactiveResize w

-- make widget focusable on left click    
makeFocusable :: ReactiveWidget -> SDLIO ()
makeFocusable rw = onClickActW rw (\w e -> 
        if isLeftClick 1 e
        then addFocus w
        else pure ()
    )

-- register Signal to receive focus events and put remove listener action to then remove focus
addFocus :: ReactiveWidget -> SDLIO ()
addFocus rw = do
    doRemoveFocus -- first, remove currently focused widget
    fe <- focusEvents <$> gets eventSources
    let conn e = modifyVal rw (pureTextEdit e)
    rm <- addListenerWRemove fe conn -- adding listener and getting a remove action
    modify' (\s -> s { removeFocus = rm, focusWidget = Just rw }) -- updating state with remove action
    modifyVal rw (\w -> w { isFocus = True })

-- remove focus from the currently focused widget
doRemoveFocus :: SDLIO ()
doRemoveFocus = do 
    -- execute remove action:
    join $ gets removeFocus
    mfw <- gets focusWidget
    maybe (pure ())
          (\rw -> modifyVal rw (\w -> w { isFocus = False }) ) -- resetting focus to false
          mfw
    modify' (\s -> s { removeFocus = pure (), focusWidget = Nothing }) -- set it to nothing

-- handler for events that should only be received by widget currently in focus: mostly textedit events?    
-- but eventually we need to handle mouse clicks so that the cursor is put in the right place etc
{-
focusEventsListener :: P.Event -> SDLIO ()
focusEventsListener e = do
    mw <- gets focusWidget
    maybe (pure ())
          (\w ->  )
-}
-- needs to receive RESIZE EVENTS ONLY AS A SOURCE!!!
-- EWindowResized { source :: EventSource, size :: V2 Int }
-- calculateCacheRect :: Int -> Int -> Widget -> Widget
reactiveResize = do
    sig <- allEvents <$> gets eventSources
    ret <- createStatefulSignal id
    let conn (EWindowResized _ (V2 x y)) = modifyVal ret $ const (calculateCacheRect x y)
        conn _ = pure ()
    addListener sig conn
    return ret

addReactiveResize w = do
    fsig <- reactiveResize
    let conn f = readVal w  >>= \v -> modifyVal w (const (f v)) 
    addListener fsig conn    

-- fmapM :: MonadIO m => (Event -> (Widget -> Widget) ) -> StatefulSignal m Event -> m (StatefulSignal m (Widget -> Widget) )
-- function that handles different text editing events
pureTextEdit :: P.Event -> (Widget -> Widget)
pureTextEdit e w = case e of
    ETextInput{..} -> alterText (<> txt) w
    EBackspace{..} -> alterText (\txt -> if txt == "" then txt else T.init txt) w
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
    fmapM isHovering events >>= \s -> filterApplyE s w
    

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
                 in  isInWidget x y w

isHoveringW :: Widget -> P.Event -> Bool
isHoveringW w e = isHovering e w

isHoveringE :: P.Event -> Widget -> Maybe P.Event
isHoveringE e w = let (V2 x y) = pos $ source e
                 in  if isInWidget x y w then Just e else Nothing

-- returns a signal with click events for a specific widget
onClickE :: ReactiveWidget -> SDLIO (StatefulSignal SDLIO (Maybe P.Event))
onClickE w = do    
    events <- clickEvents <$> gets eventSources
    fmapM isHoveringE events >>= \f -> applyE f w >>= filterJust
    
    
-- add a listener for on click events    
onClickAct :: ReactiveWidget -> (P.Event -> SDLIO ()) -> SDLIO ()
onClickAct w act = do
    sig <- onClickE w
    let conn (Just e) = act e
    addListener sig conn

-- add a listener that also takes a widget as an argument    
onClickActW :: ReactiveWidget -> (ReactiveWidget -> P.Event -> SDLIO ()) -> SDLIO ()    
onClickActW w act = do
    sig <- onClickE w
    let conn (Just e) = act w e
    addListener sig conn



