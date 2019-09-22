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
module UIH.UI.ManagerMonad where

-- this is a (pure?) state monad that keeps the current UI state 
-- It is connected to SDL low level events etc via SDL2.SDLUI monad, 
-- which handles SDL events transformation etc
-- This one is a handy abstraction that potentially allows other rendering engines to be used.

import UIH.UI.Widgets

import Data.Map.Strict as Map 

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)

import Control.Monad

import Data.Text as T

data EventTypes = EvHover 
    | EvClick
    | EventGeneric
    deriving (Eq, Show)

data Event = Event {
    eventType :: EventTypes,
    source :: (Int, BasicWidget)
}

-- Polymorphic UI state in the monad m
data UIState m = UIState {
    -- incrementing ids for UI elements
    idCounter :: !Int, 
    -- polymorphic map from Ints (ids) to Renderables
    -- Eventually we want to track colliders separately, since not every widget will be an event source
    widgets :: Map.Map Int BasicWidget,
    -- event handlers for widget with id = key
    handlers :: Map.Map Int [EventHandler m],
    -- certain state in terms of current focus / hover etc widgets -- 
    -- needed to handle text events etc
    currentHoverId :: Maybe Int,
    currentFocusId :: Maybe Int, -- widget that has focus, used for text editing mostly
    editingText :: Text -- text currently being edited
}

-- Event handlers are actions from Event 
data EventHandler m = EventHandler {
    runHandler :: Event -> ManagerMonadT m ()
}

initUIState = UIState {
    idCounter = 0,
    widgets = Map.empty,
    handlers = Map.empty,
    currentHoverId = Nothing,
    currentFocusId = Nothing,
    editingText = ""
}

setCurrentFocusId i = modify' (\s -> s { currentFocusId = i }) 

-- process backspace if we are editing currently
backspaceEditingText :: Monad m => ManagerMonadT m ()
backspaceEditingText = do
    txt <- gets editingText
    if txt /= "" then do
        let txt' = T.init txt
        alterTextWidget txt'
        modify' \s -> s { editingText = txt' }
    else pure ()

addEditingText :: Monad m => Text -> ManagerMonadT m ()
addEditingText text = do
    txt <- gets editingText
    let txt' = txt <> text
    alterTextWidget txt'
    modify' \s -> s { editingText = txt' }


-- Set a textual widget to a new text
alterTextWidget :: Monad m => Text -> ManagerMonadT m ()
alterTextWidget txt = 
    getFocusWidget >>=
    maybe (return ()) 
          (\(i,widg) -> 
            case widg of
                Box{..} -> return ()
                w -> do
                    let wp' = w { text = txt }
                    ws <- gets widgets
                    let ws' = Map.insert i wp' ws
                    modify' (\s -> s { widgets = ws' } )
                    return ())
    
                

-- returns a pair of focus widget with its index
getFocusWidget :: Monad m => ManagerMonadT m (Maybe (Int, BasicWidget))
getFocusWidget = do
    im <- gets currentFocusId
    ws <- gets widgets
    return $ maybe Nothing (\i -> maybe Nothing (\w -> Just (i,w)) (Map.lookup i ws)) im

type ManagerMonadT m = StateT (UIState m) m

-- adding a new widget to UIState
-- returns ID of newly added widget
registerWidget :: Monad m => BasicWidget -> ManagerMonadT m Int
registerWidget w = do
    s <- get
    ws <- widgets <$> get
    idc <- idCounter <$> get
    let idc' = idc + 1
    let ws' = Map.insert idc' w ws
    put $ s {idCounter = idc', widgets = ws'}
    return idc'

-- add event handler to handlers list at key i
addHandler :: Monad m => EventHandler m -> Int -> ManagerMonadT m ()
addHandler h i = do
    s <- get
    let hs = handlers s
    let hs' = Map.alter fn i hs
    put s { handlers = hs' }
    where fn Nothing = Just [h]
          fn (Just chs) = Just (h:chs)

-- adds new widget and a handler          
addWidgetWithHandler :: Monad m => BasicWidget -> EventHandler m -> ManagerMonadT m Int
addWidgetWithHandler w h = registerWidget w >>= \i -> addHandler h i >> return i

-- firing event to all registered handlers
fireEvent :: Monad m => Event -> ManagerMonadT m ()
fireEvent ev = do
    let (i, pw) = source ev -- getting index and widget of the event
    res <- Map.lookup <$> pure i <*> (handlers <$> get) -- getting list of handlers if any
    maybe (return ()) -- not found, no handlers at index i
          (\hsl -> mapM_ (\h -> runHandler h ev) hsl ) -- running handlers
          res
          
    
-- given x,y coordinates finds a widget that contains them and returns it (if any)
getEventSource :: Monad m => Int -> Int -> ManagerMonadT m (Maybe (Int, BasicWidget))
getEventSource x y = do
    ws <- widgets <$> get
    let res = Map.assocs $ Map.filter (isInWidget x y) ws
    case res of
        [] -> return Nothing
        (e:xs) -> return $ Just e -- returning first collider that catches the event, no propagation or anything, that's TBD
    