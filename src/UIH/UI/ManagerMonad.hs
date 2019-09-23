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
import Control.Monad.IO.Class (liftIO, MonadIO)

import Control.Monad

import Data.Text as T

data EventTypes = EvHover 
    | EvClick
    | EventGeneric
    | EvTextInput Text
    | EvKbBackspace
    deriving (Eq, Show)

data Event = Event {
    eventType :: EventTypes,
    source :: (Int, BasicWidget)
}

-- Monad transformer for abstract interface handling, where
-- m: is the underlying UI monad that handles rendering, in our case SDLIO
-- users DON'T have to use this type directly, only if someone wishes to implement a different rendering backend
-- u: is the User state *for UI only* - this way, users can stack another monad on top
-- of this one, handle UI state here and the rest of the state in this other top monad.

-- In this monad, user state is mostly needed so that a user can define custom event handlers for their functionality.
-- Once we move to reactive might change this obviously.
type ManagerMonadT m u = StateT (UIState m u) m

-- Polymorphic UI state in the monad m
data UIState m u = UIState {
    -- incrementing ids for UI elements
    idCounter :: !Int, 
    -- polymorphic map from Ints (ids) to Renderables
    -- Eventually we want to track colliders separately, since not every widget will be an event source
    widgets :: Map.Map Int BasicWidget,
    -- event handlers for widget with id = key
    handlers :: Map.Map Int [EventHandler m u],
    -- certain state in terms of current focus / hover etc widgets -- 
    -- needed to handle text events etc
    currentHoverId :: Maybe Int,
    currentFocusId :: Maybe Int, -- widget that has focus, used for text editing mostly
    editingText :: Text, -- text currently being edited
    userState   :: Maybe u
}

setUserState :: Monad m => u -> ManagerMonadT m u ()
setUserState us = modify' (\s -> s { userState = Just us })

getUserState :: Monad m => ManagerMonadT m u (Maybe u)
getUserState = gets userState

modifyUserState' :: Monad m => (Maybe u -> Maybe u) -> ManagerMonadT m u ()
modifyUserState' f = modify' (\s-> s { userState = f $ userState s })

-- Event handlers are actions from Event to ManagerMonadT m  
data EventHandler m u = EventHandler {
    runHandler :: Event -> ManagerMonadT m u ()
}

initUIState us = UIState {
    idCounter = 0,
    widgets = Map.empty,
    handlers = Map.empty,
    currentHoverId = Nothing,
    currentFocusId = Nothing,
    editingText = "",
    userState = us
}

setCurrentFocusId i = modify' (\s -> s { currentFocusId = i }) 

modifyWidget :: MonadIO m => Int -> BasicWidget -> ManagerMonadT m u () 
modifyWidget i w = do
    ws <- gets widgets
    modify' (\s -> s { widgets = Map.insert i w ws })

-- handler that handles editing "text" field in a widget    
-- use it internally to create editable labels etc
textHandler :: MonadIO m => Event -> ManagerMonadT m u ()
textHandler ev@(Event EvKbBackspace (i,w)) = do
    liftIO $ putStrLn $ "Backspace to widget # " ++ show i
    let txt = text w
    if txt /= "" then do
        let txt' = T.init txt
        let w' = w { text = txt' }
        modifyWidget i w'
    else pure ()
textHandler ev@(Event (EvTextInput tinp) (i,w)) = do
    liftIO $ putStrLn $ "Text to widget # " ++ show i
    let txt = (text w) <> tinp
    let w' = w { text = txt }
    modifyWidget i w'
textHandler _ = pure ()    


-- returns a pair of focus widget with its index
getFocusWidget :: Monad m => ManagerMonadT m u (Maybe (Int, BasicWidget))
getFocusWidget = do
    im <- gets currentFocusId
    ws <- gets widgets
    return $ maybe Nothing (\i -> maybe Nothing (\w -> Just (i,w)) (Map.lookup i ws)) im

-- adding a new widget to UIState
-- returns ID of newly added widget
registerWidget :: Monad m => BasicWidget -> ManagerMonadT m u Int
registerWidget w = do
    s <- get
    ws <- widgets <$> get
    idc <- idCounter <$> get
    let idc' = idc + 1
    let ws' = Map.insert idc' w ws
    put $ s {idCounter = idc', widgets = ws'}
    return idc'

-- add event handler to handlers list at key i
addHandler :: Monad m => EventHandler m u -> Int -> ManagerMonadT m u ()
addHandler h i = do
    s <- get
    let hs = handlers s
    let hs' = Map.alter fn i hs
    put s { handlers = hs' }
    where fn Nothing = Just [h]
          fn (Just chs) = Just (h:chs)

-- adds new widget and a handler          
addWidgetWithHandler :: Monad m => BasicWidget -> EventHandler m u -> ManagerMonadT m u Int
addWidgetWithHandler w h = registerWidget w >>= \i -> addHandler h i >> return i

-- firing event to all registered handlers
fireEvent :: Monad m => Event -> ManagerMonadT m u ()
fireEvent ev = do
    let (i, pw) = source ev -- getting index and widget of the event
    res <- Map.lookup <$> pure i <*> (handlers <$> get) -- getting list of handlers if any
    maybe (return ()) -- not found, no handlers at index i
          (\hsl -> mapM_ (\h -> runHandler h ev) hsl ) -- running handlers
          res
          
    
-- given x,y coordinates finds a widget that contains them and returns it (if any)
getEventSource :: Monad m => Int -> Int -> ManagerMonadT m u (Maybe (Int, BasicWidget))
getEventSource x y = do
    ws <- widgets <$> get
    let res = Map.assocs $ Map.filter (isInWidget x y) ws
    case res of
        [] -> return Nothing
        (e:xs) -> return $ Just e -- returning first collider that catches the event, no propagation or anything, that's TBD
    