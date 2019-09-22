{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , DataKinds
    , TypeApplications
    , FlexibleContexts
    , RankNTypes
    
     #-}
module UIH.UI.ManagerMonad where

-- this is a (pure?) state monad that keeps the current UI state and handles low-level events from SDL,
-- transforming them to high-level UI events (button clicks etc)

import UIH.UI.Widgets

import Data.Map.Strict as Map 

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)

import Control.Monad

-- Polymorphic UI state in the monad m
data UIState m = UIState {
    -- incrementing ids for UI elements
    idCounter :: !Int, 
    -- polymorphic map from Ints (ids) to Renderables
    -- Eventually we want to track colliders separately, since not every widget will be an event source
    widgets :: Map.Map Int (PolyWidget m)
}

initUIState = UIState {
    idCounter = 0,
    widgets = Map.empty
}

type ManagerMonadT m = StateT (UIState m) m

-- adding a new widget to UIState
registerWidget :: Monad m => PolyWidget m -> ManagerMonadT m ()
registerWidget w = do
    s <- get
    ws <- widgets <$> get
    idc <- idCounter <$> get
    let idc' = idc + 1
    let ws' = Map.insert idc' w ws
    put $ s {idCounter = idc', widgets = ws'}

-- given x,y coordinates finds a widget that contains them and returns it (if any)
getEventSource :: Monad m => Int -> Int -> ManagerMonadT m (Maybe (PolyWidget m))
getEventSource x y = do
    ws <- widgets <$> get
    res <- (filterM (isInWidget x y) (Map.elems ws))
    case res of
        [] -> return Nothing
        (e:xs) -> return $ Just e
    