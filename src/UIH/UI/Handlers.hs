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

module UIH.UI.Handlers where

-- standard event handlers that are used to create standard widgets

import UIH.UI.AbstractWidgets
import UIH.UI.ManagerMonad

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text as T

-- contains BasicWidget - that defines what to render, and a list of handlers that define behavior
-- type UIWidget m u = (BasicWidget, [EventHandler m u])

-- Registering a new AbstractWidget in Manager Monad - by adding correct handlers as needed!!!
-- registerWidget w >>= \i -> mapM_ (addHandler i) hs >> return i
registerWidget :: MonadIO m => Widget -> ManagerMonadT m u Int
-- InputText needs to handle corresponding events
registerWidget w@InputText{..} = registerWidgetPlain w >>= \i -> addHandlerPlain i (EventHandler textHandler) >> return i
-- the others do not handle any events (for now at least, will need to add hover etc)
registerWidget w = registerWidgetPlain w



-- registerUIWidget (w, hs) = addWidgetWithHandlers w hs

-- newTextInput button = (button, [EventHandler textHandler])

-- handler that handles editing "text" field in a widget    
-- use it internally to create editable labels etc
textHandler :: MonadIO m => Event -> ManagerMonadT m u ()
textHandler ev@(Event EvKbBackspace (i,w)) = do
    liftIO $ putStrLn $ "Backspace to widget # " ++ show i
    let (w', res) = hndlBackspace w
    if res then modifyWidget i w' >> setDirty else pure ()
textHandler ev@(Event (EvTextInput tinp) (i,w)) = do
    liftIO $ putStrLn $ "Text to widget # " ++ show i
    let w' = hndlAppendText tinp w
    modifyWidget i w'
    setDirty
textHandler _ = pure ()    
