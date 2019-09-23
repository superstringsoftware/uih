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

import UIH.UI.BasicWidgets
import UIH.UI.ManagerMonad

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text as T

-- contains BasicWidget - that defines what to render, and a list of handlers that define behavior
type UIWidget m u = (BasicWidget, [EventHandler m u])

registerUIWidget (w, hs) = addWidgetWithHandlers w hs

newTextInput button = (button, [EventHandler textHandler])

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
        setDirty
    else pure ()
textHandler ev@(Event (EvTextInput tinp) (i,w)) = do
    liftIO $ putStrLn $ "Text to widget # " ++ show i
    let txt = (text w) <> tinp
    let w' = w { text = txt }
    modifyWidget i w'
    setDirty
textHandler _ = pure ()    
