{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UI.UIHElm.App.SimpleRuntime where

import qualified SDL
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (pack)

import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App
import UI.UIHElm.Rendering.Simple
import UI.UIHElm.Events.Events

-- | Application runtime with simple SDL integration
data SimpleAppRuntime state msg = SimpleAppRuntime
  { sarApp     :: App state msg
  , sarState   :: state
  , sarSDL     :: SimpleRenderState
  , sarRunning :: Bool
  , sarSize    :: Size
  }

-- | Initialize simple application runtime
initSimpleAppRuntime :: Size -> App state msg -> IO (Either String (SimpleAppRuntime state msg))
initSimpleAppRuntime windowSize app = do
  result <- initSimpleSDL "UIH-Elm App" windowSize
  case result of
    Left err -> return $ Left err
    Right sdlState -> do
      let (initialState, _effects) = appInit app
      return $ Right $ SimpleAppRuntime
        { sarApp = app
        , sarState = initialState
        , sarSDL = sdlState
        , sarRunning = True  
        , sarSize = windowSize
        }

-- | Clean up simple runtime resources
cleanupSimpleRuntime :: SimpleAppRuntime state msg -> IO ()
cleanupSimpleRuntime SimpleAppRuntime{..} = cleanupSimpleSDL sarSDL

-- | Process a message and update runtime state
processSimpleMessage :: msg -> SimpleAppRuntime state msg -> IO (SimpleAppRuntime state msg)
processSimpleMessage msg runtime@SimpleAppRuntime{..} = do
  let (newState, effects) = appUpdate sarApp msg sarState
  -- Execute effects (for now just log them)
  mapM_ executeEffect effects
  return $ runtime { sarState = newState }

-- | Main application loop with simple rendering
runSimpleApp :: Size -> App state msg -> IO ()
runSimpleApp windowSize app = do
  result <- initSimpleAppRuntime windowSize app
  case result of
    Left err -> putStrLn $ "Failed to initialize: " ++ err
    Right runtime -> do
      simpleMainLoop runtime
      cleanupSimpleRuntime runtime

-- | Main event/render loop  
simpleMainLoop :: SimpleAppRuntime state msg -> IO ()
simpleMainLoop runtime = do
  let SimpleAppRuntime{..} = runtime
  unless (not sarRunning) $ do
    -- Handle events
    events <- SDL.pollEvents
    newRuntime <- handleSimpleEvents events runtime
    
    -- Render current state
    let SimpleAppRuntime newApp newState newSDL newRunning newSize = newRuntime
    let currentWidget = appView newApp newState
    renderWidgetTree newSDL newSize currentWidget
    
    -- Continue loop
    simpleMainLoop newRuntime

-- | Handle all SDL events
handleSimpleEvents :: [SDL.Event] -> SimpleAppRuntime state msg -> IO (SimpleAppRuntime state msg)
handleSimpleEvents events runtime = foldl handleSingleSimpleEvent (return runtime) events

-- | Handle a single SDL event
handleSingleSimpleEvent :: IO (SimpleAppRuntime state msg) -> SDL.Event -> IO (SimpleAppRuntime state msg)
handleSingleSimpleEvent runtimeIO event = do
  runtime@SimpleAppRuntime{..} <- runtimeIO
  
  case sdlEventToSystemEvent event of
    Nothing -> return runtime  -- Ignore unknown events
    
    Just WindowClose -> return $ runtime { sarRunning = False }
    
    Just systemEvent -> do
      -- Build event map from current widget tree
      let currentWidget = appView sarApp sarState
      let layoutWidgets = calculateLayout sarSize currentWidget
      let eventMap = buildEventMap layoutWidgets
      
      -- Process system event to get message
      case processSystemEvent systemEvent eventMap of
        Nothing -> return runtime  -- No message generated
        Just msg -> processSimpleMessage msg runtime