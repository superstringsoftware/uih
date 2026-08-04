{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UI.UIHElm.App.Runtime where

-- This module is temporarily disabled to avoid conflicts
-- Use UI.UIHElm.App.SimpleRuntime instead

{- DISABLED

import qualified SDL
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (pack)

import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App
-- import UI.UIHElm.Rendering.SDL
-- import UI.UIHElm.Events.Events

-- | Application runtime with SDL integration (DISABLED - use SimpleRuntime instead)
-- data AppRuntime state msg = AppRuntime
--   { arApp     :: App state msg
--   , arState   :: state
--   , arSDL     :: SDLRenderState
--   , arRunning :: Bool
--   , arSize    :: Size
--   }

-- | Initialize application runtime
initAppRuntime :: Size -> App state msg -> IO (AppRuntime state msg)
initAppRuntime windowSize app = do
  sdlState <- initSDL "UIH-Elm App" windowSize
  let (initialState, _effects) = appInit app
  return $ AppRuntime
    { arApp = app
    , arState = initialState
    , arSDL = sdlState
    , arRunning = True  
    , arSize = windowSize
    }

-- | Clean up runtime resources
cleanupRuntime :: AppRuntime state msg -> IO ()
cleanupRuntime AppRuntime{..} = cleanupSDL arSDL

-- | Process a message and update runtime state
processRuntimeMessage :: msg -> AppRuntime state msg -> IO (AppRuntime state msg)
processRuntimeMessage msg runtime@AppRuntime{..} = do
  let (newState, effects) = appUpdate arApp msg arState
  -- Execute effects (for now just log them)
  mapM_ executeEffect effects
  return $ runtime { arState = newState }

-- | Main application loop
runApp :: Size -> App state msg -> IO ()
runApp windowSize app = do
  runtime <- initAppRuntime windowSize app
  mainLoop runtime
  cleanupRuntime runtime

-- | Main event/render loop  
mainLoop :: AppRuntime state msg -> IO ()
mainLoop runtime = do
  let AppRuntime{..} = runtime
  unless (not arRunning) $ do
    -- Handle events
    events <- SDL.pollEvents
    newRuntime <- handleEvents events runtime
    
    -- Render current state  
    let AppRuntime newApp newState newSDL newRunning newSize = newRuntime
    let currentWidget = appView newApp newState
    renderWidgetTree newSDL newSize currentWidget
    
    -- Continue loop
    mainLoop newRuntime

-- | Handle all SDL events
handleEvents :: [SDL.Event] -> AppRuntime state msg -> IO (AppRuntime state msg)
handleEvents events runtime = foldl handleSingleEvent (return runtime) events

-- | Handle a single SDL event
handleSingleEvent :: IO (AppRuntime state msg) -> SDL.Event -> IO (AppRuntime state msg)
handleSingleEvent runtimeIO event = do
  runtime@AppRuntime{..} <- runtimeIO
  
  case sdlEventToSystemEvent event of
    Nothing -> return runtime  -- Ignore unknown events
    
    Just WindowClose -> return $ runtime { arRunning = False }
    
    Just systemEvent -> do
      -- Build event map from current widget tree
      let currentWidget = appView arApp arState
      let layoutWidgets = calculateLayout arSize currentWidget
      let eventMap = buildEventMap layoutWidgets
      
      -- Process system event to get message
      case processSystemEvent systemEvent eventMap of
        Nothing -> return runtime  -- No message generated
        Just msg -> processRuntimeMessage msg runtime

-- | Test function for the runtime system
testRuntime :: IO ()
testRuntime = do
  putStrLn "Testing UIH-Elm Runtime..."
  
  -- Simple test app
  let testApp = mkApp
        (0 :: Int, [])
        (\count -> column 
          [ text "Click Counter"
          , text (pack $ show count)
          , button "Click Me!" 'c'
          ])
        (\msg count -> case msg of
          'c' -> (count + 1, [Log "Clicked!"]))
  
  runApp (Size 400 300) testApp

-}