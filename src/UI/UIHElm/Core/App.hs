{-# LANGUAGE OverloadedStrings #-}

module UI.UIHElm.Core.App where

import Data.Text (Text)
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.Types

-- | Effect system for side effects
data Effect msg
  = None                           -- No effect
  | Log Text                       -- Log a message
  | Batch [Effect msg]             -- Batch multiple effects
  deriving (Show, Eq)

-- | Core App type following Elm architecture
data App state msg = App
  { appInit   :: (state, [Effect msg])           -- Initial state and effects
  , appView   :: state -> Widget msg             -- Render function
  , appUpdate :: msg -> state -> (state, [Effect msg])  -- Update function
  }

-- | Application runtime state
data Runtime state msg = Runtime
  { runtimeState :: state
  , runtimeApp   :: App state msg
  , runtimeLastWidget :: Maybe (Widget msg)  -- For diffing later
  }

-- | Create initial runtime
initRuntime :: App state msg -> Runtime state msg
initRuntime app = 
  let (initialState, _effects) = appInit app
  in Runtime
    { runtimeState = initialState
    , runtimeApp = app
    , runtimeLastWidget = Nothing
    }

-- | Process a message and update runtime
processMessage :: msg -> Runtime state msg -> (Runtime state msg, [Effect msg])
processMessage msg runtime = 
  let app = runtimeApp runtime
      oldState = runtimeState runtime
      (newState, effects) = appUpdate app msg oldState
      newRuntime = runtime { runtimeState = newState }
  in (newRuntime, effects)

-- | Get current widget tree
getCurrentWidget :: Runtime state msg -> Widget msg
getCurrentWidget runtime = 
  let app = runtimeApp runtime
      state = runtimeState runtime
  in appView app state

-- | Execute effects (placeholder for now)
executeEffect :: Effect msg -> IO ()
executeEffect effect = case effect of
  None -> pure ()
  Log msg -> putStrLn $ "LOG: " ++ show msg
  Batch effects -> mapM_ executeEffect effects

-- | Helper to create apps
mkApp :: (state, [Effect msg])                    -- init
      -> (state -> Widget msg)                     -- view  
      -> (msg -> state -> (state, [Effect msg]))  -- update
      -> App state msg
mkApp initData viewFn updateFn = App
  { appInit = initData
  , appView = viewFn
  , appUpdate = updateFn
  }