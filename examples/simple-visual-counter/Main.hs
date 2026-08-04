{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App
import UI.UIHElm.App.SimpleRuntime

-- | Counter application model
data CounterModel = CounterModel 
  { count :: Int 
  } deriving (Show, Eq)

-- | Counter messages
data CounterMsg 
  = Increment
  | Decrement  
  | Reset
  deriving (Show, Eq)

-- | Simple visual counter application
simpleVisualCounterApp :: App CounterModel CounterMsg
simpleVisualCounterApp = mkApp
  -- init: starting state and effects
  (CounterModel 0, [Log "Simple visual counter initialized"])
  -- view: state → UI  
  viewSimpleVisualCounter
  -- update: message → state → (new state, effects)
  updateSimpleVisualCounter

-- | View function - renders the current state to widgets
viewSimpleVisualCounter :: CounterModel -> Widget CounterMsg
viewSimpleVisualCounter model = 
  column
    [ text "UIH-Elm Simple Counter" `withStyle` titleStyle
    , spacer 0 40
    , text (pack $ "Count: " ++ show (count model)) `withStyle` centerStyle
    , spacer 0 30
    , row
        [ button "−" Decrement `withStyle` buttonStyle
        , spacer 20 0
        , button "+" Increment `withStyle` buttonStyle  
        , spacer 20 0
        , button "Reset" Reset `withStyle` buttonStyle
        ] `withStyle` centerStyle
    , spacer 0 20
    , text "Click the buttons above!" `withStyle` defaultStyle
    ] `withStyle` centerStyle

-- | Update function - handles messages and produces new state + effects
updateSimpleVisualCounter :: CounterMsg -> CounterModel -> (CounterModel, [Effect CounterMsg])
updateSimpleVisualCounter msg model = case msg of
  Increment -> 
    let newCount = count model + 1
        newModel = model { count = newCount }
        effect = Log ("Incremented to " <> pack (show newCount))
    in (newModel, [effect])
    
  Decrement -> 
    let newCount = count model - 1
        newModel = model { count = newCount }
        effect = Log ("Decremented to " <> pack (show newCount))
    in (newModel, [effect])
    
  Reset ->
    let newModel = model { count = 0 }
        effect = Log "Counter reset to 0"
    in (newModel, [effect])

-- | Main function to run the simple visual counter
main :: IO ()
main = do
  putStrLn "=== Starting UIH-Elm Simple Visual Counter ==="
  putStrLn "This version uses simplified font rendering"
  putStrLn "Click the buttons to interact!"
  putStrLn "Close the window to quit."
  putStrLn ""
  
  -- Run the simple visual counter app
  runSimpleApp (Size 600 400) simpleVisualCounterApp
  
  putStrLn "Simple visual counter closed. Goodbye!"