{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App
import UI.UIHElm.App.Runtime

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

-- | Visual counter application
visualCounterApp :: App CounterModel CounterMsg
visualCounterApp = mkApp
  -- init: starting state and effects
  (CounterModel 0, [Log "Visual counter initialized"])
  -- view: state → UI  
  viewVisualCounter
  -- update: message → state → (new state, effects)
  updateVisualCounter

-- | View function - renders the current state to widgets
viewVisualCounter :: CounterModel -> Widget CounterMsg
viewVisualCounter model = 
  column
    [ text "UIH-Elm Visual Counter" `withStyle` titleStyle
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
updateVisualCounter :: CounterMsg -> CounterModel -> (CounterModel, [Effect CounterMsg])
updateVisualCounter msg model = case msg of
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

-- | Main function to run the visual counter
main :: IO ()
main = do
  putStrLn "=== Starting UIH-Elm Visual Counter ==="
  putStrLn "Click the buttons to interact!"
  putStrLn "Close the window to quit."
  putStrLn ""
  
  -- Run the visual counter app
  runApp (Size 600 400) visualCounterApp
  
  putStrLn "Visual counter closed. Goodbye!"

-- | Test the visual counter (same as main)
runVisualCounter :: IO ()
runVisualCounter = main