{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget  
import UI.UIHElm.Core.App

-- Counter model and messages
data CounterModel = CounterModel { count :: Int } deriving (Show, Eq)
data CounterMsg = Increment | Decrement | Reset deriving (Show, Eq)

-- Counter app definition
counterApp :: App CounterModel CounterMsg
counterApp = mkApp
  (CounterModel 0, [Log "Counter initialized"])
  viewCounter
  updateCounter

-- View function
viewCounter :: CounterModel -> Widget CounterMsg
viewCounter model = 
  column
    [ text "UIH-Elm Counter" `withStyle` titleStyle
    , spacer 0 20
    , text (pack $ show $ count model) `withStyle` centerStyle
    , spacer 0 10
    , row
        [ button "-" Decrement
        , spacer 10 0  
        , button "+" Increment
        , spacer 10 0
        , button "Reset" Reset
        ]
    ] `withStyle` centerStyle

-- Update function
updateCounter :: CounterMsg -> CounterModel -> (CounterModel, [Effect CounterMsg])
updateCounter msg model = case msg of
  Increment -> 
    let newModel = model { count = count model + 1 }
    in (newModel, [Log $ "Incremented to " <> pack (show $ count newModel)])
  Decrement ->
    let newModel = model { count = count model - 1 }  
    in (newModel, [Log $ "Decremented to " <> pack (show $ count newModel)])
  Reset ->
    let newModel = model { count = 0 }
    in (newModel, [Log "Reset to 0"])

-- Test our counter logic
main :: IO ()
main = do
  putStrLn "=== Testing UIH-Elm Counter Logic ==="
  
  -- Initialize runtime
  let runtime = initRuntime counterApp
  putStrLn $ "Initial state: " ++ show (runtimeState runtime)
  
  -- Test sequence: increment, increment, decrement, reset
  let testMessages = [Increment, Increment, Decrement, Reset]
  
  finalRuntime <- foldl testMessage (pure runtime) testMessages
  
  putStrLn "\n=== Final Widget Structure ==="
  let finalWidget = getCurrentWidget finalRuntime
  putStrLn $ widgetStructure finalWidget
  
  putStrLn "\n=== Test Complete! ==="

-- Helper to process messages and show effects
testMessage :: IO (Runtime CounterModel CounterMsg) -> CounterMsg -> IO (Runtime CounterModel CounterMsg)
testMessage runtimeIO msg = do
  runtime <- runtimeIO
  let (newRuntime, effects) = processMessage msg runtime
  putStrLn $ "\nProcessing: " ++ show msg
  putStrLn $ "New state: " ++ show (runtimeState newRuntime)
  putStrLn "Effects:"
  mapM_ executeEffect effects
  return newRuntime

-- Helper to show widget structure
widgetStructure :: Widget msg -> String
widgetStructure widget = case widget of
  Text content _ -> "Text(" ++ show content ++ ")"
  Button label _ _ -> "Button(" ++ show label ++ ")"  
  Column children _ -> "Column[" ++ unwords (map widgetStructure children) ++ "]"
  Row children _ -> "Row[" ++ unwords (map widgetStructure children) ++ "]"
  Spacer w h -> "Spacer(" ++ show w ++ "x" ++ show h ++ ")"