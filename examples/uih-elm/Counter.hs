{-# LANGUAGE OverloadedStrings #-}

module Counter where

import Data.Text (Text, pack)
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App

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

-- | Counter application
counterApp :: App CounterModel CounterMsg
counterApp = mkApp
  -- init
  (CounterModel 0, [Log "Counter app initialized"])
  -- view
  viewCounter
  -- update  
  updateCounter

-- | View function - renders the current state
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

-- | Update function - handles messages and produces new state
updateCounter :: CounterMsg -> CounterModel -> (CounterModel, [Effect CounterMsg])
updateCounter msg model = case msg of
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

-- | Demo function to test the logic without rendering
demoCounter :: IO ()
demoCounter = do
  putStrLn "=== UIH-Elm Counter Demo ==="
  
  -- Initialize the runtime
  let runtime = initRuntime counterApp
  putStrLn $ "Initial state: " ++ show (runtimeState runtime)
  
  -- Test increment
  let (runtime1, effects1) = processMessage Increment runtime
  putStrLn $ "After increment: " ++ show (runtimeState runtime1)
  mapM_ executeEffect effects1
  
  -- Test increment again
  let (runtime2, effects2) = processMessage Increment runtime1  
  putStrLn $ "After second increment: " ++ show (runtimeState runtime2)
  mapM_ executeEffect effects2
  
  -- Test decrement
  let (runtime3, effects3) = processMessage Decrement runtime2
  putStrLn $ "After decrement: " ++ show (runtimeState runtime3)
  mapM_ executeEffect effects3
  
  -- Test reset
  let (runtime4, effects4) = processMessage Reset runtime3
  putStrLn $ "After reset: " ++ show (runtimeState runtime4)
  mapM_ executeEffect effects4
  
  -- Show final widget tree (structure only)
  let finalWidget = getCurrentWidget runtime4
  putStrLn $ "Final widget structure: " ++ show (widgetStructure finalWidget)

-- | Helper to show widget structure without full details
widgetStructure :: Widget msg -> String
widgetStructure widget = case widget of
  Text content _ -> "Text(" ++ show content ++ ")"
  Button label _ _ -> "Button(" ++ show label ++ ")"  
  Column children _ -> "Column[" ++ unwords (map widgetStructure children) ++ "]"
  Row children _ -> "Row[" ++ unwords (map widgetStructure children) ++ "]"
  Spacer w h -> "Spacer(" ++ show w ++ "x" ++ show h ++ ")"