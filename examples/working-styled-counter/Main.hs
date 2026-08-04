{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import Control.Monad (unless)
import Data.Text (pack)

import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App
import UI.UIHElm.App.SimpleRuntime

-- | Application state
type Counter = Int

-- | Application messages
data Msg = Increment | Decrement | Reset
  deriving (Show, Eq)

-- | Create working styled counter app
workingStyledCounterApp :: App Counter Msg
workingStyledCounterApp = mkApp
  (0, [])  -- Initial state and effects
  view     -- View function
  update   -- Update function

-- | Simple view function that works with current system
view :: Counter -> Widget Msg
view count = 
  column
    [ -- Title 
      text "Working Styled Counter Demo" `withStyle` titleStyle
    
    , -- Counter display with larger font
      text (pack $ "Count: " ++ show count) `withStyle` centerStyle
        
    , -- Button row with clear labels
      row
        [ -- Decrement button
          button "Decrement (-)" Decrement
            
        , -- Reset button  
          button "Reset (0)" Reset
            
        , -- Increment button
          button "Increment (+)" Increment
        ] `withStyle` centerStyle
          
    , -- Instructions
      text "Click buttons to change the counter value" `withStyle` defaultStyle
        
    ] `withStyle` centerStyle

-- | Update function
update :: Msg -> Counter -> (Counter, [Effect Msg])
update msg count = case msg of
  Increment -> (count + 1, [Log "Incremented counter"])
  Decrement -> (count - 1, [Log "Decremented counter"])
  Reset     -> (0, [Log "Reset counter to zero"])

-- | Main function
main :: IO ()
main = do
  putStrLn "Starting working styled counter..."
  runSimpleApp (Size 600 400) workingStyledCounterApp
  putStrLn "Application finished successfully"