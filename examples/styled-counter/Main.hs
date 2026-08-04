{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import Control.Monad (unless)
import Data.Text (Text, pack)
import qualified UI.UIHElm.Core.Widget as W

import UI.UIHElm.Core.Types hiding (white, black, red, green, blue, gray, lightGray, darkGray, fontSize)
import UI.UIHElm.Core.App
import UI.UIHElm.App.SimpleRuntime
import UI.UIHElm.Styling.SimpleStyles

-- | Application state
type Counter = Int

-- | Application messages
data Msg = Increment | Decrement | Reset
  deriving (Show, Eq)

-- | Create styled counter app with CSS-like styling
styledCounterApp :: App Counter Msg
styledCounterApp = mkApp
  (0, [])  -- Initial state and effects
  view     -- View function
  update   -- Update function

-- | View function using CSS-like styling
view :: Counter -> W.Widget Msg
view count = 
  W.column
    [ -- Title with custom styling
      W.text "🎨 Styled Counter Demo" `W.withStyle` (simpleToStyle titleStyle)
    
    , W.spacer 0 20
    
    , -- Counter display with card-like styling
      W.text (pack $ "Count: " ++ show count) `W.withStyle` (simpleToStyle $ style 
        [ backgroundColor white
        , textColor blue  
        , fontSize 28
        , padding 20
        , borderColor lightGray
        , borderWidth 2
        ])
        
    , W.spacer 0 30
        
    , -- Button row with custom styled buttons
      W.row
        [ -- Decrement button (red theme)
          W.button "−" Decrement `W.withStyle` (simpleToStyle $ style
            [ backgroundColor red
            , textColor white
            , fontSize 24
            , padding 16
            , borderColor darkGray
            , borderWidth 1
            ])
            
        , W.spacer 15 0
            
        , -- Reset button (gray theme)
          W.button "Reset" Reset `W.withStyle` (simpleToStyle $ style
            [ backgroundColor gray
            , textColor white
            , fontSize 16
            , padding 12
            , borderColor black
            , borderWidth 1
            ])
            
        , W.spacer 15 0
            
        , -- Increment button (green theme)
          W.button "+" Increment `W.withStyle` (simpleToStyle $ style
            [ backgroundColor green
            , textColor white
            , fontSize 24
            , padding 16
            , borderColor darkGray
            , borderWidth 1
            ])
        ] `W.withStyle` W.centerStyle
          
    , W.spacer 0 30
          
    , -- Instructions with custom styling
      W.text "Click the colorful buttons above!" `W.withStyle` (simpleToStyle $ style
        [ textColor darkGray
        , fontSize 14
        ])
        
    ] `W.withStyle` (simpleToStyle $ style
      [ backgroundColor lightGray
      , padding 40
      ])

-- | Update function
update :: Msg -> Counter -> (Counter, [Effect Msg])
update msg count = case msg of
  Increment -> (count + 1, [Log "Incremented"])
  Decrement -> (count - 1, [Log "Decremented"])
  Reset     -> (0, [Log "Reset counter"])

-- | Main function
main :: IO ()
main = do
  putStrLn "Starting styled counter with CSS-like styling..."
  runSimpleApp (Size 500 400) styledCounterApp
  putStrLn "Application finished successfully"