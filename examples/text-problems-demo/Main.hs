{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App
import UI.UIHElm.App.SimpleRuntime

-- | Application state
data Model = Model deriving (Show, Eq)

-- | Application messages  
data Msg = NoOp deriving (Show, Eq)

-- | Text problems demonstration app
textProblemsApp :: App Model Msg
textProblemsApp = mkApp
  (Model, [])
  view
  update

-- | View function showcasing text layout problems
view :: Model -> Widget Msg
view _ = 
  column
    [ -- Title
      text "Text Layout Problems Demo" `withStyle` titleStyle
      
    , spacer 0 20
      
    , -- Problem 1: No line breaking
      column
        [ text "Problem 1: Long text doesn't wrap" `withStyle` defaultStyle
        , spacer 0 10
        , text "This is a very long line of text that should wrap but doesn't because the current text rendering system has no line breaking capabilities and will just draw off the edge of the screen" `withStyle` defaultStyle
        ]
        
    , spacer 0 30
        
    , -- Problem 2: Newlines don't work
      column
        [ text "Problem 2: Newlines are ignored" `withStyle` defaultStyle
        , spacer 0 10
        , text "Line 1\nLine 2\nLine 3\n\nThis should be multiple lines but appears as one line with boxes" `withStyle` defaultStyle
        ]
        
    , spacer 0 30
        
    , -- Problem 3: Rich text layout is broken
      column
        [ text "Problem 3: Rich text layout is broken" `withStyle` defaultStyle
        , spacer 0 10
        , richText
            [ segment "This " defaultTextStyle
            , segment "should " (bold defaultTextStyle)
            , segment "flow " (italic defaultTextStyle)
            , segment "naturally " (withTextColor red defaultTextStyle)
            , segment "but " defaultTextStyle
            , segment "segments " (withTextColor blue defaultTextStyle)
            , segment "are " defaultTextStyle  
            , segment "spaced " (withTextColor green defaultTextStyle)
            , segment "equally " defaultTextStyle
            , segment "across " (bold $ withTextColor purple defaultTextStyle)
            , segment "the " defaultTextStyle
            , segment "width" (italic $ withTextColor orange defaultTextStyle)
            ] `withStyle` defaultStyle
        ]
        
    , spacer 0 30
        
    , -- Problem 4: No text input
      column
        [ text "Problem 4: Text inputs don't work" `withStyle` defaultStyle
        , spacer 0 10
        , text "TextBox below should be editable but isn't:" `withStyle` defaultStyle
        , spacer 0 5
        , textBox "Try typing here - nothing happens" (\_ -> NoOp) `withStyle` textBoxStyle
        ]
        
    ] `withStyle` Style
      { stylePadding = Padding 20 20 20 20
      , styleBackground = Just (Color 250 250 250 255)
      , styleForeground = Nothing
      , styleFont = Nothing
      , styleAlignment = (HAlignLeft, VAlignTop)
      , styleMinSize = Nothing
      , styleMaxSize = Nothing
      }
  where
    red = Color 255 0 0 255
    blue = Color 0 0 255 255
    green = Color 0 200 0 255
    purple = Color 128 0 128 255
    orange = Color 255 165 0 255

-- | Update function (no-op for this demo)
update :: Msg -> Model -> (Model, [Effect Msg])
update NoOp model = (model, [])

-- | Main function
main :: IO ()
main = do
  putStrLn "=== Text Layout Problems Demo ==="
  putStrLn "This demo shows various text rendering issues that need to be fixed:"
  putStrLn "1. Long text doesn't wrap and goes off screen"
  putStrLn "2. Newline characters (\\n) are rendered as boxes instead of line breaks"
  putStrLn "3. Rich text segments are spaced equally instead of flowing naturally"
  putStrLn "4. Text input widgets don't accept actual input"
  putStrLn ""
  runSimpleApp (Size 800 600) textProblemsApp
  putStrLn "Demo finished."