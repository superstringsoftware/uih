{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack, unpack)
import UI.UIHElm.Core.Types hiding (white, black, red, green, blue, gray, lightGray, darkGray)
import UI.UIHElm.Core.Widget
import UI.UIHElm.Core.App
import UI.UIHElm.App.SimpleRuntime

-- | Application state
data TextDemoModel = TextDemoModel 
  { textBoxContent :: Text
  , textAreaContent :: Text
  } deriving (Show, Eq)

-- | Application messages
data TextDemoMsg 
  = TextBoxChanged Text
  | TextAreaChanged Text
  deriving (Show, Eq)

-- | Text demo application
textDemoApp :: App TextDemoModel TextDemoMsg
textDemoApp = mkApp
  (TextDemoModel "Hello World" "Line 1\nLine 2\nLine 3", [])
  view
  update

-- | View function showcasing different text widgets
view :: TextDemoModel -> Widget TextDemoMsg
view model = 
  column
    [ -- Title
      text "🎨 Text Widget Demonstration" `withStyle` titleStyle
      
    , spacer 0 20
      
    , -- Simple text display
      text "Basic text display with default styling" `withStyle` defaultStyle
      
    , spacer 0 15
      
    , -- Text box demo
      column
        [ text "TextBox (single-line input):" `withStyle` defaultStyle
        , spacer 0 5
        , textBox (textBoxContent model) TextBoxChanged `withStyle` textBoxStyle
        , spacer 0 5
        , text ("You typed: " <> textBoxContent model) `withStyle` defaultStyle
        ]
        
    , spacer 0 20
        
    , -- Text area demo  
      column
        [ text "TextArea (multi-line input):" `withStyle` defaultStyle
        , spacer 0 5
        , textArea (textAreaContent model) TextAreaChanged 3 `withStyle` textAreaStyle
        , spacer 0 5
        , text ("Lines: " <> pack (show $ length $ lines $ unpack $ textAreaContent model)) `withStyle` defaultStyle
        ]
        
    , spacer 0 20
        
    , -- Rich text demo
      column
        [ text "Rich Text (mixed styling):" `withStyle` defaultStyle
        , spacer 0 10
        , richText
            [ segment "Normal text, " defaultTextStyle
            , segment "bold text, " (bold defaultTextStyle)
            , segment "italic text, " (italic defaultTextStyle)
            , segment "colored text" (withTextColor red defaultTextStyle)
            ] `withStyle` defaultStyle
        ]
        
    , spacer 0 20
        
    , -- Formatted text examples
      column
        [ text "Different font sizes and colors:" `withStyle` defaultStyle
        , spacer 0 10
        , richText
            [ segment "Small " (withTextFont (FontSpec "Arial" 12) $ withTextColor gray defaultTextStyle)
            , segment "Medium " (withTextFont (FontSpec "Arial" 16) $ withTextColor blue defaultTextStyle)  
            , segment "Large" (withTextFont (FontSpec "Arial" 20) $ withTextColor green defaultTextStyle)
            ] `withStyle` defaultStyle
        ]
        
    ] `withStyle` Style
      { stylePadding = Padding 30 30 30 30
      , styleBackground = Just lightGray
      , styleForeground = Nothing
      , styleFont = Nothing
      , styleAlignment = (HAlignLeft, VAlignTop)
      , styleMinSize = Nothing
      , styleMaxSize = Nothing
      }
  where
    lines str = case break (== '\n') str of
      (line, []) -> [line]  
      (line, _:rest) -> line : lines rest
    red = Color 255 0 0 255
    blue = Color 0 0 255 255  
    green = Color 0 200 0 255
    gray = Color 128 128 128 255
    lightGray = Color 240 240 240 255

-- | Update function
update :: TextDemoMsg -> TextDemoModel -> (TextDemoModel, [Effect TextDemoMsg])
update msg model = case msg of
  TextBoxChanged newText -> 
    (model { textBoxContent = newText }, [Log "TextBox updated"])
  TextAreaChanged newText ->
    (model { textAreaContent = newText }, [Log "TextArea updated"])

-- | Main function
main :: IO ()
main = do
  putStrLn "Starting text widget demonstration..."
  runSimpleApp (Size 800 600) textDemoApp
  putStrLn "Text demo finished."