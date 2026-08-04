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

-- | Text fixes demonstration app
textFixedApp :: App Model Msg
textFixedApp = mkApp
  (Model, [])
  view
  update

-- | View function showcasing fixed text rendering
view :: Model -> Widget Msg
view _ = 
  column
    [ -- Title
      text "✅ Text Layout Fixes Demo" `withStyle` titleStyle
      
    , spacer 0 20
      
    , -- Fix 1: Proper newline handling
      column
        [ text "Fix 1: Newlines now work properly" `withStyle` defaultStyle
        , spacer 0 10
        , text "Line 1\nLine 2\nLine 3\n\nThis should now display as multiple lines!" `withStyle` Style
            { stylePadding = Padding 10 10 10 10
            , styleBackground = Just (Color 240 248 255 255)  -- Light blue background
            , styleForeground = Just (Color 0 0 0 255)
            , styleFont = Just defaultFont
            , styleAlignment = (HAlignLeft, VAlignTop)
            , styleMinSize = Nothing
            , styleMaxSize = Nothing
            }
        ]
        
    , spacer 0 30
        
    , -- Fix 2: Word wrapping for long text
      column
        [ text "Fix 2: Long text now wraps properly" `withStyle` defaultStyle
        , spacer 0 10
        , text "This is a very long line of text that should now wrap nicely within the container width instead of going off the edge of the screen. The word wrapping algorithm breaks at word boundaries to maintain readability." `withStyle` Style
            { stylePadding = Padding 10 10 10 10
            , styleBackground = Just (Color 255 248 240 255)  -- Light orange background
            , styleForeground = Just (Color 0 0 0 255)
            , styleFont = Just defaultFont
            , styleAlignment = (HAlignLeft, VAlignTop)
            , styleMinSize = Nothing
            , styleMaxSize = Nothing
            }
        ]
        
    , spacer 0 30
        
    , -- Fix 3: Rich text natural flow layout
      column
        [ text "Fix 3: Rich text now flows naturally" `withStyle` defaultStyle
        , spacer 0 10
        , richText
            [ segment "This " defaultTextStyle
            , segment "rich " (bold defaultTextStyle)
            , segment "text " (italic defaultTextStyle)
            , segment "now " (withTextColor red defaultTextStyle)
            , segment "flows " defaultTextStyle
            , segment "naturally " (withTextColor blue defaultTextStyle)
            , segment "based " defaultTextStyle  
            , segment "on " (withTextColor green defaultTextStyle)
            , segment "actual " defaultTextStyle
            , segment "text " (bold $ withTextColor purple defaultTextStyle)
            , segment "measurements " defaultTextStyle
            , segment "instead " (italic $ withTextColor orange defaultTextStyle)
            , segment "of " defaultTextStyle
            , segment "equal " (withTextColor pink defaultTextStyle)
            , segment "spacing!" defaultTextStyle
            ] `withStyle` Style
            { stylePadding = Padding 10 10 10 10
            , styleBackground = Just (Color 248 255 248 255)  -- Light green background
            , styleForeground = Nothing
            , styleFont = Nothing
            , styleAlignment = (HAlignLeft, VAlignTop)
            , styleMinSize = Nothing
            , styleMaxSize = Nothing
            }
        ]
        
    , spacer 0 30
        
    , -- Demonstration of different alignments
      column
        [ text "Bonus: Different text alignments" `withStyle` defaultStyle
        , spacer 0 10
        , text "Left aligned text (default)" `withStyle` Style
            { stylePadding = Padding 8 8 8 8
            , styleBackground = Just (Color 255 255 255 255)
            , styleForeground = Just (Color 0 0 0 255)
            , styleFont = Just defaultFont
            , styleAlignment = (HAlignLeft, VAlignTop)
            , styleMinSize = Nothing
            , styleMaxSize = Nothing
            }
        , spacer 0 5
        , text "Center aligned text" `withStyle` Style
            { stylePadding = Padding 8 8 8 8
            , styleBackground = Just (Color 255 255 255 255)
            , styleForeground = Just (Color 0 0 255 255)
            , styleFont = Just defaultFont
            , styleAlignment = (HAlignCenter, VAlignTop)
            , styleMinSize = Nothing
            , styleMaxSize = Nothing
            }
        , spacer 0 5
        , text "Right aligned text" `withStyle` Style
            { stylePadding = Padding 8 8 8 8
            , styleBackground = Just (Color 255 255 255 255)
            , styleForeground = Just (Color 255 0 0 255)
            , styleFont = Just defaultFont
            , styleAlignment = (HAlignRight, VAlignTop)
            , styleMinSize = Nothing
            , styleMaxSize = Nothing
            }
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
    pink = Color 255 192 203 255

-- | Update function (no-op for this demo)
update :: Msg -> Model -> (Model, [Effect Msg])
update NoOp model = (model, [])

-- | Main function
main :: IO ()
main = do
  putStrLn "=== Text Layout Fixes Demo ==="
  putStrLn "This demo shows the fixed text rendering:"
  putStrLn "✅ Newlines now render as proper line breaks"
  putStrLn "✅ Long text wraps at word boundaries"
  putStrLn "✅ Rich text flows naturally based on actual measurements"  
  putStrLn "✅ Different text alignments work properly"
  putStrLn ""
  runSimpleApp (Size 800 600) textFixedApp
  putStrLn "Demo finished."