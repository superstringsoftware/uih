{-# LANGUAGE OverloadedStrings #-}

module UI.UIHElm.Styling.DSL where

import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Map as Map
import UI.UIHElm.Core.Types hiding (black, white, blue, red, green, gray, fontSize)
import qualified UI.UIHElm.Styling.Stylesheet as S

-- | CSS-like DSL for writing styles
-- Usage: style [ backgroundColor red, color white, fontSize 18, padding 10 ]

-- | Color helpers
rgb :: Word8 -> Word8 -> Word8 -> S.StyleColor
rgb r g b = S.Solid (Color r g b 255)

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> S.StyleColor  
rgba r g b a = S.Solid (Color r g b a)

-- | Common colors
red, green, blue, white, black, gray, lightGray, darkGray :: S.StyleColor
red = rgb 255 0 0
green = rgb 0 255 0  
blue = rgb 0 0 255
white = rgb 255 255 255
black = rgb 0 0 0
gray = rgb 128 128 128
lightGray = rgb 211 211 211
darkGray = rgb 64 64 64

-- | Background styling
backgroundColor :: S.StyleColor -> S.StyleRule
backgroundColor color = S.defaultStyle { S.background = Just color }

-- | Text styling  
color :: S.StyleColor -> S.StyleRule
color c = S.defaultStyle { S.foreground = Just c }

fontSize :: Int -> S.StyleRule
fontSize size = S.defaultStyle 
  { S.typography = Just $ S.defaultTypography { S.typographyFontSize = size } }

fontWeight :: S.FontWeight -> S.StyleRule
fontWeight weight = S.defaultStyle
  { S.typography = Just $ S.defaultTypography { S.typographyFontWeight = weight } }

fontFamily :: Text -> S.StyleRule
fontFamily family = S.defaultStyle
  { S.typography = Just $ S.defaultTypography { S.typographyFontFamily = family } }

-- | Spacing
padding :: Int -> S.StyleRule
padding p = S.defaultStyle { S.padding = Just $ Padding p p p p }

paddingX :: Int -> S.StyleRule  
paddingX p = S.defaultStyle { S.padding = Just $ Padding 0 p 0 p }

paddingY :: Int -> S.StyleRule
paddingY p = S.defaultStyle { S.padding = Just $ Padding p 0 p 0 }

margin :: Int -> S.StyleRule
margin m = S.defaultStyle { S.margin = Just $ Padding m m m m }

-- | Borders
border :: Int -> S.StyleColor -> S.StyleRule
border width color = S.defaultStyle 
  { S.border = Just $ S.Border (S.AllSides width) S.SolidBorder color (S.UniformRadius 0) }

borderRadius :: Int -> S.StyleRule
borderRadius radius = S.defaultStyle
  { S.border = Just $ S.defaultBorder { S.borderRadius = S.UniformRadius radius } }

-- | Shadows
boxShadow :: Int -> Int -> Int -> S.StyleColor -> S.StyleRule
boxShadow offsetX offsetY blur color = S.defaultStyle
  { S.shadow = Just $ S.Shadow offsetX offsetY blur 0 color }

-- | Layout
flexDirection :: S.FlexDirection -> S.StyleRule
flexDirection dir = S.defaultStyle
  { S.layout = Just $ S.Layout dir S.FlexStart S.AlignStart S.NoWrap Nothing }

justifyContent :: S.JustifyContent -> S.StyleRule  
justifyContent justify = S.defaultStyle
  { S.layout = Just $ S.Layout S.Row justify S.AlignStart S.NoWrap Nothing }

alignItems :: S.AlignItems -> S.StyleRule
alignItems align = S.defaultStyle
  { S.layout = Just $ S.Layout S.Row S.FlexStart align S.NoWrap Nothing }

-- | Sizing
width :: Int -> S.StyleRule
width w = S.defaultStyle
  { S.sizing = Just $ S.Sizing { S.sizingWidth = S.Pixels w, S.sizingHeight = S.Auto, S.sizingMinWidth = Nothing, S.sizingMaxWidth = Nothing, S.sizingMinHeight = Nothing, S.sizingMaxHeight = Nothing } }

height :: Int -> S.StyleRule  
height h = S.defaultStyle
  { S.sizing = Just $ S.Sizing { S.sizingWidth = S.Auto, S.sizingHeight = S.Pixels h, S.sizingMinWidth = Nothing, S.sizingMaxWidth = Nothing, S.sizingMinHeight = Nothing, S.sizingMaxHeight = Nothing } }

-- | Opacity and cursor
opacity :: Float -> S.StyleRule
opacity o = S.defaultStyle { S.opacity = Just o }

cursor :: S.CursorType -> S.StyleRule
cursor c = S.defaultStyle { S.cursor = Just c }

-- | S.Transitions
transition :: Text -> Int -> S.EasingFunction -> S.StyleRule
transition prop duration easing = S.defaultStyle
  { S.transitions = [S.Transition prop duration easing] }

-- | Style combining function
style :: [S.StyleRule] -> S.StyleRule
style = foldl (S.<+>) S.defaultStyle

-- | Predefined common styles for buttons
buttonStyle :: S.StyleRule
buttonStyle = style
  [ backgroundColor lightGray
  , color black
  , padding 12
  , borderRadius 4
  , border 1 gray
  , cursor S.Pointer
  , transition "background-color" 200 S.EaseInOut
  ]

primaryButtonStyle :: S.StyleRule  
primaryButtonStyle = style
  [ backgroundColor blue
  , color white
  , padding 12
  , borderRadius 4
  , cursor S.Pointer
  , transition "background-color" 200 S.EaseInOut
  ]

-- | Text styles
titleStyle :: S.StyleRule
titleStyle = style
  [ fontSize 24
  , fontWeight S.Bold
  , color black
  ]

subtitleStyle :: S.StyleRule
subtitleStyle = style
  [ fontSize 18
  , color darkGray
  ]

-- | Layout styles
centerStyle :: S.StyleRule
centerStyle = style
  [ justifyContent S.Center
  , alignItems S.AlignCenter
  ]

spaceBetweenStyle :: S.StyleRule
spaceBetweenStyle = style
  [ justifyContent S.SpaceBetween
  , alignItems S.AlignCenter
  ]

-- | Card-like container
cardStyle :: S.StyleRule
cardStyle = style
  [ backgroundColor white
  , borderRadius 8
  , boxShadow 0 2 8 (rgba 0 0 0 64)
  , padding 16
  ]

-- | Interactive states helper
interactive :: S.StyleRule -> S.StyleRule -> S.StyleRule -> S.StatefulStyle
interactive base hoverStyle activeStyle = S.stateful base
  [ (S.Hover, hoverStyle)
  , (S.Active, activeStyle)
  ]

-- | Button with hover/active states
interactiveButton :: S.StyleRule
interactiveButton = S.getStyleForState S.NormalState $ interactive buttonStyle hoverStyle activeStyle
  where
    hoverStyle = backgroundColor (rgba 200 200 200 255)
    activeStyle = backgroundColor (rgba 180 180 180 255)

-- | S.Theme helpers
darkTheme :: S.Theme
darkTheme = S.Theme
  { S.themeName = "Dark"
  , S.themeColors = Map.fromList
      [ ("background", rgb 32 32 32)
      , ("surface", rgb 48 48 48) 
      , ("primary", rgb 100 150 255)
      , ("text", rgb 255 255 255)
      , ("textSecondary", rgb 180 180 180)
      ]
  , S.themeTypography = Map.fromList
      [ ("body", S.defaultTypography)
      , ("title", S.defaultTypography { S.typographyFontSize = 24, S.typographyFontWeight = S.Bold })
      ]
  , S.themeSpacing = Map.fromList
      [ ("xs", 4), ("sm", 8), ("md", 16), ("lg", 24), ("xl", 32) ]
  , S.themeShadows = Map.fromList
      [ ("small", S.Shadow 0 1 3 0 (rgba 0 0 0 64))
      , ("medium", S.Shadow 0 4 8 0 (rgba 0 0 0 96))
      ]
  }

lightTheme :: S.Theme  
lightTheme = S.Theme
  { S.themeName = "Light"
  , S.themeColors = Map.fromList
      [ ("background", rgb 255 255 255)
      , ("surface", rgb 248 248 248)
      , ("primary", rgb 50 100 200) 
      , ("text", rgb 0 0 0)
      , ("textSecondary", rgb 100 100 100)
      ]
  , S.themeTypography = Map.fromList
      [ ("body", S.defaultTypography)
      , ("title", S.defaultTypography { S.typographyFontSize = 24, S.typographyFontWeight = S.Bold })
      ]
  , S.themeSpacing = Map.fromList  
      [ ("xs", 4), ("sm", 8), ("md", 16), ("lg", 24), ("xl", 32) ]
  , S.themeShadows = Map.fromList
      [ ("small", S.Shadow 0 1 3 0 (rgba 0 0 0 32))
      , ("medium", S.Shadow 0 4 8 0 (rgba 0 0 0 48))
      ]
  }