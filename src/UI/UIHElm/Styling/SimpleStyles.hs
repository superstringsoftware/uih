{-# LANGUAGE OverloadedStrings #-}

module UI.UIHElm.Styling.SimpleStyles where

import Data.Text (Text)
import Data.Word (Word8)
import UI.UIHElm.Core.Types hiding (black, white, blue, red, green, gray, fontSize)
import UI.UIHElm.Core.Widget (Style(..), defaultStyle)

-- | Simple styling approach to avoid field name conflicts
-- This is a simplified version for demo purposes

-- | Simple style configuration
data SimpleStyle = SimpleStyle
  { sBgColor     :: Maybe Color
  , sTextColor   :: Maybe Color  
  , sFontSize    :: Maybe Int
  , sBorderColor :: Maybe Color
  , sBorderWidth :: Maybe Int
  , sPadding     :: Maybe Int
  , sMargin      :: Maybe Int
  } deriving (Show, Eq)

-- | Default simple style
defaultSimpleStyle :: SimpleStyle
defaultSimpleStyle = SimpleStyle
  { sBgColor = Nothing
  , sTextColor = Just (Color 0 0 0 255)  -- black
  , sFontSize = Just 16
  , sBorderColor = Nothing
  , sBorderWidth = Nothing
  , sPadding = Nothing
  , sMargin = Nothing
  }

-- | Color helpers
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = Color r g b 255

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgba r g b a = Color r g b a

-- | Common colors  
red, green, blue, white, black, gray, lightGray, darkGray :: Color
red = rgb 255 0 0
green = rgb 0 255 0
blue = rgb 0 0 255
white = rgb 255 255 255
black = rgb 0 0 0
gray = rgb 128 128 128
lightGray = rgb 211 211 211
darkGray = rgb 64 64 64

-- | Style builders
backgroundColor :: Color -> SimpleStyle
backgroundColor c = defaultSimpleStyle { sBgColor = Just c }

textColor :: Color -> SimpleStyle
textColor c = defaultSimpleStyle { sTextColor = Just c }

fontSize :: Int -> SimpleStyle
fontSize s = defaultSimpleStyle { sFontSize = Just s }

borderColor :: Color -> SimpleStyle  
borderColor c = defaultSimpleStyle { sBorderColor = Just c }

borderWidth :: Int -> SimpleStyle
borderWidth w = defaultSimpleStyle { sBorderWidth = Just w }

padding :: Int -> SimpleStyle
padding p = defaultSimpleStyle { sPadding = Just p }

margin :: Int -> SimpleStyle
margin m = defaultSimpleStyle { sMargin = Just m }

-- | Style combination
combineSimpleStyles :: [SimpleStyle] -> SimpleStyle
combineSimpleStyles = foldr combineTwo defaultSimpleStyle
  where
    combineTwo s1 s2 = SimpleStyle
      { sBgColor = sBgColor s1 `orElse` sBgColor s2
      , sTextColor = sTextColor s1 `orElse` sTextColor s2
      , sFontSize = sFontSize s1 `orElse` sFontSize s2
      , sBorderColor = sBorderColor s1 `orElse` sBorderColor s2
      , sBorderWidth = sBorderWidth s1 `orElse` sBorderWidth s2
      , sPadding = sPadding s1 `orElse` sPadding s2
      , sMargin = sMargin s1 `orElse` sMargin s2
      }
    orElse Nothing b = b
    orElse a _ = a

-- | Convert SimpleStyle to old Style for compatibility  
simpleToStyle :: SimpleStyle -> Style
simpleToStyle ss = Style
  { stylePadding = maybe noPadding (\p -> Padding p p p p) (sPadding ss)
  , styleBackground = sBgColor ss
  , styleForeground = sTextColor ss
  , styleFont = maybe (Just defaultFont) (\size -> Just (FontSpec "Arial" size)) (sFontSize ss)
  , styleAlignment = (HAlignCenter, VAlignCenter)  -- Default alignment
  , styleMinSize = Nothing
  , styleMaxSize = Nothing
  }

-- | Helper function to create combined styles
style :: [SimpleStyle] -> SimpleStyle
style = combineSimpleStyles

-- | Predefined button styles
buttonStyle :: SimpleStyle
buttonStyle = style
  [ backgroundColor lightGray
  , textColor black
  , padding 12
  , borderColor gray
  , borderWidth 1
  ]

primaryButtonStyle :: SimpleStyle
primaryButtonStyle = style
  [ backgroundColor blue
  , textColor white
  , padding 12
  ]

titleStyle :: SimpleStyle
titleStyle = style
  [ fontSize 24
  , textColor black
  ]

cardStyle :: SimpleStyle
cardStyle = style
  [ backgroundColor white
  , padding 16
  , borderColor lightGray
  , borderWidth 1
  ]