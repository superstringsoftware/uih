{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.UIHElm.Core.Widget where

import Data.Text (Text)
import UI.UIHElm.Core.Types

-- | Text styling information for rich text
data TextStyle = TextStyle
  { textStyleFont       :: Maybe FontSpec
  , textStyleColor      :: Maybe Color  
  , textStyleBackground :: Maybe Color
  , textStyleBold       :: Bool
  , textStyleItalic     :: Bool
  , textStyleUnderline  :: Bool
  } deriving (Show, Eq)

-- | Rich text segment with individual styling
data RichTextSegment = RichTextSegment
  { segmentText  :: Text
  , segmentStyle :: TextStyle
  } deriving (Show, Eq)

-- | Core Widget type parameterized by message type
data Widget msg where
  -- Basic widgets
  Text   :: Text -> Style -> Widget msg
  Button :: Text -> msg -> Style -> Widget msg
  
  -- Enhanced text widgets
  TextBox    :: Text -> (Text -> msg) -> Style -> Widget msg  -- content, onChange, style
  TextArea   :: Text -> (Text -> msg) -> Int -> Style -> Widget msg  -- content, onChange, lines, style
  RichText   :: [RichTextSegment] -> Style -> Widget msg  -- segments, style
  
  -- Layout widgets  
  Column :: [Widget msg] -> Style -> Widget msg
  Row    :: [Widget msg] -> Style -> Widget msg
  
  -- Spacer for layout
  Spacer :: Int -> Int -> Widget msg  -- width height

-- | Style system
data Style = Style
  { stylePadding    :: Padding
  , styleBackground :: Maybe Color
  , styleForeground :: Maybe Color
  , styleFont       :: Maybe FontSpec
  , styleAlignment  :: (HAlign, VAlign)
  , styleMinSize    :: Maybe Size
  , styleMaxSize    :: Maybe Size
  } deriving (Show, Eq)

-- | Default style
defaultStyle :: Style
defaultStyle = Style
  { stylePadding = noPadding
  , styleBackground = Nothing
  , styleForeground = Just black
  , styleFont = Just defaultFont
  , styleAlignment = (HAlignLeft, VAlignTop)
  , styleMinSize = Nothing
  , styleMaxSize = Nothing
  }

-- | Convenient style variations
centerStyle :: Style
centerStyle = defaultStyle { styleAlignment = (HAlignCenter, VAlignCenter) }

buttonStyle :: Style
buttonStyle = defaultStyle 
  { stylePadding = Padding 8 16 8 16
  , styleBackground = Just (Color 200 200 200 255)
  , styleAlignment = (HAlignCenter, VAlignCenter)
  }

titleStyle :: Style  
titleStyle = defaultStyle
  { styleFont = Just (FontSpec "Roboto-Light" 24)
  , styleAlignment = (HAlignCenter, VAlignTop)
  }

-- | Text input styles
textBoxStyle :: Style
textBoxStyle = defaultStyle
  { stylePadding = Padding 8 12 8 12
  , styleBackground = Just (Color 255 255 255 255)  -- White background
  , styleForeground = Just (Color 0 0 0 255)        -- Black text
  , styleAlignment = (HAlignLeft, VAlignCenter)
  }

textAreaStyle :: Style
textAreaStyle = defaultStyle
  { stylePadding = Padding 8 12 8 12
  , styleBackground = Just (Color 255 255 255 255)  -- White background
  , styleForeground = Just (Color 0 0 0 255)        -- Black text
  , styleAlignment = (HAlignLeft, VAlignTop)
  }

-- | Default text style for rich text segments
defaultTextStyle :: TextStyle
defaultTextStyle = TextStyle
  { textStyleFont = Just defaultFont
  , textStyleColor = Just black
  , textStyleBackground = Nothing
  , textStyleBold = False
  , textStyleItalic = False
  , textStyleUnderline = False
  }

-- | Widget constructors for convenience
text :: Text -> Widget msg
text content = Text content defaultStyle

button :: Text -> msg -> Widget msg
button label msg = Button label msg buttonStyle

-- | Text input widgets
textBox :: Text -> (Text -> msg) -> Widget msg
textBox content onChange = TextBox content onChange textBoxStyle

textArea :: Text -> (Text -> msg) -> Int -> Widget msg  
textArea content onChange lines = TextArea content onChange lines textAreaStyle

-- | Rich text helpers
richText :: [RichTextSegment] -> Widget msg
richText segments = RichText segments defaultStyle

-- | Create rich text segment with styling
segment :: Text -> TextStyle -> RichTextSegment
segment = RichTextSegment

-- | Text style builders
bold :: TextStyle -> TextStyle
bold style = style { textStyleBold = True }

italic :: TextStyle -> TextStyle  
italic style = style { textStyleItalic = True }

underline :: TextStyle -> TextStyle
underline style = style { textStyleUnderline = True }

withTextColor :: Color -> TextStyle -> TextStyle
withTextColor color style = style { textStyleColor = Just color }

withTextBackground :: Color -> TextStyle -> TextStyle
withTextBackground color style = style { textStyleBackground = Just color }

withTextFont :: FontSpec -> TextStyle -> TextStyle
withTextFont font style = style { textStyleFont = Just font }

-- | Layout widgets
column :: [Widget msg] -> Widget msg
column children = Column children defaultStyle

row :: [Widget msg] -> Widget msg
row children = Row children defaultStyle

spacer :: Int -> Int -> Widget msg
spacer w h = Spacer w h

-- | Helper function to apply styles
withStyle :: Widget msg -> Style -> Widget msg
withStyle widget newStyle = case widget of
  Text content _ -> Text content newStyle
  Button label msg _ -> Button label msg newStyle
  TextBox content onChange _ -> TextBox content onChange newStyle
  TextArea content onChange lines _ -> TextArea content onChange lines newStyle
  RichText segments _ -> RichText segments newStyle
  Column children _ -> Column children newStyle
  Row children _ -> Row children newStyle
  Spacer w h -> Spacer w h  -- Spacer ignores style