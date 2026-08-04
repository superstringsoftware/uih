{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module UI.UIHElm.Styling.Stylesheet where

import Data.Text (Text)
import qualified Data.Map as Map
import Data.Word (Word8)
import UI.UIHElm.Core.Types

-- | Enhanced color with opacity and gradients
data StyleColor 
  = Solid Color                           -- rgba(255, 0, 0, 255)
  | Transparent                           -- transparent
  | Gradient GradientType [Color]         -- linear/radial gradients
  deriving (Show, Eq)

data GradientType = Linear | Radial
  deriving (Show, Eq)

-- | Typography properties
data Typography = Typography
  { typographyFontFamily :: Text                    -- "Roboto", "Arial", etc.
  , typographyFontSize   :: Int                     -- 16px
  , typographyFontWeight :: FontWeight              -- normal, bold, etc.
  , typographyFontStyle  :: FontStyle               -- normal, italic
  , typographyLineHeight :: Maybe Float             -- 1.4 (multiplier)
  , typographyLetterSpacing :: Maybe Int            -- 2px
  } deriving (Show, Eq)

data FontWeight = NormalWeight | Bold | Light | ExtraBold
  deriving (Show, Eq)

data FontStyle = NormalStyle | Italic | Oblique
  deriving (Show, Eq)

-- | Border properties
data Border = Border
  { borderWidth :: BorderWidth           -- 1px, 2px all, etc.
  , borderStyle :: BorderStyle           -- solid, dashed, etc.
  , borderColor :: StyleColor            -- color of border
  , borderRadius :: BorderRadius         -- rounded corners
  } deriving (Show, Eq)

data BorderWidth 
  = AllSides Int                         -- border: 1px
  | Individual Int Int Int Int           -- top right bottom left
  deriving (Show, Eq)

data BorderStyle = SolidBorder | Dashed | Dotted | None
  deriving (Show, Eq)

data BorderRadius
  = UniformRadius Int                    -- border-radius: 5px
  | CornerRadius Int Int Int Int         -- top-left, top-right, bottom-right, bottom-left
  deriving (Show, Eq)

-- | Shadow effects
data Shadow = Shadow
  { shadowOffsetX :: Int                 -- horizontal offset
  , shadowOffsetY :: Int                 -- vertical offset  
  , shadowBlur    :: Int                 -- blur radius
  , shadowSpread  :: Int                 -- spread radius
  , shadowColor   :: StyleColor          -- shadow color
  } deriving (Show, Eq)

-- | Animation and transition properties
data Transition = Transition
  { transitionProperty :: Text           -- "background-color", "all"
  , transitionDuration :: Int            -- milliseconds
  , transitionEasing   :: EasingFunction -- ease-in, ease-out, etc.
  } deriving (Show, Eq)

data EasingFunction = EaseIn | EaseOut | EaseInOut | LinearEasing
  deriving (Show, Eq)

-- | Layout and sizing
data Sizing = Sizing
  { sizingWidth     :: SizeValue
  , sizingHeight    :: SizeValue
  , sizingMinWidth  :: Maybe Int
  , sizingMaxWidth  :: Maybe Int
  , sizingMinHeight :: Maybe Int
  , sizingMaxHeight :: Maybe Int
  } deriving (Show, Eq)

data SizeValue 
  = Auto                                 -- auto
  | Pixels Int                           -- 100px
  | Percent Float                        -- 50%
  | Fill                                 -- fill available space
  deriving (Show, Eq)

-- | Flexbox-style layout
data Layout = Layout
  { flexDirection   :: FlexDirection     -- row, column
  , justifyContent  :: JustifyContent    -- flex-start, center, etc.
  , alignItems      :: AlignItems        -- flex-start, center, etc.
  , flexWrap        :: FlexWrap          -- nowrap, wrap
  , gap             :: Maybe Int         -- space between items
  } deriving (Show, Eq)

data FlexDirection = Row | Column | RowReverse | ColumnReverse
  deriving (Show, Eq)

data JustifyContent = FlexStart | FlexEnd | Center | SpaceBetween | SpaceAround | SpaceEvenly
  deriving (Show, Eq)

data AlignItems = AlignStart | AlignEnd | AlignCenter | Stretch | Baseline
  deriving (Show, Eq)

data FlexWrap = NoWrap | Wrap | WrapReverse
  deriving (Show, Eq)

-- | Complete style definition
data StyleRule = StyleRule
  { background    :: Maybe StyleColor
  , foreground    :: Maybe StyleColor
  , typography    :: Maybe Typography
  , border        :: Maybe Border
  , shadow        :: Maybe Shadow
  , padding       :: Maybe Padding
  , margin        :: Maybe Padding      -- reuse Padding type
  , sizing        :: Maybe Sizing
  , layout        :: Maybe Layout
  , transitions   :: [Transition]
  , opacity       :: Maybe Float       -- 0.0 to 1.0
  , cursor        :: Maybe CursorType
  } deriving (Show, Eq)

data CursorType = DefaultCursor | Pointer | Text | NotAllowed
  deriving (Show, Eq)

-- | Widget states for interactive styling
data WidgetState = NormalState | Hover | Active | Focus | Disabled
  deriving (Show, Eq, Ord)

-- | Style with state variations
data StatefulStyle = StatefulStyle
  { baseStyle    :: StyleRule           -- default appearance
  , stateStyles  :: Map.Map WidgetState StyleRule  -- state-specific overrides
  } deriving (Show, Eq)

-- | Theme system
data Theme = Theme
  { themeName     :: Text
  , themeColors   :: Map.Map Text StyleColor  -- color palette
  , themeTypography :: Map.Map Text Typography  -- typography scales
  , themeSpacing  :: Map.Map Text Int         -- spacing scale
  , themeShadows  :: Map.Map Text Shadow      -- shadow presets
  } deriving (Show, Eq)

-- | Stylesheet - collection of named styles
data Stylesheet = Stylesheet
  { stylesheetTheme  :: Maybe Theme
  , stylesheetStyles :: Map.Map Text StatefulStyle
  } deriving (Show, Eq)

-- | Default values and helpers
defaultTypography :: Typography
defaultTypography = Typography
  { typographyFontFamily = "Arial"
  , typographyFontSize = 16
  , typographyFontWeight = NormalWeight
  , typographyFontStyle = NormalStyle
  , typographyLineHeight = Nothing
  , typographyLetterSpacing = Nothing
  }

defaultBorder :: Border
defaultBorder = Border
  { borderWidth = AllSides 1
  , borderStyle = SolidBorder
  , borderColor = Solid (Color 128 128 128 255)
  , borderRadius = UniformRadius 0
  }

defaultStyle :: StyleRule
defaultStyle = StyleRule
  { background = Nothing
  , foreground = Just $ Solid (Color 0 0 0 255)  -- black
  , typography = Just defaultTypography
  , border = Nothing
  , shadow = Nothing
  , padding = Nothing
  , margin = Nothing
  , sizing = Nothing
  , layout = Nothing
  , transitions = []
  , opacity = Nothing
  , cursor = Nothing
  }

-- | Smart constructors for common styles
solidBackground :: Color -> StyleRule
solidBackground color = defaultStyle { background = Just $ Solid color }

textColor :: Color -> StyleRule  
textColor color = defaultStyle { foreground = Just $ Solid color }


roundedCorners :: Int -> StyleRule
roundedCorners radius = defaultStyle 
  { border = Just $ defaultBorder { borderRadius = UniformRadius radius } }

-- | Style combinators
(<+>) :: StyleRule -> StyleRule -> StyleRule
style1 <+> style2 = StyleRule
  { background = background style2 <|> background style1
  , foreground = foreground style2 <|> foreground style1
  , typography = mergeTypography (typography style1) (typography style2)
  , border = border style2 <|> border style1
  , shadow = shadow style2 <|> shadow style1
  , padding = padding style2 <|> padding style1
  , margin = margin style2 <|> margin style1
  , sizing = sizing style2 <|> sizing style1
  , layout = layout style2 <|> layout style1
  , transitions = transitions style1 ++ transitions style2
  , opacity = opacity style2 <|> opacity style1
  , cursor = cursor style2 <|> cursor style1
  }
  where
    (<|>) = \case
      Nothing -> id
      Just x -> const (Just x)
    
    mergeTypography Nothing t2 = t2
    mergeTypography t1 Nothing = t1
    mergeTypography (Just t1) (Just t2) = Just $ t2  -- t2 overrides t1

-- | Helper to create stateful styles
stateful :: StyleRule -> [(WidgetState, StyleRule)] -> StatefulStyle
stateful base states = StatefulStyle base (Map.fromList states)

-- | Get style for specific state
getStyleForState :: WidgetState -> StatefulStyle -> StyleRule
getStyleForState state StatefulStyle{..} = 
  case Map.lookup state stateStyles of
    Nothing -> baseStyle
    Just stateStyle -> baseStyle <+> stateStyle