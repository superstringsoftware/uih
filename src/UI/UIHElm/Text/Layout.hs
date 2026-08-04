{-# LANGUAGE OverloadedStrings #-}

module UI.UIHElm.Text.Layout where

import Data.Text (Text)
import qualified Data.Text as T
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget (TextStyle(..))

-- | A positioned text fragment with styling ready for rendering
data TextFragment = TextFragment
  { fragmentText     :: Text           -- The actual text content
  , fragmentStyle    :: TextStyle      -- Styling information
  , fragmentRect     :: Rect           -- Position and size on screen
  , fragmentBaseline :: Int            -- Y offset for baseline alignment
  } deriving (Show, Eq)

-- | A line of text composed of multiple fragments
data TextLine = TextLine
  { lineFragments :: [TextFragment]    -- Text fragments in this line
  , lineRect      :: Rect              -- Bounding rectangle of entire line
  , lineHeight    :: Int               -- Height of this line
  , lineBaseline  :: Int               -- Baseline Y position
  } deriving (Show, Eq)

-- | Text layout result
data TextLayout = TextLayout
  { layoutLines    :: [TextLine]       -- All lines in the layout
  , layoutBounds   :: Rect             -- Total bounding rectangle
  , layoutBaseline :: Int              -- Primary baseline position
  } deriving (Show, Eq)

-- | Text measurement information
data TextMetrics = TextMetrics
  { metricsWidth    :: Int             -- Width in pixels
  , metricsHeight   :: Int             -- Height in pixels  
  , metricsAscent   :: Int             -- Height above baseline
  , metricsDescent  :: Int             -- Height below baseline
  , metricsAdvance  :: Int             -- Horizontal advance for cursor
  } deriving (Show, Eq)

-- | Text wrapping behavior
data WrapMode 
  = NoWrap                             -- No wrapping, single line
  | WordWrap                           -- Wrap at word boundaries
  | CharWrap                           -- Wrap at any character
  deriving (Show, Eq)

-- | Text alignment within available space
data TextAlign
  = AlignLeft
  | AlignCenter  
  | AlignRight
  | AlignJustify
  deriving (Show, Eq)

-- | Layout constraints for text
data LayoutConstraints = LayoutConstraints
  { constraintsBounds    :: Rect       -- Available space
  , constraintsWrapMode  :: WrapMode   -- How to wrap text
  , constraintsAlignment :: TextAlign  -- How to align lines
  , constraintsLineSpacing :: Float    -- Extra spacing between lines
  } deriving (Show, Eq)

-- | Segment of styled text input for layout
data StyledTextSegment = StyledTextSegment
  { segmentContent :: Text
  , segmentStyle   :: TextStyle
  } deriving (Show, Eq)

-- ============================================================================
-- Core text measurement functions (placeholders for now)
-- ============================================================================

-- | Measure a single text segment with given style
-- TODO: Implement actual font measurement using SDL_ttf
measureTextSegment :: StyledTextSegment -> TextMetrics
measureTextSegment segment = 
  let content = segmentContent segment
      style = segmentStyle segment
      -- Simplified measurement - in reality we'd use SDL_ttf font metrics
      charCount = T.length content
      fontSize = maybe 16 (\(FontSpec _ size) -> size) (textStyleFont style)
  in TextMetrics
    { metricsWidth = charCount * (fontSize `div` 2)  -- Rough approximation
    , metricsHeight = fontSize
    , metricsAscent = fontSize * 3 `div` 4
    , metricsDescent = fontSize `div` 4  
    , metricsAdvance = charCount * (fontSize `div` 2)
    }

-- | Break text into words for word wrapping
breakIntoWords :: Text -> [Text]
breakIntoWords text = T.words text

-- | Break text into individual characters for character wrapping  
breakIntoChars :: Text -> [Text]
breakIntoChars text = map T.singleton (T.unpack text)

-- | Split text by newlines
splitLines :: Text -> [Text]
splitLines = T.splitOn "\n"

-- ============================================================================
-- Line breaking algorithm
-- ============================================================================

-- | Break styled text segments into lines that fit within width constraint
breakIntoLines :: LayoutConstraints -> [StyledTextSegment] -> [[StyledTextSegment]]
breakIntoLines constraints segments = 
  case constraintsWrapMode constraints of
    NoWrap -> [segments]  -- Single line, may overflow
    WordWrap -> breakWithWordWrap (rectWidth $ constraintsBounds constraints) segments
    CharWrap -> breakWithCharWrap (rectWidth $ constraintsBounds constraints) segments

-- | Word-wrap algorithm
breakWithWordWrap :: Int -> [StyledTextSegment] -> [[StyledTextSegment]]
breakWithWordWrap maxWidth segments = 
  let
    -- First, handle explicit line breaks
    explicitLines = concatMap splitSegmentByNewlines segments
    
    -- Then word-wrap each line
    wordWrappedLines = map (wrapWordsInLine maxWidth) explicitLines
  in
    wordWrappedLines
  where
    -- Split segments that contain newlines into multiple segments
    splitSegmentByNewlines :: StyledTextSegment -> [[StyledTextSegment]]
    splitSegmentByNewlines segment =
      let lines = splitLines (segmentContent segment)
          style = segmentStyle segment
      in map (\line -> [StyledTextSegment line style]) lines
    
    -- Wrap words within a single logical line
    wrapWordsInLine :: Int -> [StyledTextSegment] -> [StyledTextSegment]
    wrapWordsInLine width segs =
      -- For now, simplified - just return the segments as-is
      -- TODO: Implement proper word wrapping with width calculation
      segs

-- | Character-wrap algorithm (simpler fallback)
breakWithCharWrap :: Int -> [StyledTextSegment] -> [[StyledTextSegment]]
breakWithCharWrap maxWidth segments = 
  -- Simplified implementation
  [segments]

-- ============================================================================  
-- Layout algorithm
-- ============================================================================

-- | Layout styled text segments into positioned fragments
layoutStyledText :: LayoutConstraints -> [StyledTextSegment] -> TextLayout
layoutStyledText constraints segments =
  let
    -- Break into lines
    lineSegments = breakIntoLines constraints segments
    
    -- Layout each line
    bounds = constraintsBounds constraints
    startY = rectY bounds
    lines = layoutLines' startY lineSegments
    
    -- Calculate total bounds
    totalHeight = sum (map lineHeight lines)
    totalBounds = Rect (rectX bounds) startY (rectWidth bounds) totalHeight
    
  in TextLayout
    { layoutLines = lines
    , layoutBounds = totalBounds  
    , layoutBaseline = startY + (if null lines then 0 else lineBaseline (head lines))
    }
  where
    layoutLines' :: Int -> [[StyledTextSegment]] -> [TextLine]
    layoutLines' _ [] = []
    layoutLines' currentY (segments:restLines) =
      let
        line = layoutSingleLine constraints currentY segments
        nextY = currentY + lineHeight line + round (constraintsLineSpacing constraints)
      in
        line : layoutLines' nextY restLines

-- | Layout segments within a single line
layoutSingleLine :: LayoutConstraints -> Int -> [StyledTextSegment] -> TextLine
layoutSingleLine constraints y segments =
  let
    bounds = constraintsBounds constraints
    fragments = layoutFragments (rectX bounds) y segments
    
    -- Calculate line metrics
    lineWidth = sum (map (rectWidth . fragmentRect) fragments)
    lineHeight = if null fragments then 0 else maximum (map (rectHeight . fragmentRect) fragments)
    lineBaseline = y + (lineHeight * 3 `div` 4)  -- Simplified baseline calculation
    
    -- Apply alignment
    alignedFragments = applyAlignment (constraintsAlignment constraints) bounds lineWidth fragments
    
    lineRect = Rect (rectX bounds) y lineWidth lineHeight
    
  in TextLine
    { lineFragments = alignedFragments
    , lineRect = lineRect
    , lineHeight = lineHeight
    , lineBaseline = lineBaseline
    }

-- | Layout individual text fragments within a line
layoutFragments :: Int -> Int -> [StyledTextSegment] -> [TextFragment]
layoutFragments startX y segments = 
  layoutFragments' startX segments
  where
    layoutFragments' :: Int -> [StyledTextSegment] -> [TextFragment]
    layoutFragments' _ [] = []
    layoutFragments' currentX (segment:rest) =
      let
        metrics = measureTextSegment segment
        fragmentRect = Rect currentX y (metricsWidth metrics) (metricsHeight metrics)
        fragment = TextFragment
          { fragmentText = segmentContent segment
          , fragmentStyle = segmentStyle segment  
          , fragmentRect = fragmentRect
          , fragmentBaseline = y + metricsAscent metrics
          }
        nextX = currentX + metricsWidth metrics
      in
        fragment : layoutFragments' nextX rest

-- | Apply text alignment to fragments within a line
applyAlignment :: TextAlign -> Rect -> Int -> [TextFragment] -> [TextFragment]
applyAlignment alignment bounds lineWidth fragments =
  case alignment of
    AlignLeft -> fragments  -- Already left-aligned
    AlignCenter -> 
      let offset = (rectWidth bounds - lineWidth) `div` 2
      in map (offsetFragment offset) fragments
    AlignRight ->
      let offset = rectWidth bounds - lineWidth  
      in map (offsetFragment offset) fragments
    AlignJustify -> fragments  -- TODO: Implement justify
  where
    offsetFragment :: Int -> TextFragment -> TextFragment
    offsetFragment offset fragment =
      let rect = fragmentRect fragment
          newRect = rect { rectX = rectX rect + offset }
      in fragment { fragmentRect = newRect }

-- ============================================================================
-- Utility functions
-- ============================================================================

-- | Create layout constraints from a bounding rectangle
simpleConstraints :: Rect -> LayoutConstraints
simpleConstraints bounds = LayoutConstraints
  { constraintsBounds = bounds
  , constraintsWrapMode = WordWrap
  , constraintsAlignment = AlignLeft
  , constraintsLineSpacing = 1.2
  }

-- | Layout simple text with default styling
layoutSimpleText :: Rect -> TextStyle -> Text -> TextLayout
layoutSimpleText bounds style content =
  let
    segment = StyledTextSegment content style
    constraints = simpleConstraints bounds
  in
    layoutStyledText constraints [segment]