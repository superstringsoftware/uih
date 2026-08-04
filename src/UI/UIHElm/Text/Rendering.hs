{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module UI.UIHElm.Text.Rendering where

import qualified SDL
import qualified SDL.Font as Font
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (try, SomeException)

import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget (TextStyle(..))
import UI.UIHElm.Text.Layout
import UI.UIHElm.Rendering.Simple (SimpleRenderState(..), colorToSDL, rectToSDL)

-- ============================================================================
-- Font management  
-- ============================================================================

-- | Font cache for managing loaded fonts
type FontCache = [(FontSpec, Font.Font)]

-- | Load or retrieve font from cache
getFont :: FontCache -> FontSpec -> IO (FontCache, Font.Font)
getFont cache fontSpec@(FontSpec _ size) = 
  case lookup fontSpec cache of
    Just font -> return (cache, font)
    Nothing -> do
      -- Try to load the font
      result <- try @SomeException $ loadFontWithSize size
      case result of
        Left _ -> do
          -- Fallback to default font if loading fails
          defaultFont <- loadFontWithSize size
          let newCache = (fontSpec, defaultFont) : cache
          return (newCache, defaultFont)
        Right font -> do
          let newCache = (fontSpec, font) : cache
          return (newCache, font)

-- | Load font with specific size
loadFontWithSize :: Int -> IO Font.Font
loadFontWithSize size = do
  -- Try different font paths in order of preference
  let fontPaths = 
        [ "./fonts/Roboto/Roboto-Light.ttf"  -- Project font
        , "/System/Library/Fonts/Arial.ttf"  -- macOS
        , "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"  -- Linux
        , "C:/Windows/Fonts/arial.ttf"  -- Windows
        ]
  
  tryLoadFont fontPaths
  where
    tryLoadFont [] = error "No suitable font found! Please ensure you have a font file available."
    tryLoadFont (path:paths) = do
      result <- try @SomeException $ Font.load path size
      case result of
        Left _ -> tryLoadFont paths
        Right font -> return font

-- ============================================================================
-- Accurate text measurement using SDL_ttf
-- ============================================================================

-- | Measure text accurately using SDL_ttf
measureTextWithFont :: Font.Font -> Text -> IO TextMetrics
measureTextWithFont font text = do
  if T.null text 
    then return $ TextMetrics 0 0 0 0 0
    else do
      -- Get text size using SDL_ttf
      (width, height) <- Font.size font text
      
      -- Get font metrics
      fontHeight <- Font.height font
      fontAscent <- Font.ascent font
      fontDescent <- Font.descent font
      
      return $ TextMetrics
        { metricsWidth = width
        , metricsHeight = height
        , metricsAscent = fontAscent
        , metricsDescent = abs fontDescent  -- Make positive
        , metricsAdvance = width
        }

-- | Measure a styled text segment accurately
measureStyledSegment :: FontCache -> StyledTextSegment -> IO (FontCache, TextMetrics)
measureStyledSegment cache segment = do
  let style = segmentStyle segment
      content = segmentContent segment
      fontSpec = maybe (FontSpec "Arial" 16) id (textStyleFont style)
  
  (newCache, font) <- getFont cache fontSpec
  metrics <- measureTextWithFont font content
  return (newCache, metrics)

-- ============================================================================
-- Advanced line breaking with accurate measurements
-- ============================================================================

-- | Break text into lines with accurate width calculations
breakTextIntoLines :: FontCache -> LayoutConstraints -> [StyledTextSegment] -> IO (FontCache, [[StyledTextSegment]])
breakTextIntoLines cache constraints segments = do
  case constraintsWrapMode constraints of
    NoWrap -> return (cache, [segments])
    WordWrap -> breakWithAccurateWordWrap cache constraints segments
    CharWrap -> breakWithAccurateCharWrap cache constraints segments

-- | Word wrap with accurate text measurement
breakWithAccurateWordWrap :: FontCache -> LayoutConstraints -> [StyledTextSegment] -> IO (FontCache, [[StyledTextSegment]])
breakWithAccurateWordWrap cache constraints segments = do
  let maxWidth = rectWidth $ constraintsBounds constraints
  
  -- First handle explicit newlines
  explicitLineSegments <- expandNewlines segments
  
  -- Then word-wrap each line
  (finalCache, wrappedLines) <- foldM (wrapLine maxWidth) (cache, []) explicitLineSegments
  
  return (finalCache, reverse wrappedLines)
  where
    -- Handle explicit newlines in segments
    expandNewlines :: [StyledTextSegment] -> IO [[StyledTextSegment]]
    expandNewlines segs = return $ concatMap expandSegmentNewlines segs
    
    expandSegmentNewlines :: StyledTextSegment -> [[StyledTextSegment]]
    expandSegmentNewlines segment =
      let lines = T.splitOn "\n" (segmentContent segment)
          style = segmentStyle segment
      in map (\line -> [StyledTextSegment line style]) lines
    
    -- Wrap a single logical line  
    wrapLine :: Int -> (FontCache, [[StyledTextSegment]]) -> [StyledTextSegment] -> IO (FontCache, [[StyledTextSegment]])
    wrapLine maxWidth (currentCache, acc) lineSegments = do
      (newCache, wrappedSegments) <- wrapSingleLine currentCache maxWidth lineSegments
      return (newCache, wrappedSegments : acc)

-- | Wrap segments within a single line based on actual width
wrapSingleLine :: FontCache -> Int -> [StyledTextSegment] -> IO (FontCache, [StyledTextSegment])
wrapSingleLine cache maxWidth segments = do
  -- For now, simplified implementation
  -- TODO: Implement proper word boundary detection and width accumulation
  return (cache, segments)

-- | Character wrap with accurate measurement
breakWithAccurateCharWrap :: FontCache -> LayoutConstraints -> [StyledTextSegment] -> IO (FontCache, [[StyledTextSegment]])
breakWithAccurateCharWrap cache constraints segments = do
  -- Simplified implementation for now
  return (cache, [segments])

-- ============================================================================
-- Accurate text layout
-- ============================================================================

-- | Layout styled text with accurate measurements
layoutStyledTextAccurate :: FontCache -> LayoutConstraints -> [StyledTextSegment] -> IO (FontCache, TextLayout)
layoutStyledTextAccurate cache constraints segments = do
  -- Break into lines with accurate measurements
  (cache1, lineSegments) <- breakTextIntoLines cache constraints segments
  
  -- Layout each line accurately
  let bounds = constraintsBounds constraints
      startY = rectY bounds
  (cache2, lines) <- layoutLinesAccurate cache1 startY lineSegments
  
  -- Calculate total bounds
  let totalHeight = sum (map lineHeight lines)
      totalBounds = Rect (rectX bounds) startY (rectWidth bounds) totalHeight
      primaryBaseline = if null lines then startY else lineBaseline (head lines)
  
  let layout = TextLayout
        { layoutLines = lines
        , layoutBounds = totalBounds
        , layoutBaseline = primaryBaseline
        }
  
  return (cache2, layout)

-- | Layout multiple lines with accurate measurements
layoutLinesAccurate :: FontCache -> Int -> [[StyledTextSegment]] -> IO (FontCache, [TextLine])
layoutLinesAccurate cache _ [] = return (cache, [])
layoutLinesAccurate cache currentY (segments:restLines) = do
  -- Layout this line
  (cache1, line) <- layoutSingleLineAccurate cache currentY segments
  
  -- Layout remaining lines
  let nextY = currentY + lineHeight line + 4  -- Add some line spacing
  (cache2, restLayoutLines) <- layoutLinesAccurate cache1 nextY restLines
  
  return (cache2, line : restLayoutLines)

-- | Layout a single line with accurate measurements
layoutSingleLineAccurate :: FontCache -> Int -> [StyledTextSegment] -> IO (FontCache, TextLine)
layoutSingleLineAccurate cache y segments = do
  -- Layout fragments with accurate positioning
  (cache1, fragments) <- layoutFragmentsAccurate cache 0 y segments
  
  -- Calculate line metrics
  let lineWidth = sum (map (rectWidth . fragmentRect) fragments)
      lineHeight = if null fragments then 0 else maximum (map (rectHeight . fragmentRect) fragments)
      lineBaseline = y + (if null fragments then 0 else maximum (map fragmentBaseline fragments))
      lineRect = Rect 0 y lineWidth lineHeight
  
  let line = TextLine
        { lineFragments = fragments
        , lineRect = lineRect
        , lineHeight = lineHeight
        , lineBaseline = lineBaseline
        }
  
  return (cache1, line)

-- | Layout fragments with accurate measurements
layoutFragmentsAccurate :: FontCache -> Int -> Int -> [StyledTextSegment] -> IO (FontCache, [TextFragment])
layoutFragmentsAccurate cache _ _ [] = return (cache, [])
layoutFragmentsAccurate cache currentX y (segment:rest) = do
  -- Measure this segment accurately
  (cache1, metrics) <- measureStyledSegment cache segment
  
  -- Create fragment
  let fragmentRect = Rect currentX y (metricsWidth metrics) (metricsHeight metrics)
      fragment = TextFragment
        { fragmentText = segmentContent segment
        , fragmentStyle = segmentStyle segment
        , fragmentRect = fragmentRect
        , fragmentBaseline = y + metricsAscent metrics
        }
  
  -- Layout remaining fragments
  let nextX = currentX + metricsWidth metrics
  (cache2, restFragments) <- layoutFragmentsAccurate cache1 nextX y rest
  
  return (cache2, fragment : restFragments)

-- ============================================================================
-- Fragment rendering
-- ============================================================================

-- | Render a text layout using accurate positioning
renderTextLayout :: SimpleRenderState -> FontCache -> TextLayout -> IO FontCache
renderTextLayout state cache layout = do
  foldM (renderTextLine state) cache (layoutLines layout)

-- | Render a single text line
renderTextLine :: SimpleRenderState -> FontCache -> TextLine -> IO FontCache
renderTextLine state cache line = do
  foldM (renderTextFragment state) cache (lineFragments line)

-- | Render a single text fragment with accurate positioning
renderTextFragment :: SimpleRenderState -> FontCache -> TextFragment -> IO FontCache
renderTextFragment state@SimpleRenderState{..} cache fragment = do
  let style = fragmentStyle fragment
      content = fragmentText fragment
      rect = fragmentRect fragment
      
  -- Skip empty fragments
  if T.null content
    then return cache
    else do
      -- Get font for this fragment
      let fontSpec = maybe (FontSpec "Arial" 16) id (textStyleFont style)
      (newCache, font) <- getFont cache fontSpec
      
      -- Render background if specified
      case textStyleBackground style of
        Just bgColor -> do
          SDL.rendererDrawColor srRenderer SDL.$= colorToSDL bgColor
          SDL.fillRect srRenderer (Just $ rectToSDL rect)
        Nothing -> return ()
      
      -- Get text color
      let textColor = case textStyleColor style of
            Just color -> colorToSDL color
            Nothing -> SDL.V4 0 0 0 255  -- Default black
      
      -- Create text surface
      surface <- Font.blended font textColor content
      
      -- Create texture from surface
      texture <- SDL.createTextureFromSurface srRenderer surface
      SDL.freeSurface surface
      
      -- Render texture at exact position
      SDL.copy srRenderer texture Nothing (Just $ rectToSDL rect)
      SDL.destroyTexture texture
      
      return newCache

-- ============================================================================
-- Utility functions for integration
-- ============================================================================

-- | Render simple text with proper layout
renderTextWithLayout :: SimpleRenderState -> FontCache -> Rect -> Text -> TextStyle -> IO FontCache
renderTextWithLayout state cache bounds content style = do
  let segment = StyledTextSegment content style
      constraints = LayoutConstraints
        { constraintsBounds = bounds
        , constraintsWrapMode = WordWrap
        , constraintsAlignment = AlignLeft
        , constraintsLineSpacing = 1.2
        }
  
  (newCache, layout) <- layoutStyledTextAccurate cache constraints [segment]
  finalCache <- renderTextLayout state newCache layout
  return finalCache

-- | Render rich text with proper layout
renderRichTextWithLayout :: SimpleRenderState -> FontCache -> Rect -> [StyledTextSegment] -> IO FontCache
renderRichTextWithLayout state cache bounds segments = do
  let constraints = LayoutConstraints
        { constraintsBounds = bounds
        , constraintsWrapMode = WordWrap
        , constraintsAlignment = AlignLeft
        , constraintsLineSpacing = 1.2
        }
  
  (newCache, layout) <- layoutStyledTextAccurate cache constraints segments
  finalCache <- renderTextLayout state newCache layout
  return finalCache