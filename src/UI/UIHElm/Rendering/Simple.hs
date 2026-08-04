{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module UI.UIHElm.Rendering.Simple where

import qualified SDL
import qualified SDL.Font as Font
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import Foreign.C.Types (CInt)
import Control.Exception (try, SomeException)

-- Import our UIH-Elm types
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget
import UI.UIHElm.Events.Events (LayoutWidget(..))

-- | Simple SDL rendering state with direct font loading
data SimpleRenderState = SimpleRenderState
  { srWindow   :: SDL.Window
  , srRenderer :: SDL.Renderer
  , srFont     :: Font.Font  -- Single font for simplicity
  } deriving Show

-- | Convert UIH-Elm Color to SDL Color
colorToSDL :: Color -> SDL.V4 Word8
colorToSDL (Color r g b a) = SDL.V4 r g b a

-- | Convert UIH-Elm Rect to SDL Rectangle
rectToSDL :: Rect -> SDL.Rectangle CInt
rectToSDL (Rect x y w h) = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) 
                                         (SDL.V2 (fromIntegral w) (fromIntegral h))

-- | Initialize simple SDL rendering
initSimpleSDL :: Text -> Size -> IO (Either String SimpleRenderState)
initSimpleSDL windowTitle (Size width height) = do
  result <- try @SomeException $ do
    SDL.initializeAll
    Font.initialize
    
    -- Create window
    window <- SDL.createWindow windowTitle $ SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height) }
    SDL.showWindow window
    
    -- Create renderer
    renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedRenderer
      , SDL.rendererTargetTexture = False
      }
    
    -- Load a simple font - try system font first, then fallback
    font <- loadDefaultFont
    
    return $ SimpleRenderState window renderer font
  
  case result of
    Left ex -> return $ Left $ "SDL initialization failed: " ++ show ex
    Right state -> return $ Right state

-- | Try to load a default font with fallbacks
loadDefaultFont :: IO Font.Font
loadDefaultFont = do
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
      result <- try @SomeException $ Font.load path 16
      case result of
        Left _ -> tryLoadFont paths
        Right font -> return font

-- | Clean up simple SDL resources
cleanupSimpleSDL :: SimpleRenderState -> IO ()
cleanupSimpleSDL SimpleRenderState{..} = do
  Font.free srFont
  SDL.destroyRenderer srRenderer
  SDL.destroyWindow srWindow
  SDL.quit

-- | Clear the screen
clearScreen :: SimpleRenderState -> Color -> IO ()
clearScreen SimpleRenderState{..} bgColor = do
  SDL.rendererDrawColor srRenderer SDL.$= colorToSDL bgColor
  SDL.clear srRenderer

-- | Present the rendered frame
presentScreen :: SimpleRenderState -> IO ()
presentScreen SimpleRenderState{..} = SDL.present srRenderer

-- | Render a single widget
renderWidget :: SimpleRenderState -> LayoutWidget msg -> IO ()
renderWidget state@SimpleRenderState{..} layoutWidget = do
  let widget = lwWidget layoutWidget
  let rect = lwRect layoutWidget
  case widget of
    Text content style -> renderTextWithStyle state rect content style
    Button label _ style -> renderButtonWithStyle state rect label style
    TextBox content _ style -> renderTextBoxWithStyle state rect content style
    TextArea content _ _ style -> renderTextAreaWithStyle state rect content style
    RichText segments style -> renderRichTextWithStyle state rect segments style
    Column _ _ -> return ()  -- Layout widgets don't render themselves
    Row _ _ -> return () 
    Spacer _ _ -> return ()  -- Spacers are invisible

-- | Render text with style support and proper line handling
renderTextWithStyle :: SimpleRenderState -> Rect -> Text -> Style -> IO ()
renderTextWithStyle state@SimpleRenderState{..} rect content style = do
  if T.null content 
    then return ()
    else do
      -- Draw background if specified
      case styleBackground style of
        Just bgColor -> do
          SDL.rendererDrawColor srRenderer SDL.$= colorToSDL bgColor
          SDL.fillRect srRenderer (Just $ rectToSDL rect)
        Nothing -> return ()
      
      -- Split text into lines and apply word wrapping
      let explicitLines = T.splitOn "\n" content
      wrappedLines <- mapM (wrapTextLine state rect) explicitLines
      let allLines = concat wrappedLines
      renderTextLines state rect allLines style

-- | Wrap a single line of text to fit within the given width
wrapTextLine :: SimpleRenderState -> Rect -> Text -> IO [Text]
wrapTextLine SimpleRenderState{..} rect content = do
  if T.null content
    then return [""]
    else do
      let availableWidth = rectWidth rect
          words = T.words content
      
      if null words
        then return [""]
        else wrapWords availableWidth words []
  where
    wrapWords :: Int -> [Text] -> [Text] -> IO [Text]
    wrapWords _ [] currentLine = return [T.unwords (reverse currentLine)]
    wrapWords availableWidth (word:restWords) currentLine = do
      let testLine = if null currentLine 
                     then word
                     else T.unwords (reverse (word:currentLine))
      
      -- Measure the test line
      (lineWidth, _) <- Font.size srFont testLine
      
      if lineWidth <= availableWidth || null currentLine
        then -- Word fits on current line (or it's the first word)
          wrapWords availableWidth restWords (word:currentLine)
        else do -- Start a new line
          let completedLine = if null currentLine then word else T.unwords (reverse currentLine)
          restLines <- wrapWords availableWidth (word:restWords) []
          return (completedLine : restLines)

-- | Render multiple lines of text
renderTextLines :: SimpleRenderState -> Rect -> [Text] -> Style -> IO ()
renderTextLines state@SimpleRenderState{..} rect textLines style = do
  -- Get actual font metrics for proper line height
  fontHeight <- Font.height srFont
  let lineHeight = fontHeight + 4  -- Add some line spacing
      startY = rectY rect
      textColor = case styleForeground style of
        Just color -> colorToSDL color
        Nothing -> SDL.V4 0 0 0 255  -- Default black
  
  -- Render each line with proper spacing
  mapM_ (renderSingleLine state textColor lineHeight) (zip [0..] textLines)
  where
    renderSingleLine :: SimpleRenderState -> SDL.V4 Word8 -> Int -> (Int, Text) -> IO ()
    renderSingleLine SimpleRenderState{..} color lineHeight (lineIndex, lineText) = do
      if T.null lineText
        then return ()  -- Skip empty lines
        else do
          let lineY = rectY rect + lineIndex * lineHeight
              lineRect = Rect (rectX rect) lineY (rectWidth rect) lineHeight
          
          -- Create text surface for this line
          surface <- Font.blended srFont color lineText
          
          -- Create texture from surface
          texture <- SDL.createTextureFromSurface srRenderer surface
          SDL.freeSurface surface
          
          -- Get texture size for positioning
          textureInfo <- SDL.queryTexture texture
          let textW = fromIntegral $ SDL.textureWidth textureInfo
          let textH = fromIntegral $ SDL.textureHeight textureInfo
          
          -- Position text according to style alignment
          let (finalX, finalY) = case styleAlignment style of
                (HAlignLeft, VAlignTop) -> (rectX rect, lineY)
                (HAlignCenter, VAlignTop) -> (rectX rect + (rectWidth rect - textW) `div` 2, lineY)
                (HAlignRight, VAlignTop) -> (rectX rect + rectWidth rect - textW, lineY)
                (HAlignLeft, VAlignCenter) -> (rectX rect, lineY + (lineHeight - textH) `div` 2)
                (HAlignCenter, VAlignCenter) -> (rectX rect + (rectWidth rect - textW) `div` 2, lineY + (lineHeight - textH) `div` 2)
                (HAlignRight, VAlignCenter) -> (rectX rect + rectWidth rect - textW, lineY + (lineHeight - textH) `div` 2)
                _ -> (rectX rect, lineY)  -- Fallback to top-left
          
          let finalRect = Rect finalX finalY textW textH
          
          -- Render texture
          SDL.copy srRenderer texture Nothing (Just $ rectToSDL finalRect)
          SDL.destroyTexture texture

-- | Render text directly (legacy function)
renderText :: SimpleRenderState -> Rect -> Text -> IO ()
renderText state rect content = renderTextWithStyle state rect content defaultStyle

-- | Render a button with style support
renderButtonWithStyle :: SimpleRenderState -> Rect -> Text -> Style -> IO ()
renderButtonWithStyle state@SimpleRenderState{..} rect label style = do
  -- Get button background color from style, default to light gray
  let bgColor = case styleBackground style of
        Just color -> colorToSDL color
        Nothing -> SDL.V4 220 220 220 255  -- Default light gray
  
  -- Draw button background
  SDL.rendererDrawColor srRenderer SDL.$= bgColor
  SDL.fillRect srRenderer (Just $ rectToSDL rect)
  
  -- Draw button border
  SDL.rendererDrawColor srRenderer SDL.$= SDL.V4 100 100 100 255  -- Dark gray
  SDL.drawRect srRenderer (Just $ rectToSDL rect)
  
  -- Draw button text with style
  renderTextWithStyle state rect label style

-- | Render a button with background and text (legacy function)
renderButton :: SimpleRenderState -> Rect -> Text -> IO ()
renderButton state rect label = renderButtonWithStyle state rect label buttonStyle

-- | Render a text box (single-line input) with style support
renderTextBoxWithStyle :: SimpleRenderState -> Rect -> Text -> Style -> IO ()
renderTextBoxWithStyle state@SimpleRenderState{..} rect content style = do
  -- Get background color from style, default to white
  let bgColor = case styleBackground style of
        Just color -> colorToSDL color
        Nothing -> SDL.V4 255 255 255 255  -- Default white
  
  -- Draw text box background
  SDL.rendererDrawColor srRenderer SDL.$= bgColor
  SDL.fillRect srRenderer (Just $ rectToSDL rect)
  
  -- Draw text box border
  SDL.rendererDrawColor srRenderer SDL.$= SDL.V4 128 128 128 255  -- Gray border
  SDL.drawRect srRenderer (Just $ rectToSDL rect)
  
  -- Draw text content with cursor (simplified - no actual cursor for now)
  renderTextWithStyle state rect content style

-- | Render a text area (multi-line input) with style support  
renderTextAreaWithStyle :: SimpleRenderState -> Rect -> Text -> Style -> IO ()
renderTextAreaWithStyle state@SimpleRenderState{..} rect content style = do
  -- Get background color from style, default to white
  let bgColor = case styleBackground style of
        Just color -> colorToSDL color
        Nothing -> SDL.V4 255 255 255 255  -- Default white
  
  -- Draw text area background
  SDL.rendererDrawColor srRenderer SDL.$= bgColor
  SDL.fillRect srRenderer (Just $ rectToSDL rect)
  
  -- Draw text area border
  SDL.rendererDrawColor srRenderer SDL.$= SDL.V4 128 128 128 255  -- Gray border
  SDL.drawRect srRenderer (Just $ rectToSDL rect)
  
  -- Draw text content (simplified - no line wrapping for now)
  renderTextWithStyle state rect content style

-- | Render rich text segments with natural flow layout
renderRichTextWithStyle :: SimpleRenderState -> Rect -> [RichTextSegment] -> Style -> IO ()
renderRichTextWithStyle state@SimpleRenderState{..} rect segments style = do
  -- Draw container background if specified
  case styleBackground style of
    Just bgColor -> do
      SDL.rendererDrawColor srRenderer SDL.$= colorToSDL bgColor
      SDL.fillRect srRenderer (Just $ rectToSDL rect)
    Nothing -> return ()
  
  -- Layout segments with natural flow (measure each segment's actual width)
  renderRichTextSegmentsFlow state rect segments

-- | Render rich text segments with proper flowing layout
renderRichTextSegmentsFlow :: SimpleRenderState -> Rect -> [RichTextSegment] -> IO ()
renderRichTextSegmentsFlow state@SimpleRenderState{..} rect segments = do
  -- Get proper line height from font metrics
  fontHeight <- Font.height srFont
  let lineHeight = fontHeight + 4  -- Add some line spacing
  
  -- Layout segments from left to right, wrapping to new lines as needed
  layoutSegments lineHeight (rectX rect) (rectY rect) segments
  where
    layoutSegments :: Int -> Int -> Int -> [RichTextSegment] -> IO ()
    layoutSegments _ _ _ [] = return ()
    layoutSegments lineH currentX currentY (segment:restSegments) = do
      -- Measure this segment
      let content = segmentText segment
      if T.null content
        then layoutSegments lineH currentX currentY restSegments  -- Skip empty segments
        else do
          -- Get text color for this segment
          let segStyle = segmentStyle segment
              textColor = case textStyleColor segStyle of
                Just color -> colorToSDL color
                Nothing -> SDL.V4 0 0 0 255  -- Default black
          
          -- Create surface to measure the text
          surface <- Font.blended srFont textColor content
          texture <- SDL.createTextureFromSurface srRenderer surface
          SDL.freeSurface surface
          
          -- Get actual text dimensions
          textureInfo <- SDL.queryTexture texture
          let segmentWidth = fromIntegral $ SDL.textureWidth textureInfo
              segmentHeight = fromIntegral $ SDL.textureHeight textureInfo
          
          -- Check if segment fits on current line
          let availableWidth = rectX rect + rectWidth rect - currentX
          let (finalX, finalY, nextX, nextY) = 
                if segmentWidth <= availableWidth || currentX == rectX rect
                  then -- Fits on current line (or is first segment on line)
                    ( currentX
                    , currentY
                    , currentX + segmentWidth
                    , currentY
                    )
                  else -- Move to next line
                    ( rectX rect
                    , currentY + lineH  -- Use proper line height
                    , rectX rect + segmentWidth
                    , currentY + lineH
                    )
          
          -- Draw background for this segment if specified
          case textStyleBackground segStyle of
            Just bgColor -> do
              SDL.rendererDrawColor srRenderer SDL.$= colorToSDL bgColor
              SDL.fillRect srRenderer (Just $ rectToSDL $ Rect finalX finalY segmentWidth segmentHeight)
            Nothing -> return ()
          
          -- Render the segment
          let segmentRect = Rect finalX finalY segmentWidth segmentHeight
          SDL.copy srRenderer texture Nothing (Just $ rectToSDL segmentRect)
          SDL.destroyTexture texture
          
          -- Continue with remaining segments
          layoutSegments lineH nextX nextY restSegments

-- | Render a single rich text segment
renderRichTextSegment :: SimpleRenderState -> Rect -> RichTextSegment -> IO ()
renderRichTextSegment state@SimpleRenderState{..} rect segment = do
  let content = segmentText segment
  let textStyle = segmentStyle segment
  
  -- Convert TextStyle to Style for rendering
  let style = Style
        { stylePadding = noPadding
        , styleBackground = textStyleBackground textStyle
        , styleForeground = textStyleColor textStyle
        , styleFont = textStyleFont textStyle
        , styleAlignment = (HAlignLeft, VAlignCenter)
        , styleMinSize = Nothing
        , styleMaxSize = Nothing
        }
  
  renderTextWithStyle state rect content style

-- | Calculate layout (same as before)
calculateLayout :: Size -> Widget msg -> [LayoutWidget msg]
calculateLayout screenSize rootWidget = layoutWidget (Rect 0 0 (sizeWidth screenSize) (sizeHeight screenSize)) rootWidget

layoutWidget :: Rect -> Widget msg -> [LayoutWidget msg]
layoutWidget bounds widget = case widget of
  Text _ _ -> [LayoutWidget widget bounds]
  Button _ _ _ -> [LayoutWidget widget bounds]
  TextBox _ _ _ -> [LayoutWidget widget bounds]
  TextArea _ _ _ _ -> [LayoutWidget widget bounds]
  RichText _ _ -> [LayoutWidget widget bounds]
  Spacer _ _ -> [LayoutWidget widget bounds]
  
  Column children style -> layoutColumn bounds children style
  Row children style -> layoutRow bounds children style

layoutColumn :: Rect -> [Widget msg] -> Style -> [LayoutWidget msg]
layoutColumn (Rect x y w h) children _ = 
  let childHeight = if null children then 0 else h `div` length children
      childRects = [Rect x (y + i * childHeight) w childHeight | i <- [0..]]
      childLayouts = zipWith layoutWidget childRects children
  in concat childLayouts

layoutRow :: Rect -> [Widget msg] -> Style -> [LayoutWidget msg] 
layoutRow (Rect x y w h) children _ =
  let childWidth = if null children then 0 else w `div` length children
      childRects = [Rect (x + i * childWidth) y childWidth h | i <- [0..]]
      childLayouts = zipWith layoutWidget childRects children
  in concat childLayouts

-- | Main rendering function
renderWidgetTree :: SimpleRenderState -> Size -> Widget msg -> IO ()
renderWidgetTree state screenSize rootWidget = do
  -- Clear screen
  clearScreen state white
  
  -- Calculate layout
  let layoutWidgets = calculateLayout screenSize rootWidget
  
  -- Render each positioned widget
  mapM_ (renderWidget state) layoutWidgets
  
  -- Present frame
  presentScreen state