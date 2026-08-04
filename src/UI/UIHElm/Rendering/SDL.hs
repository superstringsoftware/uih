{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UI.UIHElm.Rendering.SDL where

import qualified SDL
import qualified SDL.Font as Font
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types (CInt)

-- Import our UIH-Elm types
import UI.UIHElm.Core.Types
import UI.UIHElm.Core.Widget

-- Import existing SDL infrastructure  
import qualified UI.Hatto.SDL.Common as Common
import qualified UI.Hatto.SDL.Fonts as Fonts
import qualified UI.Hatto.Widgets as Hatto (SDLState(..), MutState(..), newMutState, readMutState, updateMutState)
import qualified Color as UIH

-- | SDL-specific rendering state
data SDLRenderState = SDLRenderState
  { sdlWindow   :: SDL.Window
  , sdlRenderer :: SDL.Renderer
  , sdlFonts    :: Hatto.SDLState  -- Reuse existing font system
  } deriving Show

-- | Layout information for a positioned widget
data LayoutWidget msg = LayoutWidget
  { lwWidget :: Widget msg
  , lwRect   :: Rect
  }

-- | Convert UIH-Elm Color to SDL Color
colorToSDL :: Color -> SDL.V4 Word8
colorToSDL (Color r g b a) = SDL.V4 r g b a

-- | Convert UIH-Elm Rect to SDL Rectangle
rectToSDL :: Rect -> SDL.Rectangle CInt
rectToSDL (Rect x y w h) = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) 
                                         (SDL.V2 (fromIntegral w) (fromIntegral h))

-- | Initialize SDL for UIH-Elm  
initSDL :: Text -> Size -> IO SDLRenderState
initSDL windowTitle (Size width height) = do
  SDL.initializeAll
  Font.initialize
  
  -- Create window
  window <- SDL.createWindow windowTitle $ SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height) }
  SDL.showWindow window
  
  -- Create renderer with explicit settings
  renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedRenderer
    , SDL.rendererTargetTexture = False
    }
  
  -- Initialize fonts using existing system
  fontsState <- do
    let emptyState = Hatto.SDLState 
          { Hatto.mainWindow = window
          , Hatto.mainRenderer = renderer  
          , Hatto.loadedFonts = mempty
          , Hatto.scaleXY = SDL.V2 1.0 1.0
          , Hatto.autoScale = False
          , Hatto.defaultPixelFormat = undefined  -- Will be set by font init
          }
    fontsStateRef <- Hatto.newMutState emptyState
    Fonts.initFonts fontsStateRef
    Hatto.readMutState fontsStateRef
  
  return $ SDLRenderState window renderer fontsState

-- | Clean up SDL resources
cleanupSDL :: SDLRenderState -> IO ()
cleanupSDL SDLRenderState{..} = do
  -- TODO: Properly clean up fonts - need to keep reference to MutState
  SDL.destroyRenderer sdlRenderer
  SDL.destroyWindow sdlWindow
  SDL.quit

-- | Clear the screen with background color
clearScreen :: SDLRenderState -> Color -> IO ()
clearScreen SDLRenderState{..} bgColor = do
  SDL.rendererDrawColor sdlRenderer SDL.$= colorToSDL bgColor
  SDL.clear sdlRenderer

-- | Present the rendered frame
presentScreen :: SDLRenderState -> IO ()
presentScreen SDLRenderState{..} = SDL.present sdlRenderer

-- | Render a single widget at the given position
renderWidget :: SDLRenderState -> LayoutWidget msg -> IO ()
renderWidget state@SDLRenderState{..} (LayoutWidget widget rect) = do
  case widget of
    Text content style -> renderTextWidget state rect content style
    Button label _ style -> renderButtonWidget state rect label style
    Column _ _ -> return ()  -- Layout widgets don't render themselves
    Row _ _ -> return () 
    Spacer _ _ -> return ()  -- Spacers are invisible

-- | Render a text widget
renderTextWidget :: SDLRenderState -> Rect -> Text -> Style -> IO ()
renderTextWidget SDLRenderState{..} rect content style = do
  -- Get font from existing font system
  let fontSpec = maybe defaultFont id (styleFont style)
  let fontKey = (fontFamily fontSpec, fontSize fontSpec)
  
  fontRef <- Hatto.newMutState sdlFonts
  maybeFont <- Fonts.getFont fontRef fontKey
  
  case maybeFont of
    Nothing -> return ()  -- Skip if font not found
    Just font -> do
      -- Create text surface
      let textColor = maybe black id (styleForeground style)
      surface <- Font.blended font (colorToSDL textColor) content
      
      -- Create texture from surface
      texture <- SDL.createTextureFromSurface sdlRenderer surface
      SDL.freeSurface surface
      
      -- Render texture
      let sdlRect = rectToSDL rect
      SDL.copy sdlRenderer texture Nothing (Just sdlRect)
      SDL.destroyTexture texture

-- | Render a button widget  
renderButtonWidget :: SDLRenderState -> Rect -> Text -> Style -> IO ()
renderButtonWidget state@SDLRenderState{..} rect label style = do
  -- Draw button background
  let bgColor = maybe (Color 200 200 200 255) id (styleBackground style)
  SDL.rendererDrawColor sdlRenderer SDL.$= colorToSDL bgColor
  SDL.fillRect sdlRenderer (Just $ rectToSDL rect)
  
  -- Draw button border
  SDL.rendererDrawColor sdlRenderer SDL.$= colorToSDL black
  SDL.drawRect sdlRenderer (Just $ rectToSDL rect)
  
  -- Draw button text (centered)
  let textRect = Rect (rectX rect + 4) (rectY rect + 4) 
                      (rectWidth rect - 8) (rectHeight rect - 8)
  renderTextWidget state textRect label style

-- | Simple layout engine - calculates positions for widgets
calculateLayout :: Size -> Widget msg -> [LayoutWidget msg]
calculateLayout screenSize rootWidget = layoutWidget (Rect 0 0 (sizeWidth screenSize) (sizeHeight screenSize)) rootWidget

-- | Layout a single widget within the given bounds
layoutWidget :: Rect -> Widget msg -> [LayoutWidget msg]
layoutWidget bounds widget = case widget of
  Text _ _ -> [LayoutWidget widget bounds]
  Button _ _ _ -> [LayoutWidget widget bounds]
  Spacer _ _ -> [LayoutWidget widget bounds]
  
  Column children style -> layoutColumn bounds children style
  Row children style -> layoutRow bounds children style

-- | Layout column children vertically
layoutColumn :: Rect -> [Widget msg] -> Style -> [LayoutWidget msg]
layoutColumn (Rect x y w h) children _ = 
  let childHeight = if null children then 0 else h `div` length children
      childRects = [Rect x (y + i * childHeight) w childHeight | i <- [0..]]
      childLayouts = zipWith layoutWidget childRects children
  in concat childLayouts

-- | Layout row children horizontally
layoutRow :: Rect -> [Widget msg] -> Style -> [LayoutWidget msg] 
layoutRow (Rect x y w h) children _ =
  let childWidth = if null children then 0 else w `div` length children
      childRects = [Rect (x + i * childWidth) y childWidth h | i <- [0..]]
      childLayouts = zipWith layoutWidget childRects children
  in concat childLayouts

-- | Main rendering function - render a complete widget tree
renderWidgetTree :: SDLRenderState -> Size -> Widget msg -> IO ()
renderWidgetTree state screenSize rootWidget = do
  -- Clear screen
  clearScreen state white
  
  -- Calculate layout
  let layoutWidgets = calculateLayout screenSize rootWidget
  
  -- Render each positioned widget
  mapM_ (renderWidget state) layoutWidgets
  
  -- Present frame
  presentScreen state

-- | Simple test function
testRender :: IO ()
testRender = do
  state <- initSDL "UIH-Elm Test" (Size 800 600)
  
  let testWidget = column
        [ text "Hello UIH-Elm!"
        , row 
            [ button "Button 1" () `withStyle` buttonStyle
            , button "Button 2" () `withStyle` buttonStyle
            ]
        ]
  
  -- Render once
  renderWidgetTree state (Size 800 600) testWidget
  
  -- Wait a bit to see the result
  SDL.delay 2000
  
  cleanupSDL state