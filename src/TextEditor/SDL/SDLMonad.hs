{-# LANGUAGE OverloadedStrings, 
    TypeSynonymInstances, 
    FlexibleInstances,
    NoImplicitPrelude
 #-}
{-# LANGUAGE RecordWildCards #-}

module TextEditor.SDL.SDLMonad
where

import SDL hiding (Color)
import SDL.Font as SDL hiding (Color)
import Data.Map as Map hiding (map)
import Data.Text as T hiding (map)
import Prelude
import Foreign.C.Types (CFloat)
import Color
import Linear(V2)
import Control.Monad.MRWS
import qualified Data.Vector as V
import Foreign.C (CInt)

data SDLState = SDLState {
    mainWindow    :: SDL.Window
  , mainRenderer  :: SDL.Renderer
  , loadedFonts   :: Map.Map (Text, Int) SDL.Font -- map from font names and sizes to actual fonts
  , scaleXY       :: V2 CFloat -- in case we use highDPI, this will be the scale
  , autoScale     :: Bool -- apply scaling automatically so that same logical size is used on high dpi displays
  , defaultPixelFormat :: SDL.PixelFormat
} | SDLStateNone deriving Show

-- representation of the text window buffer
data TextWindowState = TextWindowState {
    currentLines :: V.Vector Text,
    logicalCursorPos :: V2 Int, -- row / column,
    currentFilePath :: Maybe Text -- if the file is saved, path    
} deriving Show

data TextStyle = TextStyle {
    fontName  :: Text
  , fontSize  :: !Int
  , fontColor :: Color
  , fontStyle :: [SDL.Style]
  , bgColor   :: Maybe Color
} deriving (Show, Eq)

defaultTextStyle = TextStyle {
        fontName = "Roboto",
        fontSize = 16,
        fontColor = mBlack,
        fontStyle = [],
        bgColor = Nothing
    }

type SDLUIT m a = MRWST [String] [String] SDLState m a

mainWindowSettings :: WindowConfig
mainWindowSettings = defaultWindow
  { windowBorder       = True
  -- There are issues with high DPI windows b/c we need to recalculate all coordinates when drawing / checking event
  -- coordinates, so its support is pending
  -- OpenGLContext defaultOpenGL
  , windowHighDPI      = True
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowGraphicsContext = OpenGLContext $ defaultOpenGL {
                                                              glColorPrecision = V4 8 8 8 0
                                                            , glDepthPrecision = 24
                                                            , glStencilPrecision = 8
                                                            , glMultisampleSamples = 1
                                                            , glProfile = Compatibility Debug 3 2
                                                          }
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 1200 800
  }
  
  -- checks if (x,y) is inside the rectangle (x',y',w,h)
isInsideRectangle :: V2 CInt -> V2 CInt -> V2 CInt -> Bool
isInsideRectangle (V2 x y) (V2 x' y') (V2 w h) = (x >= x') && (x <= (x' + w)) && (y >= y') && (y <= (y'+h))

isInsideWSkeleton :: V2 CInt -> WidgetSkeleton -> Bool
isInsideWSkeleton point WidgetSkeleton{..} = isInsideRectangle point pos size

{-
isInsideWidget :: V2 CInt -> Widget -> Bool
isInsideWidget point Widget{..} = if element == WENone then False else isInsideWSkeleton point (wsk element) 
-}
-- Used to keep track of the info common to all widgets - sizes, states etc
data WidgetSkeleton = WidgetSkeleton {
    wid :: Text
  , pos  :: V2 CInt
  , size :: V2 CInt
} deriving (Show, Eq)

emptySkeleton = WidgetSkeleton {
    wid = "",
    pos = V2 0 0, size = V2 0 0
}