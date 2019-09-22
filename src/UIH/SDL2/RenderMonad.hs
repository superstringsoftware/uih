{-# LANGUAGE OverloadedStrings #-}
module UIH.SDL2.RenderMonad where

-- this is a huge state monad taking care of low-level SDL interactions

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import SDL as SDL hiding (get)
import SDL.Font
import Data.Text
import Foreign.C.Types (CInt)
import Data.Word
import qualified Data.Map.Strict as Map

-- record to store current position etc of the cursor
data CursorStatus = CursorStatus {
      x      :: CInt
    , y      :: CInt
    , color  :: V4 Word8
    , height :: CInt
    , cursorTimer :: Maybe Timer
} deriving Show

-- record to keep our current SDL subsystem state
data SDLState = SDLState {
    mainWindow    :: Window
  , mainRenderer  :: Renderer
  , loadedFonts   :: Map.Map Text Font -- map from font names to actual fonts
  , cursor        :: CursorStatus
  , bgColor       :: V4 Word8  
} | SDLEmptyState deriving Show

runSDLIO :: SDLIO a -> IO a
runSDLIO prog = evalStateT prog SDLEmptyState


getRenderer :: SDLIO Renderer
getRenderer = mainRenderer <$> get

instance Show Timer where
  show _ = "Timer present"

-- Stacking State and IO into a monad
type SDLIO = StateT SDLState IO

-- needs to be read from config!
mainWindowSettings = defaultWindow
  { windowBorder       = True
  -- There are issues with high DPI windows b/c we need to recalculate all coordinates when drawing / checking event
  -- coordinates, so its support is pending
  , windowHighDPI      = False 
  , windowInputGrabbed = False
  , windowMode         = Windowed
  --, windowOpenGL       = Nothing
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 1200 800
  }


dumpSDLState :: SDLIO ()
dumpSDLState = get >>= liftIO . print . show

-- main initialization functions
initStateIO :: IO SDLState
initStateIO = do 
    r <- try $ do
            SDL.initializeAll
            window <- SDL.createWindow "My SDL Application" mainWindowSettings
            renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
            return $ SDLState {
                mainWindow = window,
                mainRenderer = renderer,
                loadedFonts = Map.empty,
                cursor = CursorStatus 0 0 (V4 255 255 255 0) 0 Nothing,
                bgColor = V4 210 210 210 0                
                }
    either (\e -> print (e::SDLException) >> fail "Could not initialize SDL")
           (\st -> (print $ show st) >> return st) r
                 