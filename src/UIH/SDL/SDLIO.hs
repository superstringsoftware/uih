{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleInstances  #-}

-- SDLIO Monad - State, IO...

module UIH.SDL.SDLIO where

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)

-- import Control.Exception

-- import qualified SDL.Raw as Raw

import SDL as SDL hiding (get)
-- import SDL.Internal.Types
-- import SDL.Vect
import SDL.Font
--import SDL.Video

import Color

-- import GHC.Prim
import Data.Text
-- import SDL.Exception

import Foreign.C.Types (CInt)
import Data.Word

import qualified Data.Map.Strict as Map

-- record to store current position etc of the cursor
data CursorStatus = CursorStatus {
      x      :: CInt
    , y      :: CInt
    , color  :: V4 Word8
    , height :: CInt
} deriving Show

-- record to keep our current SDL subsystem state
data SDLState = SDLState {
    mainWindow    :: Window
  , mainRenderer  :: Renderer
  , loadedFonts   :: Map.Map Text Font -- map from font names to actual fonts
  , allLogs       :: [Text]
  , cursor        :: CursorStatus
  , bgColor       :: V4 Word8
  , cursorTimer   :: Maybe Timer
} | SDLEmptyState deriving Show

instance Show Timer where
  show _ = "Timer present"

-- Stacking State and IO into a monad
type SDLIO = StateT SDLState IO

-- needs to be read from config!
mainWindowSettings = WindowConfig
  { windowBorder       = True
  , windowHighDPI      = True
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowOpenGL       = Nothing
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 1200 800
  }


dumpSDLState :: SDLIO ()
dumpSDLState = get >>= liftIO . print . show
