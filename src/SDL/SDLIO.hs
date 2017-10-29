{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}

-- SDLIO Monad - State, IO...

module SDL.SDLIO where

import Control.Monad.Trans.State.Strict 
import Control.Monad.IO.Class (liftIO)

import Control.Exception

import qualified SDL.Raw as Raw

import qualified SDL as SDL
import SDL.Internal.Types
import SDL.Vect
import SDL.Font
--import SDL.Video

import Color
import CSS.Box

import GHC.Prim

import Screen.RawWidgets

import Data.Text
import SDL.Exception

import qualified Data.Map.Strict as Map

-- record to keep our current SDL subsystem state
data SDLState = SDLState {
    mainWindow :: Window
  , mainRenderer :: Renderer
  , loadedFonts :: Map.Map Text Font -- map from font names to actual fonts
  , allLogs :: [Text]
} | SDLEmptyState deriving Show

-- Stacking State and IO into a monad
type SDLIO = StateT SDLState IO

-- needs to be read from config!
mainWindowSettings = SDL.WindowConfig
  { windowBorder       = True
  , windowHighDPI      = True
  , windowInputGrabbed = False
  , windowMode         = SDL.Windowed
  , windowOpenGL       = Nothing
  , windowPosition     = SDL.Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 1200 800
  }


dumpSDLState :: SDLIO ()
dumpSDLState = get >>= liftIO . print . show



