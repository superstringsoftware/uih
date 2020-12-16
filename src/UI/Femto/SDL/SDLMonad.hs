{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
    RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, FlexibleInstances #-}
module UI.Femto.SDL.SDLMonad where

-- | this is a huge state monad taking care of low-level SDL interactions
-- overall idea after several iterations is as follows:
-- we will have at least 2 levels:
-- * very low level of Raw.Widgets that (proobably) don't handle any state, but are only used for RENDERING
-- * middle-level widgets that are much more convenient for defining the UI programmatically,
--   we will store them in the monad as well, events will be handled by them (so they will have behavior), 
--   and they will COMPILE to the low level widgets as needed (ONLY WHEN CHANGED!!) which will then be rendered.
--
-- So the workflow on the high level is:
-- 1) define middleware widgets that describe the ui
-- 2) they get registered in the monad with corresponding *pure* handlers
-- 3) they get compiled to low level widgets
-- 4) event loop starts
-- 5) as middleware widgets change when responding to events, we recompile corresponding low-level widgets,
--      but not the whole tree.


import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception ( try )
import SDL hiding (get, Event)
import SDL.Font hiding (height)
import Data.Text
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Control.Concurrent (threadDelay)

import Color

import PreludeFixes

-- record to keep our current SDL subsystem state
data SDLState = SDLState {
    mainWindow    :: Window
  , mainRenderer  :: Renderer
  , loadedFonts   :: Map.Map (Text, Int) Font -- map from font names and sizes to actual fonts
  , bgColor       :: V4 Word8
  , rawIdCounter  :: !Int
  , scaleXY       :: V2 CFloat -- in case we use highDPI, this will be the scale
  , autoScale     :: Bool -- apply scaling automatically so that same logical size is used on high dpi displays
  , defaultPixelFormat :: PixelFormat
} | SDLEmptyState deriving Show

-- instance Show (FemtoUIM u ()) where show _ = "FemtoUIM u () action"

-- Stacking State and IO into a monad with potential user state
type FemtoUIM = StateT SDLState IO



quickEvalSDLIO :: FemtoUIM a -> IO a
quickEvalSDLIO act = evalStateT act SDLEmptyState

getRenderer :: FemtoUIM Renderer
getRenderer = mainRenderer <$> get


