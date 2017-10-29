{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}

-- SDLIO Monad - State, IO...

module SDL.SDLSystem where

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
import SDL.SDLIO

import SDL.Fonts


-- lifting try into our monad - probably not needed, but was a good excersize in types
trySDLIO :: Exception e => SDLIO a -> SDLIO (Either e a)
trySDLIO act = do
    st <- get -- getting current state
    liftIO $ try $ evalStateT act st -- evaluating action act with state st (so goes into IO), trying it and lifting back to our monad 

-- convertSDLIO :: SDLIO a -> IO SDLState

-- main initialization functions
initStateIO :: IO SDLState
initStateIO = do r <- try $ do
                                SDL.initializeAll
                                window <- SDL.createWindow "My SDL Application" mainWindowSettings
                                renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
                                return $ SDLState {mainWindow = window, mainRenderer = renderer, loadedFonts = Map.empty, allLogs = []}
                 case r of
                    Left  e   -> print (e::SDLException) >> fail "Could not initialize SDL"
                    Right st -> (print $ show st) >> return st

-- action in our monad that wraps the IO actions: simply initializing the state in SDLIO monad, initializing fonts etc
initializeAll :: SDLIO ()
initializeAll = (liftIO initStateIO) >>= put >> initFonts