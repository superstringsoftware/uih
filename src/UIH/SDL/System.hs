{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}

module UIH.SDL.System where

import UIH.SDL.SDLIO
import UIH.SDL.Fonts

import SDL.Exception
import Control.Exception

import SDL as SDL hiding (get)
import qualified Data.Map.Strict as Map

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)

import Data.Word



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
                                renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
                                return $ SDLState {
                                    mainWindow = window,
                                    mainRenderer = renderer,
                                    loadedFonts = Map.empty,
                                    allLogs = [],
                                    cursor = CursorStatus 0 0 (V4 255 255 255 0) 0,
                                    bgColor = V4 210 210 210 0,
                                    cursorTimer = Nothing
                                  }
                 case r of
                    Left  e   -> print (e::SDLException) >> fail "Could not initialize SDL"
                    Right st -> (print $ show st) >> return st

-- action in our monad that wraps the IO actions: simply initializing the state in SDLIO monad, initializing fonts etc
initializeAll :: SDLIO ()
initializeAll = (liftIO initStateIO) >>= put >> initFonts

-- draw cursor: if true, then cursor color, if false - background color
drawCursor :: Bool -> SDLIO ()
drawCursor flag = do
  st <- get
  let cur = cursor st
  let ren = mainRenderer st
  let clr = if flag then color cur else bgColor st
  rendererRenderTarget ren $= Nothing
  rendererDrawColor ren $= clr
  drawLine ren (P $ V2 (x cur) (y cur)) (P $ V2 (x cur) (y cur + height cur))

drawCursorRaw x y h clr1 clr2 ren ms = do
  rendererRenderTarget ren $= Nothing
  rendererDrawColor ren $= clr1
  drawLine ren (P $ V2 x y) (P $ V2 x (y + h) )
  print "Calling timer white"
  delay ms
  rendererRenderTarget ren $= Nothing
  rendererDrawColor ren $= clr2
  drawLine ren (P $ V2 x y) (P $ V2 x (y + h) )
  print "Calling timer black"
  return $ Reschedule ms


cursorOn :: SDLIO ()
cursorOn = do
  st <- get
  let ct = cursorTimer st
  let cur = cursor st
  let ren = mainRenderer st
  case ct of (Just _) -> return ()
             Nothing -> do
                nt <- addTimer 500 (drawCursorRaw (x cur) (y cur) (height cur) (color cur) (bgColor st) ren )
                modify' (\st -> st {cursorTimer = Just nt})
