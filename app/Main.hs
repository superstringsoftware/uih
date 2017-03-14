{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main where

import Logger
import SDL.Bindings
import Color
import CSS.Box

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import System.Exit

import qualified SDL.Raw as Raw hiding (free)
import SDL as SDL
import Linear (V4(..))
import Control.Monad (unless)
import SDL.Vect

import SDL.TTF as TTF

import Screen.RawWidgets
import Screen.TestUI

import qualified SDL.Primitive as GFX -- sdl2-gfx, seems to work, performance is a question

mainWindow :: WindowConfig
mainWindow = WindowConfig
  { windowBorder       = True
  , windowHighDPI      = True
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowOpenGL       = Nothing
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 2400 1800
  }

arial :: String
arial = "/home/aantich/dev/dropbox/Haskell/uih/ARIAL.TTF"

main :: IO ()
main = do
  initializeAll
  -- TTF.init

  window <- createWindow "My SDL Application" mainWindow
  renderer <- createRenderer window (-1) defaultRenderer

  renderUI 2400 1800 renderer

  appLoop renderer
  TTF.quit
  -- destroyTexture textTexture

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  mapM_ (checkEvent renderer) events
  unless qPressed (appLoop renderer)

renderUI :: Int -> Int -> Renderer -> IO ()
renderUI w h renderer = do
    setRenderDrawColorRGBA renderer $ mdWhite
    -- rendererDrawColor renderer $= V4 0 0 0 0
    clear renderer
    mapM_ (renderGlobal renderer) (fullUI w h)
    present renderer

checkEvent renderer event = do
    case eventPayload event of
        WindowResizedEvent dt -> do
            let (V2 w h) = windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            renderUI (fromIntegral w) (fromIntegral h) renderer
        _ -> return ()








{-
font <- defaultFont 64
textTexture <- createTextTexture "some text -- hello world!" (mdGrey 900) font renderer
renderTexture 520 520 textTexture renderer

GFX.circle renderer (V2 800 1200) 200 (V4 200 50 50 255)
GFX.smoothCircle renderer (V2 1500 1200) 300 (V4 200 50 50 255)
-}





{-

handleEvent ev = do
    case (eventType ev) of
        SDL_WINDOWEVENT -> putStrLn "window event"
        SDL_QUIT -> putStrLn "quitting..." >> exitWith ExitSuccess
        _ -> return ()

eventLoop = do
    evPtr <- malloc :: IO (Ptr SDL.Event)
    pres <- SDL.pollEvent evPtr
    ev <- peek evPtr
    if (pres == 1)
        then handleEvent ev >> free evPtr >> eventLoop
        else free evPtr >> eventLoop


oldmain :: IO ()
oldmain = do
    SDL.init SDL_INIT_VIDEO
    SDL.glSetAttribute SDL_GL_CONTEXT_PROFILE_MASK  SDL_GL_CONTEXT_PROFILE_CORE
    SDL.glSetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 3
    SDL.glSetAttribute SDL_GL_CONTEXT_MINOR_VERSION 2
    SDL.glSetAttribute SDL_GL_STENCIL_SIZE 8
    winName <- newCString "OpenGL"
    window <- SDL.createWindow winName 300 300 2400 1800 SDL_WINDOW_OPENGL
    context <- SDL.glCreateContext window
    surface <- SDL.getWindowSurface window

    gRenderer <- SDL.createRenderer window (-1) SDL_RENDERER_ACCELERATED
    SDL.setRenderDrawColor gRenderer 0xFF 0xFF 0xFF 0xFF
    SDL.renderClear gRenderer
    SDL.renderPresent gRenderer

    eventLoop


    return ()

    -}
