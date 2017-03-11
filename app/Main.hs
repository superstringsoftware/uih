{-# LANGUAGE OverloadedStrings #-}
module Main where

import Logger
import SDLText

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
  TTF.init

  window <- createWindow "My SDL Application" mainWindow
  renderer <- createRenderer window (-1) defaultRenderer

  rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 10
  let rect = Rectangle (P (V2 10 10)) (V2 600 600)
  -- fillRect renderer (Just rect)

  font <- defaultFont 32
  textTexture <- createTextTexture "some text -- hello world!" (Raw.Color 127 11 255 0) font renderer
  renderTexture 10 10 textTexture renderer

  present renderer

  appLoop renderer
  TTF.quit
  destroyTexture textTexture

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


  unless qPressed (appLoop renderer)

















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
