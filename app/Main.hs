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
import qualified SDL as SDL
import Linear (V4(..))
import Control.Monad (unless)
import SDL.Vect

import SDL.Fonts as TTF

import Screen.RawWidgets
import Screen.TestUI

import System.Directory

import SDL.SDLIO

import qualified SDL.Primitive as GFX -- sdl2-gfx, seems to work, performance is a question

import Control.Monad.Trans.State.Strict 
import Control.Monad.IO.Class (liftIO)
--import Control.Monad.Trans (lift)

program :: SDLIO ()
program = do 
    initializeAll >> TTF.initFonts
    st <- get
    let renderer = mainRenderer st
    liftIO $ renderUI 1200 800 renderer
    liftIO $ appLoop renderer

runProgram :: SDLIO a -> IO a
runProgram prog = evalStateT prog SDLEmptyState

main :: IO ()
main = do
  putStrLn "Starting main..."

  --fls <- listDirectory "/Library/Fonts"
  --mapM_ print fls
  runProgram program
  


appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  events <- SDL.pollEvents -- get the events queue from SDL
  results <- mapM (checkEvent renderer) events -- gather results
  let quit = any (== True) results -- checking if any of the results is True
  unless quit (appLoop renderer)

renderUI :: Int -> Int -> SDL.Renderer -> IO ()
renderUI w h renderer = do
    setRenderDrawColorRGBA renderer $ mdWhite
    putStrLn "Inside renderUI: set color"
    -- rendererDrawColor renderer $= V4 0 0 0 0
    SDL.clear renderer
    putStrLn "Cleared renderer"
    mapM_ (renderGlobal renderer) (fullUI w h)
    putStrLn "Mapped all rendering actions"
    SDL.present renderer
    putStrLn "Presented renderer"

-- returns True if need to quit, false otherwise
checkEvent :: SDL.Renderer -> SDL.Event -> IO Bool
checkEvent renderer event = do
    case SDL.eventPayload event of
        SDL.WindowResizedEvent dt -> do
            let (V2 w h) = SDL.windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            renderUI (fromIntegral w) (fromIntegral h) renderer
            return False
        SDL.QuitEvent -> return True
        SDL.KeyboardEvent keyboardEvent -> do
            return $ SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ -> return False








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
