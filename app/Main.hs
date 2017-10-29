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
import SDL.SDLSystem

import qualified SDL.Primitive as GFX -- sdl2-gfx, seems to work, performance is a question

import Control.Monad.Trans.State.Strict 
import Control.Monad.IO.Class (liftIO)
--import Control.Monad.Trans (lift)

import SDL.InputText

program :: SDLIO ()
program = do 
    dumpSDLState >> initializeAll >> dumpSDLState 
    st <- get
    let renderer = mainRenderer st
    renderUI 1200 800
    appLoop

runProgram :: SDLIO a -> IO a
runProgram prog = evalStateT prog SDLEmptyState

main :: IO ()
main = do
  putStrLn "Starting main..."

  --fls <- listDirectory "/Library/Fonts"
  --mapM_ print fls
  runProgram program
  


appLoop :: SDLIO ()
appLoop = do
  renderer <- gets mainRenderer
  events <- liftIO SDL.pollEvents -- get the events queue from SDL
  results <- mapM checkEvent events -- gather results
  let quit = any (== True) results -- checking if any of the results is True
  unless quit appLoop

renderUI :: Int -> Int -> SDLIO ()
renderUI w h = do
    renderer <- gets mainRenderer
    setRenderDrawColorRGBA renderer $ mdWhite
    -- putStrLn "Inside renderUI: set color"
    -- rendererDrawColor renderer $= V4 0 0 0 0
    SDL.clear renderer
    -- putStrLn "Cleared renderer"
    mapM_ renderGlobal (fullUI w h)
    renderGlobal testBox

    -- putStrLn "Mapped all rendering actions"
    SDL.present renderer
    -- putStrLn "Presented renderer"

-- returns True if need to quit, false otherwise
checkEvent :: SDL.Event -> SDLIO Bool
checkEvent event = do
    --liftIO $ print $ show $ event
    renderer <- gets mainRenderer
    case SDL.eventPayload event of
        SDL.WindowResizedEvent dt -> do
            let (V2 w h) = SDL.windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            renderUI (fromIntegral w) (fromIntegral h)
            return False
        SDL.QuitEvent -> return True
        SDL.KeyboardEvent keyboardEvent -> return False
        SDL.TextInputEvent ev -> do 
                                    liftIO $ print $ show ev
                                    return False
        _ -> return False






