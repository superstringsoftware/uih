{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , TypeSynonymInstances
    , FlexibleInstances #-}

module UIH.SDL2.System where

-- MAIN ENTRY POINT TO SETTING UP THE RENDERING SYSTEM

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad (unless)

import SDL.Font
import Data.Text hiding (copy, any)
import Control.Exception
import SDL.Exception

import SDL hiding (initializeAll)

import UIH.SDL2.RenderMonad
import UIH.SDL2.Fonts

import UIH.UI.Widgets
import UIH.SDL2.RenderWidgets

import Color


initializeAll :: SDLIO ()
initializeAll = (liftIO initStateIO) >>= put >> initFonts

program = do
    dumpSDLState >> initializeAll >> dumpSDLState >> renderUI >> appLoop

-- hh
renderUI :: SDLIO ()    
renderUI = do
    renderer <- gets mainRenderer
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    rendererDrawColor renderer $= V4 255 255 255 0
    SDL.clear renderer
    renderScreen testButton
    SDL.present renderer

appLoop :: SDLIO ()
appLoop = do
    renderer <- gets mainRenderer
    events <- SDL.pollEvents -- get the events queue from SDL
    results <- mapM checkEvent events -- gather results
    let quit = any (== True) results -- checking if any of the results is True
    unless quit appLoop

-- returns True if need to quit, false otherwise
checkEvent :: SDL.Event -> SDLIO Bool
checkEvent event = do
    --liftIO $ print $ show $ event
    renderer <- gets mainRenderer
    case SDL.eventPayload event of
        SDL.WindowResizedEvent dt -> do
            let (SDL.V2 w h) = SDL.windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            renderUI --(fromIntegral w) (fromIntegral h)
            return False
        SDL.QuitEvent -> return True
        SDL.KeyboardEvent keyboardEvent -> return False
        SDL.TextInputEvent ev -> do
                                    liftIO $ print $ show ev
                                    return False
        _ -> return False