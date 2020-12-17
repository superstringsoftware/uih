{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, TypeSynonymInstances, RecordWildCards, ScopedTypeVariables #-}

module Main where

import Color
import Linear
import Data.Text hiding (any)

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO, MonadIO)
import PreludeFixes

import Control.Exception ( try )
import Data.Either
import Data.IORef

import Control.Monad.Extra(whileM)

import SDL
import SDL.Font

import UI.Femto.SDL.Renderable
import qualified UI.Hatto.Events as E

import UI.Femto.SDL.Common as C


import qualified Data.Map.Strict as Map

import Control.Monad (unless)

import UI.Hatto.Widgets 
import UI.Hatto.App

main :: IO ()
main = runHatto $ do -- runHattoProgram (putStrLn "Hello") -- runFemtoSDLProgram prog1
        b <- board' <$> (newMutState [0,0]) <*> (newMutState "Hello World")
        renderDebug b
        appLoop b

runHattoProgram :: MonadIO m => m () -> IO ()
runHattoProgram prog = do
  r <- try $ do
        SDL.initializeAll
        window <- SDL.createWindow "My SDL Application" mainWindowSettings
        showWindow window
        ren <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
        pf <- SDL.getWindowPixelFormat window
        
        -- lbl <- label <$> newMutState (0::Int)
        b <- board' <$> (newMutState [0,0]) <*> (newMutState "Hello World")
        renderDebug b
        appLoop b

        destroyRenderer ren
        destroyWindow window
        SDL.quit

  either (\e -> print (e::SDLException) >> fail "Could not initialize SDL")
         (\st -> putStrLn "Initialized SDL") r


-- appLoop :: Renderer -> IO ()
appLoop w = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent -> do
            putStrLn ("SDL Event!\n" ++ show event) 
            walkWidgetWithEvents (E.sdlEvent2Event event) w
            renderDebug w
            pure $
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> do 
            putStrLn ("SDL Event!\n" ++ show event) 
            walkWidgetWithEvents (E.sdlEvent2Event event) w
            renderDebug w
            pure False
          
  qPresses <- mapM eventIsQPress events
  let qPressed = any (== True) qPresses
  appLoop w



