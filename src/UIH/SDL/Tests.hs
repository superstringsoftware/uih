{-# LANGUAGE OverloadedStrings  #-}
module UIH.SDL.Tests where

import UIH.SDL.Rendering
import UIH.SDL.System
import UIH.SDL.SDLIO

import SDL as SDL

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad
-- import Control.Applicative

import Data.Vector.Storable hiding (mapM, any)

import SDL.Vect

import Screen.Tests


program = do
  dumpSDLState >> UIH.SDL.System.initializeAll >> dumpSDLState >> renderUI1 >> appLoop

renderUI1 = do
  _debug $ "Drawing " Prelude.++ show testUI
  (Just tex) <- widgetToTexture testUI
  renderer <- gets mainRenderer
  SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
  rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer
  qinf <- SDL.queryTexture tex
  let rect = SDL.Rectangle (P $ V2 0 0) (V2 (textureWidth qinf) (textureHeight qinf))
  SDL.copy renderer tex Nothing (Just rect)
  SDL.present renderer

renderUI = do
  let rec1 = SDL.Rectangle (P $ V2 10 10) (V2 100 300)
  let rec2 = SDL.Rectangle (P $ V2 400 90) (V2 200 50)
  let boxes = PrimBoxes (fromList [rec1, rec2]) (V4 200 200 200 20)
  let txt = PrimText { pText = "Hello World", fontName = "default", fontSize = 0, cx = 400, cy = 400 }
  renderer <- gets mainRenderer
  SDL.clear renderer
  renderPrimitive renderer boxes
  renderPrimitive renderer (PrimTexts txt (V4 200 50 50 20))
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
