{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Font as Font

-- Simple SDL test to debug the issue
main :: IO ()
main = do
  putStrLn "Testing SDL initialization..."
  
  SDL.initializeAll
  putStrLn "SDL initialized"
  
  Font.initialize  
  putStrLn "SDL_TTF initialized"
  
  window <- SDL.createWindow "Test Window" SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 800 600 }
  putStrLn "Window created"
  
  renderer <- SDL.createRenderer window (-1) (SDL.RendererConfig SDL.AcceleratedRenderer False)
  putStrLn "Renderer created"
  
  SDL.delay 1000
  
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
  
  putStrLn "SDL test completed successfully!"