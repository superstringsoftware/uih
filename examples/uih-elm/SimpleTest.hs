{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Simple test without fonts to isolate the SDL issue

import qualified SDL
import Data.Text (Text)
import UI.UIHElm.Core.Types

-- Simple SDL test without font loading
testSDLBasic :: IO ()
testSDLBasic = do
  putStrLn "=== Testing Basic SDL ==="
  
  SDL.initializeAll
  putStrLn "SDL initialized"
  
  window <- SDL.createWindow "Basic Test" $ SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 800 600 }
  putStrLn "Window created"
  SDL.showWindow window
  
  renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedRenderer
    , SDL.rendererTargetTexture = False
    }
  putStrLn "Renderer created"
  
  -- Clear screen to red
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255
  SDL.clear renderer
  SDL.present renderer
  
  putStrLn "Screen cleared to red. Waiting 3 seconds..."
  SDL.delay 3000
  
  -- Cleanup
  SDL.destroyRenderer renderer  
  SDL.destroyWindow window
  SDL.quit
  
  putStrLn "Test completed!"

main :: IO ()
main = testSDLBasic