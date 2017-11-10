module Main where

import Graphics.UI.GLUT
import OpenGL.OpenGLTest

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= displayc
  -- runInputT defaultSettings loop
  mainLoop
