{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, MagicHash #-}
module Main where

import SDL as SDL hiding (Vector)
import Linear (V4(..))
import Control.Monad (unless)

-- import SDL.Bindings
import qualified SDL.Raw as Raw

import Color

import System.Random

import Foreign.C.Types

import Data.Vector.Storable hiding (sequence, replicate, any)

import Criterion.Measurement
import Criterion.Main

import GHC.Prim
import GHC.Exts

main = return ()

{-
data UCPoint = UCP Int# Int#

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

rnd1 :: Int -> IO [CInt]
rnd1 n = sequence $ replicate n $ randomRIO (10,1750::CInt)

rnd2 :: Int -> IO (Vector (Point V2 CInt))
rnd2 n = replicateM n $ do
    x <- randomRIO (10,1750::CInt)
    y <- randomRIO (10,1750::CInt)
    let p = P (V2 x y)
    return $ p


main :: IO ()
main = do
  initializeAll

  window <- createWindow "My SDL Application" mainWindow
  renderer <- createRenderer window (-1) defaultRenderer

  setRenderDrawColorRGBA renderer $ mdGrey 900
  -- rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer

  setRenderDrawColorRGBA renderer $ mdBlue 500
  pts <- rnd2 10000

  (mes,_) <- measure (nfIO $ drawLines renderer pts) 1

  putStrLn $ show mes

  present renderer


  -- putStrLn $ show l

  appLoop renderer

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

-}
