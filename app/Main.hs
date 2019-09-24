{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main where

import Color
import Linear
import Data.Text hiding (any)
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad

import UI.PicoUI.Raw.SDLIO
import UI.PicoUI.Raw.EventLoop

-- import Data.Foldable

import qualified SDL as SDL

-- import SDL.Raw.Types (Rect(..))
testProgram :: SDLIO ()
testProgram = do
    btn <- testButton
    registerWidget btn
    appLoop

main :: IO ()
main = runSDLIO testProgram >> pure ()

{-
main :: IO ()
main = do
  putStrLn "Starting main..."
  runSDLUI program
-}






