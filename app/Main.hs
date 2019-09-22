{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main where

import Color
import Linear
import Data.Text hiding (any)
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad

import UIH.SDL2.SDLUI
import UIH.UI.ManagerMonad
import UIH.UI.Widgets

import qualified SDL as SDL

main :: IO ()
main = do
  putStrLn "Starting main..."
  runSDLUI program

program = do
  initializeAll 
  registerWidget $ PolyWidget testButton
  appLoop









