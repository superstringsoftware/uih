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
import UIH.SDL2.RenderMonad
import UIH.UI.ManagerMonad
import UIH.UI.Widgets

import qualified SDL as SDL

import SDL.Raw.Types (Rect(..))

main :: IO ()
main = do
  putStrLn "Starting main..."
  runSDLUI program

program = do
  initializeAll 
  addWidgetWithHandler (PolyWidget testButton updateButtonText) (EventHandler testHandler)
  -- SDL.startTextInput $ Rect 0 0 300 60
  -- SDL.stopTextInput
  appLoop


testHandler :: Event SDLIO -> SDLUI ()
testHandler ev@(Event EvHover (i,w)) = liftIO $ putStrLn $ "Hovering over widget # " ++ show i




