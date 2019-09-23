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
import UIH.UI.BasicWidgets
import UIH.UI.Handlers

-- import Data.Foldable

import qualified SDL as SDL

import SDL.Raw.Types (Rect(..))

main :: IO ()
main = do
  putStrLn "Starting main..."
  runSDLUI program

program = do
  initializeAll 
  registerUIWidget $ newTextInput testButton
  renderUI
  -- SDL.startTextInput $ Rect 0 0 300 60
  -- SDL.stopTextInput
  appLoop



testHandler1 :: Event -> SDLUI u ()
testHandler1 ev@(Event EvHover (i,w)) = liftIO $ putStrLn $ "Hovering over widget # " ++ show i




