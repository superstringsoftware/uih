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
import UIH.UI.AbstractWidgets
import UIH.UI.Handlers

-- import Data.Foldable

import SDL as SDL hiding (initializeAll)

-- import SDL.Raw.Types (Rect(..))

main :: IO ()
main = do
  putStrLn "Starting main..."
  runSDLUI program

program = do
  initializeAll 
  registerWidget InputText {
      fontData = FontDataDefault,
      text = "input",
      valign = CenterAlign, halign = LeftAlign,
      layout = l_TL 40 40 300 50,
      background = BGColor $ rgbaToV4Color $ mdGrey 700,
      cacheRect = V4 0 0 0 0
  }
  registerWidget InputText {
      fontData = FontDataDefault,
      text = "new line",
      valign = CenterAlign, halign = LeftAlign,
      layout = l_TL 40 140 300 30,
      background = BGColor $ rgbaToV4Color $ mdGrey 900,
      cacheRect = V4 0 0 0 0
  }
  
  renderUI
  -- SDL.startTextInput $ Rect 0 0 300 60
  -- SDL.stopTextInput
  appLoop





