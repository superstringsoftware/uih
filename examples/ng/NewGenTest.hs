{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, TypeSynonymInstances, RecordWildCards, ScopedTypeVariables #-}

module Main where

import Color
import Linear
import Data.Text hiding (any)

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO, MonadIO)
import PreludeFixes

import Control.Exception ( try )
import Data.Either

import Control.Monad.Extra(whileM)

import SDL
import SDL.Font

import UI.Femto.SDL.Renderable

import UI.Femto.SDL.Common as C

import UI.Femto.SDL.Fonts 
import UI.Femto.SDL.SDLMonad

import qualified Data.Map.Strict as Map

main :: IO ()
main = runFemtoSDLProgram () prog1



testText = SDLStyledText {
    text = "Hellow New Generation AGAIN!",
    color = mdBlue 500,
    styles = [Bold],
    -- if not empty, we use the "shaded" rendering method to produce backround box in one go
    -- can be used to highlight the text etc
    bgColor = Just $ mdGray 100
}

-- main function that runs the main FemtoUI program
runFemtoSDLProgram :: u -> FemtoUIM u () -> IO ()
runFemtoSDLProgram uState prog = do
  r <- try $ do
        SDL.initializeAll
        window <- SDL.createWindow "My SDL Application" mainWindowSettings
        showWindow window
        ren <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
        let initState = SDLState {
                mainWindow = window
              , mainRenderer  = ren
              , loadedFonts = Map.empty
              , bgColor = V4 0 0 0 255
              , rawIdCounter = 0
              , scaleXY = V2 1 1
              , autoScale = True
              , userState = uState
            }

        quickEvalSDLIO $ do 
            put initState
            initFonts
            prog
            destroyFonts

        destroyRenderer ren
        destroyWindow window
        SDL.quit

  either (\e -> print (e::SDLException) >> fail "Could not initialize SDL")
         (\st -> putStrLn "Initialized SDL") r


prog1 :: FemtoUI ()
prog1 = do
  ren <- getRenderer
  mfont <- getDefaultFont
  let st = RWSimpleTextLine { -- text line with different styles but the same font and size
                        font = mfont,
                        text = testText,
                        cursorPos = 0
                     }
  let testEl = mkRWSimpleTextLine st "id0"
  let testEl' = renderElement ren testEl
  tex <- liftIO $ render ren st
  dest <- liftIO $ rectangleFromTexture tex 50 50
  -- let (Just tex'') = texCache testEl'
  tex' <- liftIO $ texCache testEl'
  dest' <- liftIO $ rectangleFromTexture tex' 50 150
  rendererDrawColor ren $= mWhite
  clear ren
  SDL.copy ren tex Nothing (Just dest)
  SDL.copy ren tex' Nothing (Just dest') 
  whileM $
    C.isContinue <$> SDL.pollEvent
    >>= C.conditionallyRun (sillyLoop ren)


sillyLoop r = present r 

mainWindowSettings = defaultWindow
  { windowBorder       = True
  -- There are issues with high DPI windows b/c we need to recalculate all coordinates when drawing / checking event
  -- coordinates, so its support is pending
  -- OpenGLContext defaultOpenGL
  , windowHighDPI      = True
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowGraphicsContext = OpenGLContext $ defaultOpenGL {
                                                              glColorPrecision = V4 8 8 8 0
                                                            , glDepthPrecision = 24
                                                            , glStencilPrecision = 8
                                                            , glMultisampleSamples = 0
                                                            , glProfile = Compatibility Debug 3 2
                                                          }
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 1200 800
  }