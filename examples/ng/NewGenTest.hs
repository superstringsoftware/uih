{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, TypeSynonymInstances, RecordWildCards, ScopedTypeVariables #-}

module Main where

import Color
import Linear
import Data.Text hiding (any)

import PreludeFixes

import Control.Exception ( try )
import Data.Either

import SDL
import SDL.Font

import UI.Femto.SDL.Renderable

main :: IO ()
main = initStateIO

testText = SDLStyledText {
    text = "Hellow New Generation AGAIN!",
    color = mdBlue 500,
    styles = [Bold],
    -- if not empty, we use the "shaded" rendering method to produce backround box in one go
    -- can be used to highlight the text etc
    bgColor = Just $ mdGray 100
}

initStateIO :: IO ()
initStateIO = do 
    r <- try $ do
            SDL.initializeAll
            print "SDL Initialized (hopefully)"
            window <- SDL.createWindow "My SDL Application" mainWindowSettings
            print $ "SDL Window created:\n" ++ show window 
            ren <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
            print "SDL Renderer initialized"
            mfont <- SDL.Font.initialize >> unsafeLoadFont defaultFontPath 32
            print "Loaded font"
            let st = RWSimpleTextLine { -- text line with different styles but the same font and size
                        font = mfont,
                        text = testText,
                        cursorPos = 0
                     }
            tex <- render ren st
            ti <- queryTexture tex
            let w = textureWidth ti
            let h = textureHeight ti
            let dest = Rectangle (P (V2 50 50)) (V2 w h)
            rendererDrawColor ren $= mWhite
            clear ren
            SDL.copy ren tex Nothing (Just dest) 
            present ren
            destroyTexture tex
            free mfont
            SDL.delay 5000
    either (\e -> print (e::SDLException) >> fail "Could not initialize SDL")
           (\st -> putStrLn "Initialized SDL") r


defaultFontPath = "./Roboto-Light.ttf"

unsafeLoadFont :: String -> Int -> IO Font
unsafeLoadFont path size = do
    r <- try $ load path size
    either (\e -> print (e::SDLException) >> fail "Couldn't load font") pure r

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