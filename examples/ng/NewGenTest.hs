{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, TypeSynonymInstances, RecordWildCards, ScopedTypeVariables #-}

module Main where

import Color
import Linear
import Data.Text hiding (any)

import PreludeFixes

import Control.Exception ( try )
import Data.Either

import Control.Monad.Extra(whileM)

import SDL
import SDL.Font

import UI.Femto.SDL.Renderable

import UI.Femto.SDL.Common as C

oldmain :: IO ()
oldmain = initStateIO


main :: IO ()
main = C.withSDL $ do
  C.setHintQuality
  C.withWindow "Test" (1200, 800) $ \w ->
    C.withRenderer w $ \ren -> do

      mfont <- SDL.Font.initialize >> unsafeLoadFont defaultFontPath 32
      print "Loaded font"
      let st = RWSimpleTextLine { -- text line with different styles but the same font and size
                  font = mfont,
                  text = testText,
                  cursorPos = 0
                }
      let testEl = mkRWSimpleTextLine st "id0"
      let testEl' = renderElement ren testEl
      tex <- render ren st
      dest <- rectangleFromTexture tex 50 50
      -- let (Just tex'') = texCache testEl'
      tex' <- texCache testEl'
      dest' <- rectangleFromTexture tex' 50 150
      rendererDrawColor ren $= mWhite
      

      whileM $
        C.isContinue <$> SDL.pollEvent
        >>= C.conditionallyRun (draw ren (tex,dest) (tex', dest') )

      destroyTexture tex
      destroyTexture tex'
      free mfont


-- draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw r (t,d) (t1,d1) = do
  SDL.clear r
  SDL.copy r t Nothing (Just d)
  SDL.copy r t1 Nothing (Just d1) 
  SDL.present r


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
            SDL.showWindow window
            ren <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
            print "SDL Renderer initialized"
            mfont <- SDL.Font.initialize >> unsafeLoadFont defaultFontPath 32
            print "Loaded font"
            let st = RWSimpleTextLine { -- text line with different styles but the same font and size
                        font = mfont,
                        text = testText,
                        cursorPos = 0
                     }
            let testEl = mkRWSimpleTextLine st "id0"
            let testEl' = renderElement ren testEl
            tex <- render ren st
            dest <- rectangleFromTexture tex 50 50
            -- let (Just tex'') = texCache testEl'
            tex' <- texCache testEl'
            dest' <- rectangleFromTexture tex' 50 150
            rendererDrawColor ren $= mWhite
            clear ren
            SDL.copy ren tex Nothing (Just dest)
            SDL.copy ren tex' Nothing (Just dest') 
            present ren
            destroyTexture tex
            free mfont
            SDL.delay 50000
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