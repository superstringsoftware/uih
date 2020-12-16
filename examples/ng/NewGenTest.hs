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
import Data.IORef

import Control.Monad.Extra(whileM)

import SDL
import SDL.Font

import UI.Femto.SDL.Renderable
import qualified UI.Femto.Middle.Events as E

import UI.Femto.SDL.Common as C

import UI.Femto.SDL.Fonts 
import UI.Femto.SDL.SDLMonad

import qualified Data.Map.Strict as Map

import Control.Monad (unless)

import UI.Hatto.Widgets 

main :: IO ()
main = runHattoProgram (putStrLn "Hello") -- runFemtoSDLProgram prog1

runHattoProgram :: MonadIO m => m () -> IO ()
runHattoProgram prog = do
  r <- try $ do
        SDL.initializeAll
        window <- SDL.createWindow "My SDL Application" mainWindowSettings
        showWindow window
        ren <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
        pf <- SDL.getWindowPixelFormat window
        
        
        lbl <- (label <$> newIORef (0::Int))
        lbl' <- lbl
        renderDebug lbl'
        appLoop ren lbl

        destroyRenderer ren
        destroyWindow window
        SDL.quit

  either (\e -> print (e::SDLException) >> fail "Could not initialize SDL")
         (\st -> putStrLn "Initialized SDL") r


-- appLoop :: Renderer -> IO ()
appLoop renderer w = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent -> pure $
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          MouseButtonEvent mbe -> do 
            putStrLn ("Mouse Event!\n" ++ show mbe) 
            w' <- w
            processEventsInWidget (E.sdlEvent2Event event) w'
            w'' <- w
            renderDebug w''
            pure False
          _ -> pure False
  qPresses <- mapM eventIsQPress events
  let qPressed = any (== True) qPresses
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer w)


testText = SDLStyledText {
    text = "Hellow New Generation AGAIN!",
    color = mdBlue 500,
    styles = [Bold],
    -- if not empty, we use the "shaded" rendering method to produce backround box in one go
    -- can be used to highlight the text etc
    bgColor = Just $ mdGray 100
}

-- main function that runs the main FemtoUI program
runFemtoSDLProgram :: FemtoUIM () -> IO ()
runFemtoSDLProgram prog = do
  r <- try $ do
        SDL.initializeAll
        window <- SDL.createWindow "My SDL Application" mainWindowSettings
        showWindow window
        ren <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
        pf <- SDL.getWindowPixelFormat window
        let initState = SDLState {
                mainWindow = window
              , mainRenderer  = ren
              , loadedFonts = Map.empty
              , bgColor = V4 0 0 0 255
              , rawIdCounter = 0
              , scaleXY = V2 1 1
              , autoScale = True
              , defaultPixelFormat = pf
              
            }
        print $ show initState
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


prog1 :: FemtoUIM ()
prog1 = do
  ren <- getRenderer
  mfont <- getDefaultFont
  let st = RWSimpleTextLine { -- text line with different styles but the same font and size
                        font = mfont,
                        text = testText,
                        cursorPos = 0
                     }
  let testEl  = renderElement $ mkRWSimpleTextLine st "id0"
  let testEl' = renderElement $ mkRWBox (RWBox { bgColor = mdAmberA 200, fgColor = Nothing, boundingRec = Rectangle (P (V2 0 0)) (V2 250 100)  }) "id1"
  tex  <- texCache testEl
  tex' <- texCache testEl'
  rendererDrawColor ren $= mWhite
  liftIO $ whileM $
    C.isContinue <$> SDL.pollEvent
    >>= C.conditionallyRun (sillyLoop ren tex tex' )


sillyLoop r t1 t2 = do
  clear r
  renderTexture 50 50 t1 r
  renderTexture 50 150 t2 r
  present r 

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
                                                            , glMultisampleSamples = 1
                                                            , glProfile = Compatibility Debug 3 2
                                                          }
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 1200 800
  }