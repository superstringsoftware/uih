{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, TypeSynonymInstances, RecordWildCards, ScopedTypeVariables #-}
module UI.Hatto.App
where

import UI.Hatto.Events
import UI.Hatto.Widgets

import Data.IORef
import Control.Monad.IO.Class (liftIO, MonadIO)

import SDL
import SDL.Font
import Data.Text
import Data.Map as Map
import Foreign.C.Types (CInt, CFloat)
import Data.Word ( Word8 )
import Control.Exception ( try )
import Data.Either

import UI.Hatto.SDL.Fonts



-- main GUI function that runs the app
runHatto :: MonadIO m => m () -> m ()
runHatto prog = do
        liftIO SDL.initializeAll
        window <- liftIO $ SDL.createWindow "My SDL Application" mainWindowSettings
        liftIO $ showWindow window
        ren <- liftIO $ SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
        pf <- liftIO $ SDL.getWindowPixelFormat window

        sdlState <- newMutState SDLState {
            mainWindow = window,
            mainRenderer = ren,
            loadedFonts = Map.empty,
            scaleXY = V2 1 1,
            autoScale = True,
            defaultPixelFormat = pf
        }
        initFonts sdlState
        readMutState sdlState >>= liftIO . putStrLn . show

        prog

        liftIO $ destroyFonts sdlState
        liftIO $ destroyRenderer ren
        liftIO $ destroyWindow window
        liftIO SDL.quit

{-
runHattoTry :: MonadIO m => m () -> m ()
runHattoTry prog = do
    r <- liftIO (try (runHatto prog))    
    either (\e  -> liftIO (print (e::SDLException) >> fail "Could not initialize SDL"))
            (\_ -> liftIO $ putStrLn "Initialized SDL") 
            r
-}  


  

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
  