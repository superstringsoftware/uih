{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, TypeSynonymInstances, RecordWildCards, ScopedTypeVariables #-}
module UI.Hatto.App
where

import UI.Hatto.Events
import UI.Hatto.Widgets

import Data.IORef
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM)
import Data.Functor ((<&>))

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
bracketHatto :: MonadIO m => m () -> m ()
bracketHatto prog = do
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

runHatto :: MonadIO m => m (Widget m) -> m()
runHatto mainAppW = bracketHatto (mainLoop True mainAppW)

runHattoS :: MonadIO m => StatefulWidget m -> m()
runHattoS mainAppW = bracketHatto (mainLoopS True mainAppW)


{-
runHattoTry :: MonadIO m => m () -> m ()
runHattoTry prog = do
    r <- liftIO (try (runHatto prog))    
    either (\e  -> liftIO (print (e::SDLException) >> fail "Could not initialize SDL"))
            (\_ -> liftIO $ putStrLn "Initialized SDL") 
            r
-}  

-- Actual application loop will look as follows:
-- Initialization of SDL
-- Initialization of SDLStatus attached to the main App
-- Creation of MutStates for all our Widgets -- how???
-- actuall apploop of polling events and rendering the top-level App widget
-- So the program in essence is the set of event handlers

mainLoop :: MonadIO m => Bool -> m (Widget m) -> m ()
mainLoop isDirty mainAppW = do
    -- isDirty <- getDirty
    -- if isDirty then renderUI >> setClean else pure ()
    if isDirty then renderUI mainAppW else pure ()
    events <- SDL.pollEvents -- get the events queue from SDL
    case events of 
        [] -> mainLoop False mainAppW
        _  -> do
                shouldQuit <- foldM (\a e -> fireEvent mainAppW e <&> (|| a)) False events -- gather results of firing events, folding with "or" - neat, eh?
                if shouldQuit
                then liftIO (putStrLn "Good-bye.")
                else mainLoop True mainAppW

mainLoopS :: MonadIO m => Bool -> StatefulWidget m -> m ()
mainLoopS isDirty mainAppW = do
    -- isDirty <- getDirty
    -- if isDirty then renderUI >> setClean else pure ()
    if isDirty then renderUIS mainAppW else pure ()
    events <- SDL.pollEvents -- get the events queue from SDL
    case events of 
        [] -> mainLoopS False mainAppW
        _  -> do
                shouldQuit <- foldM (\a e -> fireEventS mainAppW e <&> (|| a)) False events -- gather results of firing events, folding with "or" - neat, eh?
                if shouldQuit
                then liftIO (putStrLn "Good-bye.")
                else mainLoopS True mainAppW

renderUI :: MonadIO m => m (Widget m) -> m ()
renderUI = renderDebug

renderUIS :: MonadIO m => StatefulWidget m -> m ()
renderUIS = renderDebugS
    

fireEvent :: MonadIO m => m (Widget m) -> SDL.Event -> m Bool
fireEvent mw e = case e of
    (SDL.Event _ SDL.QuitEvent) -> pure True 
    _             -> walkWidgetWithEvents (sdlEvent2Event e) mw >> pure False 

fireEventS :: MonadIO m => StatefulWidget m -> SDL.Event -> m Bool
fireEventS sw e = case e of
    (SDL.Event _ SDL.QuitEvent) -> pure True 
    _             -> walkWidgetWithEventsS (sdlEvent2Event e) sw >> pure False 


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
  