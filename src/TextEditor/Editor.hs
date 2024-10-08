{-# LANGUAGE OverloadedStrings, 
    TypeSynonymInstances, 
    FlexibleInstances,
    NoImplicitPrelude
 #-}

module TextEditor.Editor
where

import Data.Vector hiding (modify)
import Data.Text
import Prelude
import Data.Map as Map

import Control.Monad.MRWS
import SDL
    ( V2(V2),
      RendererConfig(rendererTargetTexture),
      initializeAll,
      quit,
      createRenderer,
      createWindow,
      destroyRenderer,
      destroyWindow,
      getWindowPixelFormat,
      showWindow,
      defaultRenderer, pollEvents, copy, rendererDrawColor, ($=), clear, present )
import TextEditor.SDL.SDLMonad (SDLState(..), SDLUIT, mainWindowSettings, defaultTextStyle)
import TextEditor.SDL.Fonts (initFonts, destroyFonts, getDefaultFont)
import Control.Monad.Trans (MonadIO)
import TextEditor.SDL.Events
import TextEditor.SDL.Rendering (rectangleFromTexture, createTextureFromTextElement)
import Linear (V4(..))

-- type ActionT r w s m = MRWST r w s m



{-
We need:

- Source of the file loaded into memory, split into lines (Vector Text)
- Position cursor logically
- Window size and font size checked
- Calculate the height of the line
- Calculate the width of the line in symbols (it's limited by window physically)
- Wrapped / not wrapped? (not wrapped initially)
- if syntax highlighting etc - transforming text with whatever sybmols
- Process line by line, create Vector of textures for each line
- Calculate where cursor should be, create cursor texture
- RENDER
- Event loop
-}

initAll :: (MonadIO m, MonadFail m) => SDLUIT m ()
initAll = do
        liftIO SDL.initializeAll
        window <- liftIO $ SDL.createWindow "My SDL Application" mainWindowSettings
        liftIO $ showWindow window
        ren <- liftIO $ SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
        pf <- liftIO $ SDL.getWindowPixelFormat window

        let sdlState = SDLState {
            mainWindow = window,
            mainRenderer = ren,
            loadedFonts = Map.empty,
            scaleXY = V2 1 1,
            autoScale = True,
            defaultPixelFormat = pf
        }
        modify (const sdlState)
        initFonts
        liftIO $ print sdlState

        -- prog

cleanUp :: MonadIO m => SDLUIT m ()
cleanUp = do
    sdlState <- get
    destroyFonts
    destroyRenderer $ mainRenderer sdlState
    destroyWindow $ mainWindow sdlState
    liftIO SDL.quit

mainLoop :: (MonadIO m, MonadFail m) => SDLUIT m ()
mainLoop = do
    events <- SDL.pollEvents -- get the events queue from SDL
    Prelude.mapM_ processEvent events >> drawTest >> mainLoop
                

drawTest :: (MonadIO m, MonadFail m) => SDLUIT m ()
drawTest = do
    ren <- gets mainRenderer
    font <- getDefaultFont
    tex <- liftIO $ createTextureFromTextElement ren "Hello world" defaultTextStyle font
    rec <- liftIO $ rectangleFromTexture tex (V2 50 50)
    rendererDrawColor ren $= V4 0 0 255 255
    SDL.clear ren
    SDL.copy ren tex Nothing (Just rec)
    present ren