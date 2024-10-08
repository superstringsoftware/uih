{-# LANGUAGE OverloadedStrings #-}

module TextEditor.SDL.Events
where

import SDL hiding (get)
import SDL.Font

import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Exception ( try )

import Data.Text

import Control.Monad.MRWS
import TextEditor.SDL.SDLMonad

processEvent :: MonadIO m => Event -> SDLUIT m ()
processEvent (Event ts (KeyboardEvent pl)) = liftIO $ print pl
processEvent (Event ts (TextEditingEvent pl)) = liftIO $ print pl
processEvent (Event ts (TextInputEvent pl)) = liftIO $ print pl
processEvent (Event ts KeymapChangedEvent) = liftIO $ print "KEYMAP CHANGED!!!"
processEvent (Event ts (MouseButtonEvent pl)) = liftIO $ print pl
processEvent (Event ts (MouseWheelEvent pl)) = liftIO $ print pl
processEvent (Event ts QuitEvent) = liftIO $ print "WANNA QUIT"
processEvent _ = pure ()
