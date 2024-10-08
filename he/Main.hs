{-# LANGUAGE OverloadedStrings, 
    TypeSynonymInstances, 
    FlexibleInstances
 #-}

 module Main
 where

import Control.Monad.MRWS
import TextEditor.Editor
import TextEditor.SDL.SDLMonad (SDLState(..))

main = runMRWST (initAll >> mainLoop >> cleanUp) [] SDLStateNone 
