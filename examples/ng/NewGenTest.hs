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

import qualified UI.Hatto.Events as E


import qualified Data.Map.Strict as Map

import Control.Monad (unless)

import UI.Hatto.Widgets 
import UI.Hatto.App

main :: IO ()
main = do -- runHattoProgram (putStrLn "Hello") -- runFemtoSDLProgram prog1
        -- board' <$> newMutState [0,0] <*> newMutState "Hello World" >>= runHatto
        boardS >>= runHatto



