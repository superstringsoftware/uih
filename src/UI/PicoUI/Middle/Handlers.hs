{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, 
TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module UI.PicoUI.Middle.Handlers where

-- Impure handlers in SDLIO monad, based on PureHandlers

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Control.Exception
import Data.Text hiding (any)
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Data.Foldable as Map (length) 

import Color

import UI.PicoUI.Middle.AbstractWidgets
import UI.PicoUI.Raw.Events

import UI.PicoUI.PicoUIMonad

import Data.Vector.Generic

import PreludeFixes

-- type PureHandler e = AbstractWidget -> Reader Event AbstractWidget 

