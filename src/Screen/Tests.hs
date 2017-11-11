{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Screen.Tests where

import Color
import Linear
import Data.Text
import Data.Monoid
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

import Screen.MiddleWidgets

bi = basicInfoBox 200 50  300 80 "Hello World"
bi1 = basicInfoBox 200 150 300 80 "Not So Good"
bb = basicButton  240 250 200 80 "OK"

testUI = ((emptyPanel {width = 500, height = 500} <> bi) <> bi1) <> bb
