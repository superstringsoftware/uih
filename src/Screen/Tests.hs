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
bi1 = basicInfoBox 200 150 300 80 "Not So Good Because Its a Long String"
bb = basicButton  240 350 200 80 "OK"
bin = (basicInput 200 250 300 80 "Input text string") {cursorPos = 3}

testUI = (((emptyPanel {width = 500, height = 500} <> bi) <> bi1) <> bin) <> bb
