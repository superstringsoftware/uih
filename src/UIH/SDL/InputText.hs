{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module UIH.SDL.InputText where

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

import qualified SDL.Raw as Raw
import SDL hiding (Vector)
import SDL.Font (Font, solid, blended)

import Foreign.C.Types (CInt)

import Color

import Data.Text hiding (copy)
import qualified Data.Vector.Storable as V -- vectors needed by SDL render prims are Storable
import Data.Word

import UIH.SDL.SDLIO
import UIH.SDL.Fonts

data InputText = IT {
  text :: Text
}
