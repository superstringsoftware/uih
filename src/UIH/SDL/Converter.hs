-- this takes MiddleWidgets and converts them to low level stuff usable by renderer
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}
module UIH.SDL.Converter where

-- import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
-- import GHC.Prim
-- import SDL.Fonts

import qualified SDL.Raw as Raw
import SDL as SDL hiding (Vector)
-- import SDL.Internal.Types
import SDL.Font (Font, solid, blended)
-- import SDL.Vect hiding (Vector)

import Foreign.C.Types (CInt)

import Color

import Data.Text hiding (copy)
import Data.Vector.Storable hiding (copy)-- vectors needed by SDL render prims are Storable
import Data.Word

import UIH.SDL.SDLIO
import UIH.SDL.Fonts
import UIH.SDL.Rendering

import Screen.MiddleWidgets
