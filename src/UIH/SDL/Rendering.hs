{-# LANGUAGE OverloadedStrings  #-}
module UIH.SDL.Rendering where

-- import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
-- import GHC.Prim
-- import SDL.Fonts

import qualified SDL.Raw as Raw
import SDL as SDL hiding (Vector)
import SDL.Internal.Types
import SDL.Font (Font, solid, blended)
-- import SDL.Vect hiding (Vector)

import Foreign.C.Types (CInt)

import Color

import Data.Text hiding (copy)
import Data.Vector.Storable -- vectors needed by SDL render prims are Storable
import Data.Word

-- basic datatypes
-- first element of constructor is coordinates, second - color in SDL format
type PrimPoint = Point V2 CInt
type PrimBox   = Rectangle CInt
type PrimColor = V4 Word8
data PrimText  = PrimText { text :: Text, fontName :: Text, fontSize :: CInt, x :: CInt, y :: CInt }

data RenderPrims = PrimPoints (Vector PrimPoint) PrimColor
                 | PrimLines  (Vector PrimPoint) PrimColor
                 | PrimBoxes  (Vector PrimBox)   PrimColor
                 | PrimTexts  (Vector PrimText)  PrimColor

-- to stack layers on top of each other, need to render them in order.
-- also may use some optimizations such as render to texture and then blending etc
type Layer = Vector RenderPrims

-- function that renders a primitive to a current render target
-- the most low level, with explicit renderers and fonts, in the IO monad
-- this will need to be wrapped into our custom state keeping monad
renderPrimitive :: MonadIO m => Renderer -> RenderPrims -> m ()
-- setting a color via StateVar provided by SDL and then drawing.
renderPrimitive ren (PrimPoints ps color) = (rendererDrawColor ren) $= color >> drawPoints ren ps
renderPrimitive ren (PrimLines  ps color) = (rendererDrawColor ren) $= color >> drawLines  ren ps
renderPrimitive ren (PrimBoxes  ps color) = (rendererDrawColor ren) $= color >> drawRects  ren ps

-- drawing text is the most tricky
-- NOT IMPLEMENTED
renderPrimitive ren (PrimTexts txt color) = do
  error "renderPrimitive PrimTexts is not implemented yet!"
  return ()
