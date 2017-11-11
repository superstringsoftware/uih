{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NamedFieldPuns, OverloadedLabels  #-}
module UIH.SDL.Rendering where

import Control.Monad.Trans.State.Strict
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
import qualified Data.Vector.Storable as V -- vectors needed by SDL render prims are Storable
import Data.Word

import UIH.SDL.SDLIO
import UIH.SDL.Fonts

import Screen.MiddleWidgets

-- Why the heck can't we render directly??? Take a widget, render it to texture, do final composing later on??
widgetToTexture :: Widget -> SDLIO (Maybe Texture)
widgetToTexture w@(TextLabel {color = rclr, text = txt, x, y}) = do
  _debug $ "Rendering TextLabel: " ++ show w
  ren <- gets mainRenderer
  fnt <- getDefaultFont
  let clr = rgbaToV4Color rclr
  case fnt of
      Just font -> do
                      tsurf <- blended font clr txt
                      tex   <- createTextureFromSurface ren tsurf
                      renderTexture 0 0 tex ren
                      freeSurface tsurf
                      return (Just tex)
      Nothing -> liftIO (print "Couldn't find font when rendering TextLabel") >> return Nothing

widgetToTexture w@(Panel {width, height, color}) = do
  _debug $ "Rendering Panel: " ++ show w
  ren <- gets mainRenderer
  tex <- createTexture ren ARGB8888 TextureAccessTarget (V2 (fromIntegral width) (fromIntegral height))
  rendererRenderTarget ren $= Just tex
  rendererDrawColor ren $= rgbaToV4Color color
  fillRect ren Nothing
  return $ Just tex

widgetToTexture (Complex p@(Panel {}) ws) = do
  _debug $ "Rendering Complex: " ++ show p
  Just mainTex <- widgetToTexture p
  _debugTexture mainTex
  Prelude.mapM_ (renderToTexture mainTex) ws
  _debug $ "Finished mapping!"
  return $ Just mainTex
  where renderToTexture tex w = do
            liftIO $ print "Inside children rendering, texture to render to is:"
            _debugTexture tex
            _debug $ "Widget is: " ++ show w
            ren <- gets mainRenderer
            Just t <- widgetToTexture w
            _debugTexture t
            _debugTexture tex
            _debug $ "And now Widget is: " ++ show w
            qinf <- queryTexture t
            rendererRenderTarget ren $= Just tex -- rendering to texture
            let rect = SDL.Rectangle (P $ V2 (fromIntegral (xOfWidget w)) (fromIntegral (yOfWidget w))) (V2 (textureWidth qinf) (textureHeight qinf))
            liftIO $ print $ "Rectangle to copy to is: " ++ show rect
            SDL.copy ren t Nothing (Just rect)
            --destroyTexture t


widgetToTexture (Complex _ _) = error "Should not render Complex Widgets where root node is not Panel"

_debug = liftIO . print
_debugTexture tex = do
  qi <- queryTexture tex
  liftIO $ print $ "[DEBUG][Texture] " Prelude.++ show qi

-- basic datatypes
-- first element of constructor is coordinates, second - color in SDL format
type PrimPoint = Point V2 CInt
type PrimBox   = Rectangle CInt
type PrimColor = V4 Word8
data PrimText  = PrimText { pText :: Text, fontName :: Text, fontSize :: CInt, cx :: CInt, cy :: CInt }

data RenderPrims = PrimPoints (V.Vector PrimPoint) PrimColor
                 | PrimLines  (V.Vector PrimPoint) PrimColor
                 | PrimBoxes  (V.Vector PrimBox)   PrimColor
                 | PrimTexts  PrimText           PrimColor

-- to stack layers on top of each other, need to render them in order.
-- also may use some optimizations such as render to texture and then blending etc
type Layer = V.Vector RenderPrims

-- function that renders a primitive to a current render target
-- the most low level, with explicit renderers and fonts, in the IO monad
-- this will need to be wrapped into our custom state keeping monad
renderPrimitive :: Renderer -> RenderPrims -> SDLIO ()
-- setting a color via StateVar provided by SDL and then drawing.
renderPrimitive ren (PrimPoints ps color) = (rendererDrawColor ren) $= color >> drawPoints ren ps
renderPrimitive ren (PrimLines  ps color) = (rendererDrawColor ren) $= color >> drawLines  ren ps
renderPrimitive ren (PrimBoxes  ps color) = (rendererDrawColor ren) $= color >> fillRects  ren ps

-- drawing text is the most tricky
renderPrimitive ren (PrimTexts txt color) = do
  fnt <- getDefaultFont
  case fnt of
      Just font -> do
                      tsurf <- blended font color (pText txt)
                      tex   <- createTextureFromSurface ren tsurf
                      renderTexture (cx txt) (cy txt) tex ren
                      freeSurface tsurf
                      destroyTexture tex
      Nothing -> liftIO $ print "Couldn't find font when rendering TextLabel"


-- render a given texture at x y coordinates
renderTexture :: MonadIO m => CInt -> CInt -> Texture -> Renderer -> m ()
renderTexture x y texture renderer = do
  ti <- queryTexture texture
  -- putStrLn $ "Size is " ++ (show ti)
  let w = textureWidth ti
  let h = textureHeight ti
  let dest = Rectangle (P (V2 x y)) (V2 w h)
  copy renderer texture Nothing (Just dest)
