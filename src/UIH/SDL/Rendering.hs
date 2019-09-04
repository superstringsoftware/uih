{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards  #-}
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

import Data.Text as T hiding (copy)
import qualified Data.Vector.Storable as V -- vectors needed by SDL render prims are Storable
import Data.Word

import UIH.SDL.SDLIO
import UIH.SDL.Fonts
import UIH.SDL.System

import Screen.MiddleWidgets

-- Why the heck can't we render directly??? Take a widget, render it to texture, do final composing later on??
widgetToTexture :: Widget -> SDLIO (Maybe Texture)
-- static text label
widgetToTexture w@TextLabel {color = rclr, text = txt, x, y} = do
  ren <- gets mainRenderer
  fnt <- getDefaultFont
  let clr = rgbaToV4Color rclr
  case fnt of
      Just font -> Just <$> textToTexture txt ren clr font
      Nothing -> liftIO (print "Couldn't find font when rendering TextLabel") >> return Nothing

widgetToTexture w@Panel {width, height, color} = do
  ren <- gets mainRenderer
  tex <- createTexture ren RGBA8888 TextureAccessTarget (V2 (fromIntegral width) (fromIntegral height))
  rendererRenderTarget ren $= Just tex
  rendererDrawColor ren $= rgbaToV4Color color
  fillRect ren Nothing
  return $ Just tex

widgetToTexture (Complex p@Panel {} ws) = do
  Just mainTex <- widgetToTexture p
  Prelude.mapM_ (renderToTexture mainTex) ws
  return $ Just mainTex
  where renderToTexture tex w = do
            ren <- gets mainRenderer
            Just t <- widgetToTexture w
            textureToTexture tex t (fromIntegral $ xOfWidget w) (fromIntegral $ yOfWidget w)
            destroyTexture t

-- rendering dynamically editing text
widgetToTexture w@InputText { .. } = do
  let (s1, s2) = T.splitAt cursorPos text -- splitting status string at the cursor position
  ren <- gets mainRenderer
  fnt <- getDefaultFont
  let clr = rgbaToV4Color color
  case fnt of
      Nothing -> liftIO (print "Couldn't find font when rendering InputText") >> return Nothing
      Just font -> do
        t1 <- textToTexture s1 ren clr font
        t2 <- textToTexture s2 ren clr font
        i1 <- queryTexture t1
        i2 <- queryTexture t2
        let (w1, h1) = (textureWidth i1, textureHeight i1)
        let (w2, h2) = (textureWidth i2, textureHeight i2)
        _debug (show i1) >> _debug (show i2)
        Just tex <- widgetToTexture panel -- now rendering bounding box
        tex1 <- textureToTexture tex  t1 0  0
        tex2 <- textureToTexture tex1 t2 w1 0
        -- updating global cursor position
        modify' (\st -> st { cursor = (cursor st) {x = w1 - 1, y = 100, height = h1} })
        cursorOn
        return $ Just tex2



widgetToTexture (Complex _ _) = error "Should not render Complex Widgets where root node is not Panel"

_debug :: String -> SDLIO ()
_debug = liftIO . print
_debugTexture tex = do
  qi <- queryTexture tex
  liftIO $ print $ "[DEBUG][Texture] " Prelude.++ show qi

-- various utility functions


-- Internal helper functions
textureToTexture texTgt texSrc x y = do
          ren <- gets mainRenderer
          qinf <- queryTexture texSrc
          rendererRenderTarget ren $= Just texTgt -- rendering to texture
          let rect = SDL.Rectangle (P $ V2 x y) (V2 (textureWidth qinf) (textureHeight qinf))
          SDL.copy ren texSrc Nothing (Just rect)
          return texTgt

textToTexture :: MonadIO m => Text -> Renderer -> V4 Word8 -> Font -> m Texture
textToTexture txt ren color font = do
  tsurf <- blended font color txt
  tex   <- createTextureFromSurface ren tsurf
  freeSurface tsurf
  return tex

textToMaybeTexture :: MonadIO m => Text -> Renderer -> V4 Word8 -> Font -> m (Maybe Texture)
textToMaybeTexture txt ren color font = do
  tsurf <- blended font color txt
  tex   <- createTextureFromSurface ren tsurf
  freeSurface tsurf
  return $ Just tex
  

-- render a given texture at x y coordinates
renderTexture :: MonadIO m => CInt -> CInt -> Texture -> Renderer -> m ()
renderTexture x y texture renderer = do
  ti <- queryTexture texture
  -- putStrLn $ "Size is " ++ (show ti)
  let w = textureWidth ti
  let h = textureHeight ti
  let dest = Rectangle (P (V2 x y)) (V2 w h)
  copy renderer texture Nothing (Just dest)
