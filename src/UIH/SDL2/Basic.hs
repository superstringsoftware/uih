-- basic building blocks of the rendering engine are just 2:
-- rectangle with either color filling or image filling
-- text string
-- higher level components are built from combinations of these 2

module UIH.SDL2.Basic where

import SDL.Font (Font, solid, blended)
import Foreign.C.Types (CInt)
import SDL
import Data.Text (Text)
import Control.Monad.IO.Class
import Data.Word

boxToTexture :: MonadIO m => CInt -> CInt -> V4 Word8 -> Renderer -> m Texture
boxToTexture width height color ren = do
    -- types of textures to be created need to be configured somehow
    tex <- createTexture ren RGBA8888 TextureAccessTarget (V2 width height)
    rendererRenderTarget ren $= Just tex
    rendererDrawColor ren $= color
    fillRect ren Nothing
    return $ tex
  
-- rendering given text to texture with a given font
textToTexture :: MonadIO m => Text -> Renderer -> V4 Word8 -> Font -> m Texture
textToTexture txt ren color font = do
    tsurf <- blended font color txt
    tex   <- createTextureFromSurface ren tsurf
    freeSurface tsurf
    return tex

-- render a given texture at x y coordinates ON SCREEN
renderTexture :: MonadIO m => CInt -> CInt -> Texture -> Renderer -> m ()
renderTexture x y texture renderer = do
    ti <- queryTexture texture
    -- liftIO $ putStrLn $ "Size is " ++ (show ti) ++ " x,y: " ++ show x ++ ", " ++ show y
    let w = textureWidth ti
    let h = textureHeight ti
    let dest = Rectangle (P (V2 x y)) (V2 w h)
    copy renderer texture Nothing (Just dest)