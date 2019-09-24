-- basic building blocks of the rendering engine are just 2:
-- rectangle with either color filling or image filling
-- text string
-- higher level components are built from combinations of these 2

module UIH.SDL2.Basic where

import SDL.Font (Font, solid, blended, shaded)
import Foreign.C.Types (CInt)
import SDL
import Data.Text (Text)
import Control.Monad.IO.Class
import Data.Word

emptyTexture size ren = createTexture ren RGBA8888 TextureAccessTarget size

boxToTexture :: MonadIO m => CInt -> CInt -> V4 Word8 -> Renderer -> m Texture
boxToTexture width height color ren = do
    -- types of textures to be created need to be configured somehow
    tex <- createTexture ren RGBA8888 TextureAccessTarget (V2 width height)
    rendererRenderTarget ren $= Just tex
    rendererDrawColor ren $= color
    fillRect ren Nothing
    return $ tex

-- shaded :: MonadIO m => Font -> Color -> Color -> Text -> m Surface
textToTextureShaded :: MonadIO m => Text -> Renderer -> V4 Word8 -> V4 Word8 -> Font -> m Texture
textToTextureShaded txt ren color bgColor font = do
    tsurf <- shaded font color bgColor txt 
    tex   <- createTextureFromSurface ren tsurf
    freeSurface tsurf >> return tex


-- rendering given text to texture with a given font
textToTexture :: MonadIO m => Text -> Renderer -> V4 Word8 -> Font -> m Texture
textToTexture txt ren color font = do
    tsurf <- blended font color txt
    tex   <- createTextureFromSurface ren tsurf
    freeSurface tsurf >> return tex

-- render a given texture at x y coordinates ON SCREEN
renderTexture :: MonadIO m => CInt -> CInt -> Texture -> Renderer -> m ()
renderTexture x y texture renderer = do
    ti <- queryTexture texture
    -- liftIO $ putStrLn $ "Size is " ++ (show ti) ++ " x,y: " ++ show x ++ ", " ++ show y
    let w = textureWidth ti
    let h = textureHeight ti
    let dest = Rectangle (P (V2 x y)) (V2 w h)
    copy renderer texture Nothing (Just dest)

-- copy source texture to target texture at x y coords, CLIPPING source texture if it's bigger
-- DELETES source texture if delete = True
copyTextureClip :: MonadIO m => Texture -> CInt -> CInt -> Texture -> Renderer -> Bool -> m Texture
copyTextureClip target x y source renderer delete = do
    tiT <- queryTexture target
    tiS <- queryTexture source
    -- liftIO $ putStrLn $ "Size is " ++ (show ti) ++ " x,y: " ++ show x ++ ", " ++ show y
    let wT = textureWidth tiT
    let hT = textureHeight tiT
    let wS = textureWidth tiS
    let hS = textureHeight tiS
    -- setting up correct source and destination rectangle sizes
    let recW = if (x + wS) > wT then wT - x else wS
    let recH = if (y + hS) > hT then hT - y else hS
    let destR = Rectangle (P (V2 x y)) (V2 recW recH)
    let srcR  = Rectangle (P (V2 0 0)) (V2 recW recH)
    rendererRenderTarget renderer $= Just target -- setting background box as render target
    copy renderer source (Just srcR) (Just destR)
    rendererRenderTarget renderer $= Nothing -- resetting render target back to screen
    if delete then destroyTexture source else pure ()
    return target
    