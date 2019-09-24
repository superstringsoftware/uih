{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards #-}
module UI.PicoUI.Raw.Rendering where

-- SDL Rendering in the IO monad!    

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad

import SDL hiding (el)
import SDL.Font

import PreludeFixes

import UI.PicoUI.Raw.Widgets
import Foreign.C.Types (CInt)

import Data.Vector (foldM')

{-
data WidgetElement = WidgetElement {
    el :: !SDLElement,
    offset :: V2 Int
}

data Widget = Widget {
    isVisible :: Bool,
    collider :: V4 CInt, -- bounding box
    elements :: Vector WidgetElement -- list of elements
}
-}

-- this method DOES NOT check any bounds and DOES NOT set renderer target
-- to the screen, it has to be done elsewhere
-- NO CACHING now, everything is very straightforward and naive
renderWidgetToScreen :: Widget -> Renderer -> IO ()
renderWidgetToScreen Widget{..} ren = if not isVisible then pure () else do
    let (V4 x y w h) = collider
    let (V2 xo yo) = (offset element)
    tex <- sdlElement2Texture (V2 w h) (el element) ren
    renderTexture (x+xo) (y+yo) tex ren
    destroyTexture tex
renderWidgetToScreen WidgetVec{..} ren = if not isVisible then pure () else do
    let (V4 x y w h) = collider
    mapM_ (fn1 x y w h ren) elements
    where 
        fn1 :: CInt -> CInt -> CInt -> CInt -> Renderer -> WidgetElement -> IO ()
        fn1 x y w h ren WidgetElement{..} = do
                let (V2 xo yo) = offset
                tex <- sdlElement2Texture (V2 w h) el ren
                renderTexture (x+xo) (y+yo) tex ren
                destroyTexture tex



sdlElement2Texture :: MonadIO m => V2 CInt -> SDLElement -> Renderer -> m Texture
sdlElement2Texture size SDLBox{..} ren = do
    tex <- emptyTexture size ren
    rendererRenderTarget ren $= Just tex
    rendererDrawColor ren $= bgColor
    fillRect ren Nothing
    rendererRenderTarget ren $= Nothing
    return $ tex
sdlElement2Texture size SDLText{..} ren = do
    setStyle font []
    surf <- blended font color text
    tex  <- createTextureFromSurface ren surf
    freeSurface surf >> return tex
-- folding textures from each styledtext into base sized texture
-- TODO: MAKE IT MORE EFFICIENT, no need to check sizes at each step etc!!!
sdlElement2Texture size SDLTextLine{..} renderer = do    
    tex <- emptyTexture size renderer
    rendererRenderTarget renderer $= Just tex
    foldM' foldWithTexture 0 texts
    rendererRenderTarget renderer $= Nothing
    return tex
    where 
        foldWithTexture x elem = do
            source <- styledText2Texture elem font renderer
            tiS <- queryTexture source
            -- liftIO $ putStrLn $ "Size is " ++ (show ti) ++ " x,y: " ++ show x ++ ", " ++ show y
            let V2 wT hT = size
            let wS = textureWidth tiS
            let hS = textureHeight tiS
            -- setting up correct source and destination rectangle sizes
            let recW = if (x + wS) > wT then wT - x else wS
            let recH = if hS > hT then hT else hS
            let destR = Rectangle (P (V2 x 0)) (V2 recW recH)
            let srcR  = Rectangle (P (V2 0 0)) (V2 recW recH)
            copy renderer source (Just srcR) (Just destR)
            destroyTexture source
            return (x+recW)

--
styledText2Surface :: MonadIO m => SDLStyledText -> Font -> m Surface
styledText2Surface SDLStyledText{..} font = do
    setStyle font styles
    maybe (blended font color text)
          (\bgClr -> shaded font color bgClr text ) bgColor
    
styledText2Texture :: MonadIO m => SDLStyledText -> Font -> Renderer -> m Texture
styledText2Texture st font ren = do
    surf <- styledText2Surface st font
    tex  <- createTextureFromSurface ren surf
    freeSurface surf >> return tex


emptyTexture size ren = createTexture ren RGBA8888 TextureAccessTarget size

-- render a given texture at x y coordinates ON SCREEN
renderTexture :: MonadIO m => CInt -> CInt -> Texture -> Renderer -> m ()
renderTexture x y texture renderer = do
    ti <- queryTexture texture
    -- liftIO $ putStrLn $ "Size is " ++ (show ti) ++ " x,y: " ++ show x ++ ", " ++ show y
    let w = textureWidth ti
    let h = textureHeight ti
    let dest = Rectangle (P (V2 x y)) (V2 w h)
    copy renderer texture Nothing (Just dest)
