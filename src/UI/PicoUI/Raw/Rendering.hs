{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards #-}
module UI.PicoUI.Raw.Rendering where

-- SDL Rendering in the IO monad!    

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad
import Control.Monad.Trans.State.Strict

import SDL hiding (el)
import SDL.Font

import PreludeFixes

import UI.PicoUI.Raw.Widgets
import UI.PicoUI.Raw.WidgetCompiler (castV4)
import Foreign.C.Types (CInt)

import Data.Vector (foldM')

-- need our monad to handle correct scaling of fonts in case of high-dpi unfortunately
-- this has to be handled differently - e.g., choose specific rendering functions
-- in the beginning of the program depending on whether it's high dpi or not and then call them all the time
import UI.PicoUI.PicoUIMonad as P

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
-- cursor with timer: 
renderCursor ren = do
    cur <- gets cursor
    curTick <- ticks
    if curTick - prevTick cur < 500 then pure () 
    else modify' (\s -> s { cursor = cur { blink = not (blink cur), prevTick = curTick } })
    if not (blink cur) then pure () else do
        let rect = Just $ Rectangle (P $ V2 (x cur) (y cur)) (V2 2 (P.height cur))
        rendererDrawColor ren $= P.color cur
        fillRect ren rect
        

    
-- cut the texture based on offset and bounding rectangle dimensions
-- cutTexture tex xo yo rw rh = do 
-- castV2 (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

-- this method DOES NOT set renderer target
-- to the screen, it has to be done elsewhere
-- NO CACHING now, everything is very straightforward and naive
renderWidgetToScreen :: Widget -> Renderer -> SDLIO ()
renderWidgetToScreen Widget{..} ren = if not isVisible then pure () else do
    let (V4 x' y' w' h') = collider
    -- setting clipping rectangle to the bounding box of the element
    let rect = Rectangle (P $ V2 (fromIntegral x') (fromIntegral y')) (V2 (fromIntegral w') (fromIntegral h'))
    rendererClipRect ren $= Just rect
    mapM_ (fn1 x' y' w' h' ren) elements
    -- resetting clipping
    rendererClipRect ren $= Nothing
    where 
        fn1 :: CInt -> CInt -> CInt -> CInt -> Renderer -> WidgetElement -> SDLIO ()
        fn1 x y w h ren WidgetElement{..} = do
                let (V2 xo yo) = offset
                tex <- sdlElement2Texture (V2 w h) el ren
                -- textureBlendMode :: Texture -> StateVar BlendMode
                textureBlendMode tex $= BlendAlphaBlend
                -- unfortunately need to check between constructors here to correctly handle text rendering
                -- for high-dpi autoscaling environments
                case el of
                    SDLText{..}     -> renderTextureUnscaled (x+xo) (y+yo) tex ren
                    SDLTextLine{..} -> renderTextureUnscaled (x+xo) (y+yo) tex ren
                    _               -> renderTexture (x+xo) (y+yo) tex ren
                destroyTexture tex


runUnscaled func ren = do  
    autos <- gets autoScale
    scale <- gets scaleXY
    if autos then rendererScale ren $= V2 1 1 else pure ()
    rs <- SDL.get $ rendererScale ren
    liftIO $ putStrLn $ "Renderer scale is: " ++ show rs
    ret  <- func ren
    if autos then rendererScale ren $= scale else pure ()              
    return ret

sdlElement2Texture :: V2 CInt -> SDLElement -> Renderer -> SDLIO Texture
sdlElement2Texture size SDLBox{..} ren = do
    tex <- emptyTexture size ren
    rendererRenderTarget ren $= Just tex
    rendererDrawColor ren $= bgColor
    fillRect ren Nothing
    rendererRenderTarget ren $= Nothing
    return tex

sdlElement2Texture size SDLText{..} ren = do
    setStyle font []
    let text' = if text == "" then " " else text
    surf <- blended font color text'
    -- handling dpi scaling for fonts
    tex  <- createTextureFromSurface ren surf
    -- tex <- runUnscaled (flip createTextureFromSurface surf) ren
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
{-            
SDLSeriesLines {
    color :: V4 Word8,
    points :: S.Vector (Point V2 CInt),
    width :: !CInt
}
-}
-- width is ignored by now
sdlElement2Texture size SDLSeriesLines{..} renderer = do
    tex <- emptyTexture size renderer
    rendererRenderTarget renderer $= Just tex
    rendererDrawColor renderer $= V4 0 0 0 0
    clear renderer
    rendererDrawColor renderer $= color
    drawLines renderer points
    rendererRenderTarget renderer $= Nothing
    return tex 

-- 
{-
SDLSeriesBrokenLines { -- separate lines (used for charts mostly now, e.g. axis)
    color :: V4 Word8,
    lines :: Vector (Point V2 CInt, Point V2 CInt),
    width :: !CInt
} 
-}   
sdlElement2Texture size SDLSeriesBrokenLines{..} renderer = do
    tex <- emptyTexture size renderer
    rendererRenderTarget renderer $= Just tex
    rendererDrawColor renderer $= V4 0 0 0 0
    clear renderer
    rendererDrawColor renderer $= color
    mapM_ fn lines
    rendererRenderTarget renderer $= Nothing
    return tex 
    where fn (p1,p2) = drawLine renderer p1 p2

--
styledText2Surface :: SDLStyledText -> Font -> SDLIO Surface
styledText2Surface SDLStyledText{..} font = do
    setStyle font styles
    let text' = if text == "" then " " else text
    maybe (blended font color text')
          (\bgClr -> shaded font color bgClr text' ) bgColor
    
styledText2Texture :: SDLStyledText -> Font -> Renderer -> SDLIO Texture
styledText2Texture st font ren = do
    surf <- styledText2Surface st font
    -- tex <- runUnscaled (flip createTextureFromSurface surf) ren
    tex  <- createTextureFromSurface ren surf
    freeSurface surf >> return tex


emptyTexture size ren = createTexture ren RGBA8888 TextureAccessTarget size

-- render a given texture at x y coordinates ON SCREEN
renderTexture :: CInt -> CInt -> Texture -> Renderer -> SDLIO ()
renderTexture x y texture renderer = do
    ti <- queryTexture texture
    -- liftIO $ putStrLn $ "Size is " ++ (show ti) ++ " x,y: " ++ show x ++ ", " ++ show y
    let w = textureWidth ti
    let h = textureHeight ti
    let dest = Rectangle (P (V2 x y)) (V2 w h)
    copy renderer texture Nothing (Just dest)

renderTextureUnscaled :: CInt -> CInt -> Texture -> Renderer -> SDLIO ()
renderTextureUnscaled x y texture renderer = do
    autos <- gets autoScale
    scale@(V2 sx sy) <- gets scaleXY
    if autos then rendererScale renderer $= V2 1 1 else pure ()
    ti <- queryTexture texture
    -- liftIO $ putStrLn $ "Size is " ++ (show ti) ++ " x,y: " ++ show x ++ ", " ++ show y
    let w = textureWidth ti
    let h = textureHeight ti
    let x' = if autos then round ( fromIntegral x * sx) else x
    let y' = if autos then round ( fromIntegral y * sy) else y
    let dest = Rectangle (P (V2 x' y')) (V2 w h)
    copy renderer texture Nothing (Just dest)
    if autos then rendererScale renderer $= scale else pure ()
    