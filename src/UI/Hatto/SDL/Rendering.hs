{-#LANGUAGE RecordWildCards, OverloadedStrings #-}

module UI.Hatto.SDL.Rendering
where

-- Low level rendiring of widgets to SDL textures

import SDL hiding (el, Vector) 
import SDL.Font hiding (Color)
import Color
import Foreign.C.Types ( CInt )
import Data.Text
import UI.Hatto.Widgets

-- starting with Elements
rectangleFromTexture :: Texture -> V2 CInt -> IO (Rectangle CInt)
rectangleFromTexture tex point = do
    ti <- queryTexture tex
    let w = textureWidth ti
    let h = textureHeight ti
    return $ Rectangle (P point) (V2 w h)

-- helper method creating a texture of a given color and size from WidgetSkeleton
createTextureFromWSK :: Renderer -> PixelFormat -> WidgetSkeleton -> Color -> IO Texture
createTextureFromWSK ren pf WidgetSkeleton{..} bgc = do
    tex <- createTexture ren pf TextureAccessTarget size
    rendererRenderTarget ren $= Just tex
    clr <- SDL.get (rendererDrawColor ren)
    rendererDrawColor ren $= bgc
    clear ren
    rendererDrawColor ren $= clr
    rendererRenderTarget ren $= Nothing
    return tex

-- creating a Texture from a given styled text with a given font
createTextureFromTextElement :: Renderer -> Text -> TextStyle -> Font -> IO Texture
createTextureFromTextElement ren txt st font = do
    surf <- styledText2Surface txt st font
    -- tex <- runUnscaled (flip createTextureFromSurface surf) ren
    tex  <- createTextureFromSurface ren surf
    freeSurface surf >> return tex
    where styledText2Surface text TextStyle{..} font = do
            setStyle font fontStyle
            let text' = if text == "" then " " else text
            maybe (blended font fontColor text') (\bgClr -> shaded font fontColor bgClr text') bgColor
