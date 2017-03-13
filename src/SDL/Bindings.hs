{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}
module SDL.Bindings where

import Control.Monad.IO.Class

import SDL.TTF
import SDL.TTF.FFI (TTFFont)
import qualified SDL.Raw as Raw
import SDL as SDL
import SDL.Internal.Types

import Foreign.C.Types (CInt)

import SDL.Vect
import Color
import CSS.Box

-- setting Renderer Draw color based on RGBA values we define in Color
setRenderDrawColorRGBA ::  MonadIO m => Renderer -> RGBA -> m CInt
setRenderDrawColorRGBA (Renderer ren) rgba = Raw.setRenderDrawColor ren (r rgba) (g rgba) (b rgba) (a rgba)

rgbaToSDLColor :: RGBA -> Raw.Color
rgbaToSDLColor c = Raw.Color (r c) (g c) (b c) (a c)

defaultFontPath :: String
defaultFontPath = "/home/aantich/dev/dropbox/Haskell/uih/ARIAL.TTF"

defaultFont :: Int -> IO TTFFont
defaultFont size = openFont defaultFontPath size

-- creates text texture from a given String
createTextTexture :: String -> RGBA -> TTFFont -> Renderer -> IO Texture
createTextTexture text color font renderer = do
    textSurface <- renderUTF8Blended font text $ rgbaToSDLColor color
    textTexture <- createTextureFromSurface renderer textSurface
    freeSurface textSurface
    return textTexture

-- render a given texture at x y coordinates
renderTexture :: CInt -> CInt -> Texture -> Renderer -> IO ()
renderTexture x y texture renderer = do
    ti <- queryTexture texture
    -- putStrLn $ "Size is " ++ (show ti)
    let w = textureWidth ti
    let h = textureHeight ti
    let dest = Rectangle (P (V2 x y)) (V2 w h)
    copy renderer texture Nothing (Just dest)

renderBox :: CInt -> CInt -> Box -> Renderer -> IO ()
renderBox x y box ren = do
    let w = fromIntegral $ width (box::Box)
    let h = fromIntegral $ height box

    -- drawing borders
    let topWidth = convertBorderWidth (borderTop box)
    let bottomWidth = convertBorderWidth (borderBottom box)
    renderBorder x y w (borderTop box) ren borderRectangleTop
    renderBorder x (y+h) w (borderBottom box) ren borderRectangleBottom
    renderBorder (x+w) (y-topWidth) (h+bottomWidth+topWidth) (borderRight box) ren borderRectangleRight
    renderBorder x (y-topWidth) (h+bottomWidth+topWidth) (borderLeft box) ren borderRectangleLeft
    
    -- draw main box
    setRenderDrawColorRGBA ren $ color (box :: Box)
    let rect = Rectangle (P (V2 x y)) (V2 w h)
    fillRect ren (Just rect)

-- render horizontal border, x y - coordinates of the start of the border, l - length.
-- receives one of borderRectangleX functions to setup a correct rectangle
-- renderBorder :: CInt -> CInt -> CInt -> Maybe Border -> Renderer -> IO ()
renderBorder x y l border ren recFunc = do
    -- drawing border
    if ( border /= Nothing) then do
        let (Just brd) = border
        let bw = fromIntegral $ width (brd :: Border)
        let r = recFunc x y bw l
        setRenderDrawColorRGBA ren $ color (brd :: Border)
        fillRect ren (Just r)
    else do return ()

-- generate rectangles based on the border length, width and starting point
borderRectangleTop x y bw l =       Rectangle (P (V2 x (y-bw))) (V2 l bw)
borderRectangleBottom x y bw l =    Rectangle (P (V2 x y)) (V2 l bw)
borderRectangleLeft x y bw l =      Rectangle (P (V2 (x-bw) y)) (V2 bw l)
borderRectangleRight x y bw l =     Rectangle (P (V2 x y)) (V2 bw l)


{-
renderBoxShadow :: CInt -> CInt -> Box -> Renderer -> IO ()
renderBoxShadow x y box (Renderer ren) = do
    if ( (shadow box) != Nothing ) then
        let (Just sh) = shadow box
    else
            return ()
-}
