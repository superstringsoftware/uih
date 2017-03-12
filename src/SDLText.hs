{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}
module SDLText where

import Control.Monad.IO.Class

import SDL.TTF
import SDL.TTF.FFI (TTFFont)
import qualified SDL.Raw as Raw
import SDL as SDL
import SDL.Internal.Types

import Foreign.C.Types (CInt)

import SDL.Vect
import Color
import Box

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

    -- drawing border
    if ( (border box) /= Nothing) then do
        let (Just brd) = border box
        let bw = fromIntegral $ width (brd :: Border)
        let r1 = Rectangle (P (V2 x (y-bw))) (V2 w bw) -- top border
        let r2 = Rectangle (P (V2 (x+w) (y-bw))) (V2 bw (h+bw+bw)) -- right border
        let r3 = Rectangle (P (V2 x (y+h))) (V2 w bw) -- bottom border
        let r4 = Rectangle (P (V2 (x-bw) (y-bw))) (V2 bw (h+bw+bw)) -- left border

        setRenderDrawColorRGBA ren $ color (brd :: Border)
        fillRect ren (Just r1)
        fillRect ren (Just r2)
        fillRect ren (Just r3)
        fillRect ren (Just r4)
    else do return ()

    -- draw main box
    setRenderDrawColorRGBA ren $ color (box :: Box)
    let rect = Rectangle (P (V2 x y)) (V2 w h)
    fillRect ren (Just rect)

{-
renderBoxShadow :: CInt -> CInt -> Box -> Renderer -> IO ()
renderBoxShadow x y box (Renderer ren) = do
    if ( (shadow box) != Nothing ) then
        let (Just sh) = shadow box
    else
            return ()
-}
