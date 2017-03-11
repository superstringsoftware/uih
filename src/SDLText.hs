{-# LANGUAGE OverloadedStrings #-}
module SDLText where

import SDL.TTF
import SDL.TTF.FFI (TTFFont)
import qualified SDL.Raw as Raw
import SDL as SDL

import Foreign.C.Types (CInt)

import SDL.Vect

defaultFontPath :: String
defaultFontPath = "/home/aantich/dev/dropbox/Haskell/uih/ARIAL.TTF"

defaultFont :: Int -> IO TTFFont
defaultFont size = openFont defaultFontPath size

-- creates text texture from a given String
createTextTexture :: String -> Raw.Color -> TTFFont -> Renderer -> IO Texture
createTextTexture text color font renderer = do
    textSurface <- renderUTF8Blended font text color
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
