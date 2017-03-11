{-# LANGUAGE OverloadedStrings #-}
module SDLText where

import SDL.TTF as TTF
import qualified SDL.Raw as Raw
import SDL as SDL

import SDL.Vect

defaultFontPath :: String
defaultFontPath = "/home/aantich/dev/dropbox/Haskell/uih/ARIAL.TTF"

defaultFont size = TTF.openFont defaultFontPath size

createTextTexture text color font renderer = do
    textSurface <- TTF.renderUTF8Blended font text color
    textTexture <- SDL.createTextureFromSurface renderer textSurface
    SDL.freeSurface textSurface
    return textTexture

renderTexture x y texture renderer = do
    ti <- queryTexture texture
    -- putStrLn $ "Size is " ++ (show ti)
    let w = textureWidth ti
    let h = textureHeight ti
    let dest = Rectangle (P (V2 x y)) (V2 w h)
    copy renderer texture Nothing (Just dest)
