{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}

-- Fonts handling via sdl2-ttf
module SDL.Fonts where

import Control.Monad.Trans.State.Strict 
import Control.Monad.IO.Class (liftIO)

import SDL.Font
--import qualified SDL.Raw as Raw
--import SDL as SDL
--import SDL.Internal.Types
--import SDL.Vect

import Data.Text hiding (copy)
import Control.Exception
import SDL.Exception

import SDL.SDLIO

import qualified Data.Map.Strict as Map

--defaultFontPath :: Text
defaultFontPath = "./Roboto-Light.ttf"

defaultFont :: Int -> IO Font
defaultFont size = load defaultFontPath size

-- creates text texture from a given String
{-
createTextTexture :: String -> RGBA -> TTFFont -> Renderer -> IO Texture
createTextTexture text color font renderer = do
    textSurface <- renderUTF8Blended font text $ rgbaToSDLColor color
    textTexture <- createTextureFromSurface renderer textSurface
    freeSurface textSurface
    return textTexture
-}

initDefaultFont = do r <- try $ (initialize >> defaultFont 32)
                     case r of
                        Left  e   -> print (e::SDLException) >> fail "Could not initialize TTF fonts!"
                        Right fnt -> return fnt

initFonts :: SDLIO ()
initFonts = do 
    fnt <- liftIO initDefaultFont
    st <- get
    let fonts = Map.insert "__DEFAULT__" fnt (loadedFonts st)
    let stNew = st {loadedFonts = fonts}
    put stNew
    liftIO $ print $ show stNew
