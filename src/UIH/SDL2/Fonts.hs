{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards  #-}

-- Fonts handling via sdl2-ttf
module UIH.SDL2.Fonts where

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)

import SDL.Font
import Data.Text hiding (copy)
import Control.Exception
import SDL.Exception

import UIH.SDL2.RenderMonad

import qualified Data.Map.Strict as Map

import UIH.UI.AbstractWidgets

defaultFontPath = "./Roboto-Light.ttf"

defaultFont :: Int -> IO Font
defaultFont size = load defaultFontPath size

safeLoadFont path size = 
    do r <- try $ load path size
       either (\e -> print (e::SDLException) >> return Nothing)
              (\fnt -> return $ Just fnt) r
                      


getDefaultFont :: SDLIO (Maybe Font)
getDefaultFont = get >>= \st -> return $ Map.lookup "__DEFAULT__" (loadedFonts st)

initFonts :: SDLIO ()
initFonts = do
    fnt <- liftIO initDefaultFont
    st <- get
    let fonts = Map.insert "__DEFAULT__" fnt (loadedFonts st)
    put st {loadedFonts = fonts}
    where initDefaultFont = do   
            r <- try $ (initialize >> defaultFont 16)
            either (\e -> print (e::SDLException) >> fail "Could not initialize TTF fonts!")
                   (\font -> return font) r
                                        
