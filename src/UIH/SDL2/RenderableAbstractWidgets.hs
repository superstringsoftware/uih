{-# LANGUAGE OverloadedStrings, 
    RecordWildCards,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses,
    TypeFamilies
#-}
module UIH.SDL2.RenderableAbstractWidgets where

-- using Renderable to convert AbstractWidgets to SDLWidgets and then will use Renderable to convert SDL Widgets to
-- textures. Stumbled on this sort of by accident but seems quite nice to use typeclass like this.

import UIH.UI.Renderable
import UIH.UI.AbstractWidgets
import UIH.SDL2.SDLWidgets
import UIH.SDL2.RenderMonad

import Control.Monad.Trans.State.Strict
import Control.Exception

import SDL.Font

import Data.Map.Strict as Map
import Linear

import Color

-- convert font data to cached font
fontData2Font :: FontData -> SDLIO (Maybe Font)
fontData2Font FontDataDefault = get >>= \st -> return $ Map.lookup "__DEFAULT__" (loadedFonts st)
fontData2Font FontData{..} = return Nothing

fontData2Color :: FontData -> Color.Color
fontData2Color FontDataDefault = V4 255 255 255 0
fontData2Color FontData{..} = fontColor

rect2CInt (V4 x y w h) = V4 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

background2Color (BGColor clr) = clr
background2Color _ = rgbaToV4Color $ mdRed 500

{-
data FontData = FontData {
    fontName :: !Text,
    fontSize :: !Int,
    fontStyle :: FontStyle,
    fontColor :: Color
} | FontDataDefault
-}

instance Renderable SDLIO AbstractWidget where
    type Res SDLIO AbstractWidget = SDLWidget
    -- render :: AbstractWidget -> SDLIO SDLWidget
    render w@Panel{..} = return SDLBox { -- simply a colored box (eventually need to add with an image)
            bgColor = background2Color background,
            cachedRect = rect2CInt cacheRect, -- cached bounding box dimensions
            tex = Nothing -- cached texture
        }
    render w@InputText{..} = do
        maybeFont <- fontData2Font fontData
        maybe (throw (NoMethodError "Couldn't load default font, aborting!"))
              (\fnt -> return SDLTextBox { -- text with box as a background
                    text = text,
                    font = fnt, 
                    color = fontData2Color fontData,
                    cachedRect = rect2CInt cacheRect, -- cached bounding box dimensions
                    bgColor = background2Color background, -- background color for the box
                    paddingRect = V4 8 4 8 4, -- padding for the text texture relative to the bounding box
                    tex = Nothing
                }) maybeFont
    
    render w@Label{..} = do
        maybeFont <- fontData2Font fontData
        maybe (throw (NoMethodError "Couldn't load default font, aborting!"))
              (\fnt -> return SDLTextBox { -- text with box as a background
                    text = text,
                    font = fnt, 
                    color = fontData2Color fontData,
                    cachedRect = rect2CInt cacheRect, -- cached bounding box dimensions
                    bgColor = background2Color background, -- background color for the box
                    paddingRect = V4 8 4 8 4, -- padding for the text texture relative to the bounding box
                    tex = Nothing
                }) maybeFont
    
