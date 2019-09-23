{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , TypeSynonymInstances
    , FlexibleInstances
    , DisambiguateRecordFields #-}

module UIH.SDL2.RenderableSDLWidgets where

-- implementations of Renderable for Widgets in the SDLIO monad

import UIH.UI.Renderable
import UIH.SDL2.RenderMonad
import UIH.SDL2.Basic
import UIH.SDL2.Fonts
import UIH.SDL2.SDLWidgets

import Control.Monad.IO.Class (liftIO)

import SDL

import PreludeFixes

{-
-- low level SDL widgets used for caching
data SDLWidget = SDLBox { -- simply a colored box (eventually need to add with an image)
    bgColor :: V4 Word8,
    cachedRect :: V4 CInt, -- cached bounding box dimensions
    tex :: Maybe Texture -- cached texture
} | SDLText { -- text without any background
    text :: Text,
    font :: Font, -- SDL font object to render with
    color :: V4 Word8, -- color to render text with
    cachedRect :: V4 CInt, -- cached bounding box dimensions
    tex :: Maybe Texture -- cached texture
} | SDLTextBox { -- text with box as a background
    text :: Text,
    font :: Font, -- SDL font object to render with
    color :: V4 Word8, -- color to render text with
    cachedRect :: V4 CInt, -- cached bounding box dimensions
    bgColor :: V4 Word8, -- background color for the box
    paddingRect :: V4 CInt, -- padding for the text texture relative to the bounding box
    tex :: Maybe Texture -- cached texture
} 
-}
instance Renderable SDLIO [SDLWidget] where
    type Res SDLIO [SDLWidget] = Maybe Texture
    render [] = return Nothing
    render (x:[]) = render x
    render (x:xs) = render x -- WRONG, need to copy to the first texture!!!

    renderScreen [] = pure ()
    renderScreen (x:xs) = renderScreen x

instance Renderable SDLIO SDLWidget where
    type Res SDLIO SDLWidget = Maybe Texture
    render SDLBox{..} = do
        let (V4 x y w h) = cachedRect
        tex <- getRenderer >>= boxToTexture w h bgColor
        pure $ Just tex

    render SDLText{..} = do
        if text /= "" then do
            ren <- getRenderer
            tex <- textToTexture text ren color font
            pure $ Just tex
        else pure Nothing

    render SDLTextBox{..} = do
        ren <- getRenderer
        let (V4 x y w h) = cachedRect
        tex1 <- boxToTexture w h bgColor ren
        let tx = if text == "" then " " else text
        tex2 <- textToTexture tx ren color font
        let (V4 x1 y1 x2 y2) = paddingRect
        copyTextureClip tex1 x1 y1 tex2 ren True
        pure $ Just tex1

    renderScreen w = do
        texm <- render w
        maybe (pure ())
              (\tex -> do
                    ren <- getRenderer
                    let (V4 x y _ _) = cachedRect w
                    renderTexture x y tex ren
                    liftIO $ destroyTexture tex -- NEED TO CACHE EVENTUALLY!!!
              ) texm


        
 

