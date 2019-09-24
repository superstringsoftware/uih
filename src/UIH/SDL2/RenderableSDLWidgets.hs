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
import Control.Monad

import SDL
import SDL.Font

import PreludeFixes

{-
data SDLStyledText = SDLStyledText {
    text :: !Text,
    color :: V4 Word8,
    styles :: [Style],
    -- if not empty, we use the "shaded" rendering method to produce backround box in one go
    -- can be used to highlight the text etc
    bgColor :: Maybe (V4 Word8) 
}

-}
-- helper conversion functions
-- no check for empty string, but it MUSTNT BE EMPTY STRING!!!
styledText2Texture :: Font -> SDLStyledText -> SDLIO Texture
styledText2Texture font SDLStyledText{..} = do
    ren <- getRenderer
    -- rendering shaded or not depending on whether bgcolor is set
    tex <- maybe (textToTexture text ren color font)
                 (\bgClr -> textToTextureShaded text ren color bgClr font) bgColor
    return tex

styledText2SurfaceWDims font st = do
    surf <- styledText2Surface font st
    dims <- surfaceDimensions surf
    return (surf,dims)

styledText2Surface :: Font -> SDLStyledText -> SDLIO Surface
styledText2Surface font SDLStyledText{..} = do
    maybe (blended font color text)
          (\bgClr -> shaded font color bgClr text) bgColor
    >>= pure
    

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

    
                    

{-
SDLTextLine { -- text line with different styles but the same font and size
    font :: Font,
    cachedRect :: V4 CInt, -- cached bounding box dimensions
    tex :: Maybe Texture,
    texts :: [SDLStyledText]
}
-}

    renderScreen w = do
        texm <- render w
        maybe (pure ())
              (\tex -> do
                    ren <- getRenderer
                    let (V4 x y _ _) = cachedRect w
                    renderTexture x y tex ren
                    liftIO $ destroyTexture tex -- NEED TO CACHE EVENTUALLY!!!
              ) texm


        
 

