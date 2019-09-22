{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , TypeSynonymInstances
    , FlexibleInstances
    , DisambiguateRecordFields #-}

module UIH.SDL2.RenderWidgets where

-- implementations of Renderable for Widgets in the SDLIO monad

import UIH.UI.Widgets
import UIH.SDL2.RenderMonad
import UIH.SDL2.Basic
import UIH.SDL2.Fonts

import Control.Monad.IO.Class (liftIO)

import SDL

instance Renderable SDLIO Box where
    type Res SDLIO Box = Texture
    -- render :: Box -> SDLIO Texture
    render (Box (CollRect x y w h) clr) = getRenderer >>= boxToTexture (fromIntegral w) (fromIntegral h) clr
    renderScreen box@(Box (CollRect x y w h) clr) = do
        tex <- render box
        ren <- getRenderer
        renderTexture (fromIntegral x) (fromIntegral y) tex ren
        liftIO $ destroyTexture tex -- NEED TO CACHE EVENTUALLY!!!
        return box
 
instance Renderable SDLIO TextLabel where
    type Res SDLIO TextLabel = Texture
    render (TextLabel (CollRect x y w h) clr fname text) = do
        (Just fnt) <- getDefaultFont -- WRONG, HANDLE ERRORS!!!
        ren <- getRenderer
        textToTexture text ren clr fnt
    renderScreen tl@(TextLabel (CollRect x y w h) _ _ _) = do
        ren <- getRenderer
        tex <- render tl
        renderTexture (fromIntegral x) (fromIntegral y) tex ren
        liftIO $ destroyTexture tex -- NEED TO CACHE EVENTUALLY!!!
        return tl -- Need to update collider!
    
-- asf
instance Renderable SDLIO Button where
    type Res SDLIO Button = Texture
    -- render :: Box -> SDLIO Texture
    render btn@Button { coll = (CollRect x y w h) } = do
        ren  <- getRenderer
        tex1 <- boxToTexture (fromIntegral w) (fromIntegral h) (bgColor (btn::Button)) ren
        (Just fnt) <- getDefaultFont -- WRONG, HANDLE ERRORS!!!
        tex2 <- textToTexture (text (btn::Button) ) ren (fontColor btn) fnt
        copyTextureClip tex1 (fromIntegral $ txtX btn) (fromIntegral $ txtY btn) tex2 ren True
        pure tex1
    renderScreen btn@Button { coll = (CollRect x y w h) } = do
        ren <- getRenderer
        tex <- render btn
        renderTexture (fromIntegral x) (fromIntegral y) tex ren
        liftIO $ destroyTexture tex -- NEED TO CACHE EVENTUALLY!!!
        return btn -- Need to update collider!
