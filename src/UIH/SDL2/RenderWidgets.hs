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

instance Renderable SDLIO BasicWidget where
    type Res SDLIO BasicWidget = Texture
    -- render :: Box -> SDLIO Texture
    render (Box (CollRect x y w h) clr) = getRenderer >>= boxToTexture (fromIntegral w) (fromIntegral h) clr
    render (TextLabel (CollRect x y w h) clr fname text) = do
        (Just fnt) <- getDefaultFont -- WRONG, HANDLE ERRORS!!!
        ren <- getRenderer
        textToTexture text ren clr fnt
    render btn@Button { coll = (CollRect x y w h) } = do
        ren  <- getRenderer
        tex1 <- boxToTexture (fromIntegral w) (fromIntegral h) (bgColor (btn::BasicWidget)) ren
        (Just fnt) <- getDefaultFont -- WRONG, HANDLE ERRORS!!!
        tex2 <- textToTexture (text btn ) ren (fontColor btn) fnt
        copyTextureClip tex1 (fromIntegral $ txtX btn) (fromIntegral $ txtY btn) tex2 ren True
        pure tex1
    renderScreen w = do
        tex <- render w
        ren <- getRenderer
        let coll = getCollider w
        renderTexture (fromIntegral $ x (coll::Collider) ) (fromIntegral $ y (coll::Collider)) tex ren
        liftIO $ destroyTexture tex -- NEED TO CACHE EVENTUALLY!!!
        return ()
 

