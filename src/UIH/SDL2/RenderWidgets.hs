{-# LANGUAGE OverloadedStrings, 
    DuplicateRecordFields, NamedFieldPuns, OverloadedLabels, RecordWildCards
    , MultiParamTypeClasses 
    , TypeFamilies
    , TypeSynonymInstances
    , FlexibleInstances #-}

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
    