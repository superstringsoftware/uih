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
    type Res SDLIO BasicWidget = Maybe Texture
    -- render :: Box -> SDLIO Texture
    render (Box (CollRect x y w h) clr) = do 
        tex <- getRenderer >>= boxToTexture (fromIntegral w) (fromIntegral h) clr
        pure $ Just tex
    render (TextLabel (CollRect x y w h) clr fname text) = 
        if text /= "" then do
            (Just fnt) <- getDefaultFont -- WRONG, HANDLE ERRORS!!! GET THE RIGHT FONT!!!
            ren <- getRenderer
            tex <- textToTexture text ren clr fnt
            pure $ Just tex
        else pure Nothing
    render btn@Button { coll = (CollRect x y w h) } = do
        ren  <- getRenderer
        tex1 <- boxToTexture (fromIntegral w) (fromIntegral h) (bgColor (btn::BasicWidget)) ren
        let tx1 = text btn
        -- liftIO $ putStrLn $ "Rendering button; text: " ++ show tx
        let tx = if tx1 == "" then " " else tx1
        (Just fnt) <- getDefaultFont -- WRONG, HANDLE ERRORS!!! GET THE RIGHT FONT!!!
        tex2 <- textToTexture tx ren (fontColor btn) fnt
        copyTextureClip tex1 (fromIntegral $ txtX btn) (fromIntegral $ txtY btn) tex2 ren True
        pure $ Just tex1
    
    renderScreen w = do
        texm <- render w
        maybe (pure ())
              (\tex -> do
                    ren <- getRenderer
                    let coll = getCollider w
                    renderTexture (fromIntegral $ x (coll::Collider) ) (fromIntegral $ y (coll::Collider)) tex ren
                    liftIO $ destroyTexture tex -- NEED TO CACHE EVENTUALLY!!!
              ) texm
        
 

