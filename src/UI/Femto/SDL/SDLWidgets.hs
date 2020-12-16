-- rendering of the Widgets from 'Middle'
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards, MultiParamTypeClasses, ExistentialQuantification #-}
module UI.Femto.SDL.SDLWidgets where

-- SDL Rendering in the IO monad!    

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad
import Control.Monad.Trans.State.Strict as Mon

import SDL as SDL hiding (el, Vector) 
import SDL.Font hiding (Color)

import PreludeFixes
import Foreign.C.Types (CInt)
import Foreign.Ptr(nullPtr, Ptr)

import Data.Text
import Data.Word
import Data.Vector
import qualified Data.Vector.Storable as SV

import UI.Femto.SDL.SDLMonad

import Data.Vector (foldM')

import Color

import UI.Femto.Middle.Widgets

-- cache for a Widget, plus events processing
data SDLWidget = SDLWidget {
    widget :: Widget,
    textureCache :: Maybe Texture,
    eventHandler :: Event -> FemtoUIM()
}

rectangleFromTexture :: Texture -> V2 CInt -> IO (Rectangle CInt)
rectangleFromTexture tex point = do
    ti <- queryTexture tex
    let w = textureWidth ti
    let h = textureHeight ti
    return $ Rectangle (P point) (V2 w h)

class SDLRenderable w where 
    render :: w -> FemtoUIM Texture
    -- default implementation of rendering to screen - render texture first then copy
    renderToScreen :: V2 CInt -> w -> FemtoUIM()
    renderToScreen point w = do
        tex <- render w
        st <- Mon.get
        let ren = mainRenderer st
        rect <- liftIO $ rectangleFromTexture tex point 
        SDL.copy ren tex Nothing (Just rect)


-- helper method creating a texture of a given color and size from WidgetSkeleton
createTextureFromWSK :: WidgetSkeleton -> Color -> FemtoUIM Texture
createTextureFromWSK WidgetSkeleton{..} bgc = do
    st <- Mon.get
    let ren = mainRenderer st
    tex <- createTexture ren (defaultPixelFormat st) TextureAccessTarget size
    rendererRenderTarget ren $= Just tex
    clr <- SDL.get (rendererDrawColor ren)
    rendererDrawColor ren $= bgc
    clear ren
    rendererDrawColor ren $= clr
    rendererRenderTarget ren $= Nothing
    return tex

-- rendering of individual WidgetElements
instance SDLRenderable WidgetElement where
    render (WEBox wsk (BGColor clr)) = createTextureFromWSK wsk clr
    render (WEBox wsk (BGImage img)) = createTextureFromWSK wsk (mdRed 500) -- image is not working yet!
    