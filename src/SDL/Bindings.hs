{-# LANGUAGE OverloadedStrings, DuplicateRecordFields  #-}
module SDL.Bindings where

import Control.Monad.Trans.State.Strict 
import Control.Monad.IO.Class

--import SDL.TTF
--import SDL.TTF.FFI (TTFFont)
import qualified SDL.Raw as Raw
import SDL as SDL
import SDL.Internal.Types

import Foreign.C.Types (CInt)

import SDL.Vect
import Color
import CSS.Box

import GHC.Prim

import Screen.RawWidgets

import SDL.SDLIO
import SDL.Font (Font, solid, blended)
import SDL.Fonts

-- import Data.Text hiding (copy)

-- UGLY UGLY HACK!!! need to figure out how to handle state
--globalFont = defaultFont 32

-- every graphics binding should define Renderable class and instances for all widgets
-- this is for SDL
class Renderable a where
    render :: Renderer -> a -> CInt -> CInt -> SDLIO ()
    renderGlobal :: a -> SDLIO ()

instance Renderable Widget where
    renderGlobal (WPanel p) = renderGlobal p
    renderGlobal (WTextLabel t) = renderGlobal t


instance Renderable TextLabel where
    render renderer tl x y = do
        --putStrLn "Inside render TextLabel"
        --dumpSDLState
        fnt <- getDefaultFont 
        -- print fnt
        case fnt of
            Just font -> do
                            let box = textBox tl
                            tsurf <- liftIO $ blended font (rgbaToV4Color $ color (box::Box) ) (text (tl::TextLabel)) 
                            tex <- liftIO $ createTextureFromSurface renderer tsurf 
                            liftIO $ renderTexture x y tex renderer
            Nothing -> liftIO $ print "Couldn't find font when rendering TextLabel"
    
    renderGlobal tl = do    ren <- gets mainRenderer
                            let box = textBox tl
                            let x = (fromIntegral $ globalX box)
                            let y = (fromIntegral $ globalY box)
                            render ren tl x y


instance Renderable Box where
    render ren box x y = liftIO $ do
        let w = fromIntegral $ width (box::Box)
        let h = fromIntegral $ height box
        -- draw main box
        setRenderDrawColorRGBA ren $ color (box :: Box)
        let rect = Rectangle (P (V2 x y)) (V2 w h)
        fillRect ren (Just rect)
    
    renderGlobal box = do 
        ren <- gets mainRenderer 
        render ren box (fromIntegral $ globalX box) (fromIntegral $ globalY box)

instance Renderable Panel where
    render ren pan x y = liftIO $ do
        let bx = box pan
        let w = fromIntegral $ width (bx::Box)
        let h = fromIntegral $ height bx

        -- drawing borders
        let topWidth = convertBorderWidth (borderTop pan)
        let bottomWidth = convertBorderWidth (borderBottom pan)
        renderBorder x y w (borderTop pan) ren borderRectangleTop
        renderBorder x (y+h) w (borderBottom pan) ren borderRectangleBottom
        renderBorder (x+w) (y-topWidth) (h+bottomWidth+topWidth) (borderRight pan) ren borderRectangleRight
        renderBorder x (y-topWidth) (h+bottomWidth+topWidth) (borderLeft pan) ren borderRectangleLeft

        -- draw main box
        setRenderDrawColorRGBA ren $ color (bx :: Box)
        let rect = Rectangle (P (V2 x y)) (V2 w h)
        fillRect ren (Just rect)
    
    renderGlobal pan = gets mainRenderer >>= \ren -> (render ren pan (fromIntegral $ globalX $ box pan) (fromIntegral $ globalY $ box pan))

-- setting Renderer Draw color based on RGBA values we define in Color
setRenderDrawColorRGBA ::  MonadIO m => Renderer -> RGBA -> m CInt
setRenderDrawColorRGBA (Renderer ren) rgba = Raw.setRenderDrawColor ren (r rgba) (g rgba) (b rgba) (a rgba)

rgbaToSDLColor :: RGBA -> Raw.Color
rgbaToSDLColor c = Raw.Color (r c) (g c) (b c) (a c)


-- render a given texture at x y coordinates
renderTexture :: CInt -> CInt -> Texture -> Renderer -> IO ()
renderTexture x y texture renderer = do
    ti <- queryTexture texture
    -- putStrLn $ "Size is " ++ (show ti)
    let w = textureWidth ti
    let h = textureHeight ti
    let dest = Rectangle (P (V2 x y)) (V2 w h)
    copy renderer texture Nothing (Just dest)


-- render horizontal border, x y - coordinates of the start of the border, l - length.
-- receives one of borderRectangleX functions to setup a correct rectangle
-- renderBorder :: CInt -> CInt -> CInt -> Maybe Border -> Renderer -> IO ()
renderBorder x y l border ren recFunc = do
    -- drawing border
    if ( border /= Nothing) then do
        let (Just brd) = border
        let bw = fromIntegral $ width (brd :: Border)
        let r = recFunc x y bw l
        setRenderDrawColorRGBA ren $ color (brd :: Border)
        fillRect ren (Just r)
    else do return ()

-- generate rectangles based on the border length, width and starting point
borderRectangleTop x y bw l =       Rectangle (P (V2 x (y-bw))) (V2 l bw)
borderRectangleBottom x y bw l =    Rectangle (P (V2 x y)) (V2 l bw)
borderRectangleLeft x y bw l =      Rectangle (P (V2 (x-bw) y)) (V2 bw l)
borderRectangleRight x y bw l =     Rectangle (P (V2 x y)) (V2 bw l)


{-
renderBoxShadow :: CInt -> CInt -> Box -> Renderer -> IO ()
renderBoxShadow x y box (Renderer ren) = do
    if ( (shadow box) != Nothing ) then
        let (Just sh) = shadow box
    else
            return ()
-}
