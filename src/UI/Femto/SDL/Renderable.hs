{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards, MultiParamTypeClasses, ExistentialQuantification #-}
module UI.Femto.SDL.Renderable where

-- SDL Rendering in the IO monad!    

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad
import Control.Monad.Trans.State.Strict

import SDL hiding (el, Vector)
import SDL.Font

import PreludeFixes
import Foreign.C.Types (CInt)
import Foreign.Ptr(nullPtr, Ptr)

import Data.Text
import Data.Word
import Data.Vector
import qualified Data.Vector.Storable as SV

import Data.Vector (foldM')

class (Functor m, MonadIO m) => Renderable w m where
    render         :: Renderer -> w -> m Texture
    renderToScreen :: Renderer -> w -> V2 CInt -> m ()
    -- Default implementation is wrong - need to construct a rectangle into which we should render
    renderToScreen ren wid coords = render ren wid >>= \t -> SDL.copy ren t Nothing Nothing

data SDLStyledText = SDLStyledText {
    text :: !Text,
    color :: V4 Word8,
    styles :: [Style],
    -- if not empty, we use the "shaded" rendering method to produce backround box in one go
    -- can be used to highlight the text etc
    bgColor :: Maybe (V4 Word8) 
} deriving Show

-- creating a Texture from a given styled text with a given font
styledText2Texture :: SDLStyledText -> Font -> Renderer -> IO Texture
styledText2Texture st font ren = do
    surf <- styledText2Surface st font
    -- tex <- runUnscaled (flip createTextureFromSurface surf) ren
    tex  <- createTextureFromSurface ren surf
    freeSurface surf >> return tex
    where styledText2Surface SDLStyledText{..} font = do
            setStyle font styles
            let text' = if text == "" then " " else text
            maybe (blended font color text') (\bgClr -> shaded font color bgClr text') bgColor

data RWTextLine = RWTextLine { -- text line with different styles but the same font and size
    font :: Font,
    texts :: Vector SDLStyledText,
    cursorPos :: !Int
} deriving Show

data RWSimpleTextLine = RWSimpleTextLine { -- text line with different styles but the same font and size
    font :: Font,
    text :: SDLStyledText,
    cursorPos :: !Int
} deriving Show

instance Renderable RWSimpleTextLine IO where
    render ren RWSimpleTextLine{..} = styledText2Texture text font ren


-- helper function to create a rectangle based on the texture
rectangleFromTexture :: Texture -> CInt -> CInt -> IO (Rectangle CInt)
rectangleFromTexture tex x y = do
    ti <- queryTexture tex
    let w = textureWidth ti
    let h = textureHeight ti
    return $ Rectangle (P (V2 x y)) (V2 w h)

-- Widgets as existentials?

data CacheableElement = forall w. CacheableElement {
    _widget :: w -- existential widget type
  , _eventHandler :: SDL.Event -> w -> w -- event handler that transforms our state (w) based on SDL Events
  , _render :: w -> Renderer -> IO Texture -- render our state into a texture
  , isDirty :: Bool -- do we need to rerender?
  , id :: !Text -- global ID (Do we need it at the element level???)
  , texCache :: IO Texture -- cached texture that we use for rendering as needed
}

-- updates the element based on the event
handleElementEvent :: SDL.Event -> CacheableElement -> CacheableElement
handleElementEvent ev (CacheableElement w eh r isd id1 tc) = CacheableElement {
        _widget = eh ev w,
        _eventHandler = eh,
        _render = r,
        isDirty = True, -- needs to change depending on the result of running the event handler
        id = id1,
        texCache = tc
    }

-- updates the texture cache
renderElement :: Renderer -> CacheableElement -> CacheableElement
renderElement ren ce@CacheableElement{..} = if not isDirty then ce else ce { texCache = _render _widget ren, isDirty = False }

-- making an element out of a textline
mkRWSimpleTextLine :: RWSimpleTextLine -> Text -> CacheableElement 
mkRWSimpleTextLine rws ide = CacheableElement {
        _widget = rws
      , _eventHandler = \ev w -> w
      , _render = \RWSimpleTextLine{..} ren -> styledText2Texture text font ren
      , isDirty = True
      , id = ide
      , texCache = undefined -- ok, this is not very nice, but we are not using Maybe for performance reasons. After first call to render, this will be initialized.
    }