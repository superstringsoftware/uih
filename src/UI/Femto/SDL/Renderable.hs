{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards, MultiParamTypeClasses #-}
module UI.Femto.SDL.Renderable where

-- SDL Rendering in the IO monad!    

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad
import Control.Monad.Trans.State.Strict

import SDL hiding (el, Vector)
import SDL.Font

import PreludeFixes
import Foreign.C.Types (CInt)

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



-- low level SDL widgets used for caching
data SDLElement = SDLBox { -- simply a colored box (eventually need to add with an image)
    bgColor :: V4 Word8
} | SDLText { -- text without any background
    text :: Text,
    font :: Font, -- SDL font object to render with
    color :: V4 Word8, -- color to render text with
    cursorPos :: !Int
} | SDLTextLine { -- text line with different styles but the same font and size
    font :: Font,
    texts :: Vector SDLStyledText,
    cursorPos :: !Int
} | SDLSeriesLines { -- connected lines (used for charts mostly now)
    color :: V4 Word8,
    points :: SV.Vector (Point V2 CInt),
    width :: !CInt
} | SDLSeriesBrokenLines { -- separate lines (used for charts mostly now, e.g. axis)
    color :: V4 Word8,
    lines :: Vector (Point V2 CInt, Point V2 CInt),
    width :: !CInt
} deriving Show