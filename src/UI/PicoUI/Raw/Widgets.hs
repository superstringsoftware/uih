{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards #-}
module UI.PicoUI.Raw.Widgets where

-- 
import Data.Text
import Foreign.C.Types (CInt)
import Data.Word
import SDL hiding (Vector)
import SDL.Font

import Data.Vector
import qualified Data.Vector.Storable as S
import Data.IORef

import UI.PicoUI.Charts.DataSeries

import Color

type WidgetId = Int

-- data RawSeriesType = RSLines | RSDots deriving (Show, Eq)

-- represents styled piece of text
-- font data is provided separately
data SDLStyledText = SDLStyledText {
    text :: !Text,
    color :: V4 Word8,
    styles :: [Style],
    -- if not empty, we use the "shaded" rendering method to produce backround box in one go
    -- can be used to highlight the text etc
    bgColor :: Maybe (V4 Word8) 
} deriving Show

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
} | SDLSeriesLines {
    color :: V4 Word8,
    points :: S.Vector (Point V2 CInt),
    width :: !CInt
} deriving Show

isSDLBox SDLBox{..} = True
isSDLBox _ = False
isSDLText SDLText{..} = True
isSDLText _ = False
isSDLTextLine SDLTextLine{..} = True
isSDLTextLine _ = False

-- elements to render in turn + shift from the parent in terms of position
data WidgetElement = WidgetElement {
    el :: !SDLElement,
    offset :: V2 CInt
} deriving Show

data Widget = Widget {
    isVisible :: Bool,
    collider :: V4 CInt, -- bounding box
    elements :: Vector WidgetElement -- list of elements
} deriving Show

isInWidget x y widg = 
    let (V4 a b w h) = collider widg
    in  ( (x>a) && (x < a + w) && (y>b) && (y<b+h) )



