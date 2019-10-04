{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards #-}
module UI.PicoUI.Raw.WidgetCompiler where

-- 
import Data.Text as T
import Foreign.C.Types (CInt)
import Data.Word
import SDL hiding (Vector, get)
import SDL.Font
import Control.Monad.Trans.State.Strict

import Data.Vector as V hiding (mapM)
import Data.IORef

import UI.PicoUI.Raw.Widgets
import qualified UI.PicoUI.Middle.AbstractWidgets as Mid

import UI.PicoUI.PicoUIMonad as P

import Color

-- Compiler from abstract widgets

{-
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
} deriving Show

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
-}

compileAllWidgets :: SDLIO ()
compileAllWidgets = do 
    ws <- widgets <$> get
    ws' <- mapM fn ws
    modify' (\s -> s { widgets = ws'} )
    where fn w@ActiveWidget{..} = do
                cf <- gets curFocusId
                -- calling with True when a widget is in focus - to handle cursor correctly!!!
                cw <- if (widgetId == cf) then compile2Widget True widget else compile2Widget False widget 
                pure $ w { compiledWidget = cw }


castV4 (V4 x y w h) = V4 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

backgroundToColor (Mid.BGColor clr) = clr
backgroundToColor _ = mRed 500

-- converts fontdata to font - NO SIZE CHECKS NOW!!!
fontData2Font :: Mid.FontData -> SDLIO Font
fontData2Font Mid.FontDataDefault = getDefaultFont
fontData2Font Mid.FontData{..} = do
    fntm <- getFont fontName
    case fntm of
        (Just fnt) -> pure fnt
        Nothing -> do
            -- font with given name not found in loaded fonts, let's try to load:
            fntm1 <- safeLoadFont (unpack fontName) fontSize
            maybe getDefaultFont (\fn -> pure $ fn) fntm1

-- fontData2Color :: Mid.FontData -> Color
fontData2Color Mid.FontDataDefault = mWhite
fontData2Color fd = Mid.fontColor fd

-- calculates screen position for cursor drawing based on the font, text string *up to cursor position*, x / y offset
updateCursorPosition :: Font -> Text -> CInt -> CInt -> SDLIO ()
updateCursorPosition fnt txt x' y' = do
    (w', h') <- size fnt txt
    w <- fromIntegral <$> scaleFontSizeDown w'
    h <- fromIntegral <$> scaleFontSizeDown h'
    cur <- gets cursor
    modify' (\s-> s { cursor = cur { x = x' + w, y = y', P.height = h } })

updateCursorPositionExplicit x y h = gets cursor >>= \cur -> modify' (\s-> s { cursor = cur { x = x, y = y, P.height = h } })

compile2Widget :: Bool -> Mid.AbstractWidget -> SDLIO Widget
-- simple box with background
compile2Widget focus Mid.Panel{..} = pure $ Widget {
    isVisible = True,
    collider = castV4 cacheRect,
    elements = [
        WidgetElement {
            el = SDLBox (backgroundToColor background),
            offset = V2 0 0
        }
    ]
}
-- this is NOT good as we are calling fontdata2font each time, and that is a VERY expensive
-- operation, since we are reading from disk. Need to cache the font somehow - e.g., use recompile operation
-- that is much less expensive and does updates surgically?
-- Also, need flatter and simpler low level widget format since we are using this compiler approach, too much nesting now

-- simple label
compile2Widget focus Mid.Label {..} = do
    fnt <- fontData2Font fontData
    let coll@(V4 x y w h) = castV4 cacheRect
    (V2 tw th) <- sizeTextTexture fnt text
    let xoff = calcXOffset halign w tw
    let yoff = calcYOffset valign h th
    -- updating cursor position!!!
    let xc = if xoff + tw > w then (x + w) else if xoff < 0 then x else x + xoff + tw
    if focus then updateCursorPositionExplicit xc (y + yoff) th else pure ()
    -- updateCursorPosition fnt text (x + xoff) (y + yoff) else pure ()
    
    pure $ Widget {
                isVisible = True,
                collider = coll,
                elements = [
                    WidgetElement {
                        el = SDLBox (backgroundToColor background),
                        offset = V2 0 0
                    },
                    WidgetElement {
                        el = SDLText {
                            text = text,
                            font = fnt,
                            cursorPos = (T.length text),
                            color = fontData2Color fontData
                        },
                        offset = V2 xoff yoff
                    }
                ]
            }

-- ok, this approach for now proves quite flexible as we DON'T need to add basic widgets to handle a new 
-- more complex AbstractWidget
compile2Widget focus Mid.SimpleMultilineText{..} = do
    fnt <- fontData2Font fontData
    let clr = fontData2Color fontData
    let boxEl = WidgetElement {
            el = SDLBox (backgroundToColor background),
            offset = V2 0 0
        }
    -- check what the size of the line should be
    vsize <- lineSkip fnt >>= scaleFontSizeDown
    let lineEls = V.imap (fn fnt clr vsize) textLines
    pure $ Widget {
                isVisible = True,
                collider = castV4 cacheRect,
                elements = V.cons boxEl lineEls
            }
    where fn fnt' clr' vs i line = WidgetElement {
        el = SDLText {
            text = line,
            font = fnt',
            cursorPos = 0,
            color = clr'
        },
        offset = V2 8 (fromIntegral (i * vs + 4) )
    }
    
--  CenterAlign | LeftAlign | RightAlign 
sizeTextTexture fnt txt = do
    (w', h') <- size fnt txt
    w <- fromIntegral <$> scaleFontSizeDown w'
    h <- fromIntegral <$> scaleFontSizeDown h'
    return $ V2 w h

calcXOffset Mid.CenterAlign recW textW = round $ fromIntegral (recW - textW) / 2
calcXOffset _ _ _ = 8
calcYOffset Mid.CenterAlign recH textH = round $ fromIntegral (recH - textH) / 2
calcYOffset _ _ _ = 4