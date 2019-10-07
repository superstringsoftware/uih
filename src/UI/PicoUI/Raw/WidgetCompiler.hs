{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists, RecordWildCards,
    ScopedTypeVariables, RankNTypes, UndecidableInstances #-}
module UI.PicoUI.Raw.WidgetCompiler where

-- 
import Data.Text as T
import Foreign.C.Types (CInt)
import Data.Word
import SDL hiding (Vector, get)
import SDL.Font
import Control.Monad.Trans.State.Strict

import Data.Vector as V hiding (mapM, (!))
import Data.Vector.Unboxed as U (Unbox, Vector)
import Data.Vector.Storable as S (Vector, map, convert, generate) 
import Data.Vector.Generic as G hiding (mapM)

import Data.IORef

import UI.PicoUI.Raw.Widgets
import qualified UI.PicoUI.Middle.AbstractWidgets as Mid

import UI.PicoUI.Charts.DataSeries

import UI.PicoUI.PicoUIMonad as P


import Color

-- Compiler from abstract widgets

class CompilesToRaw a where
    compileToRawWidget :: Bool -> a -> SDLIO Widget
    isInFocus :: a -> Bool

instance CompilesToRaw Mid.AbstractWidget where
    compileToRawWidget = compile2Widget
    isInFocus = Mid.isWidgetInFocus


-- LEGACY: for non-reactive widgets
compileAllWidgets :: SDLIO ()
compileAllWidgets = do 
    ws <- widgets <$> get
    ws' <- mapM fn ws
    modify' (\s -> s { widgets = ws'} )
    where fn w@ActiveWidget{..} = do
                cf <- gets curFocusId
                -- calling with True when a widget is in focus - to handle cursor correctly!!!
                cw <- if widgetId == cf then compile2Widget True widget else compile2Widget False widget 
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
            -- return either loaded or default
            maybe getDefaultFont pure fntm1

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

{-
SDLSeriesBrokenLines { -- separate lines (used for charts mostly now, e.g. axis)
    color :: V4 Word8,
    lines :: Vector (Point V2 CInt, Point V2 CInt),
    width :: !CInt
}
-}
-- make axis lines for the chart
makeAxis xmin xmax xscreen ymin ymax yscreen width color = SDLSeriesBrokenLines {
    color = color,
    width = width,
    lines = [ (P $ V2 0 y0, P $ V2 xscreen y0)
            , (P $ V2 xscreen y0, P $ V2 (xscreen - 10) (y0 + 3))
            , (P $ V2 xscreen y0, P $ V2 (xscreen - 10) (y0 - 3))
            , (P $ V2 x0 0, P $ V2 x0 yscreen)
            , (P $ V2 x0 0, P $ V2 (x0 + 3) 10)
            , (P $ V2 x0 0, P $ V2 (x0 - 3) 10)
            ]
} where xs = fromIntegral xscreen / (xmax - xmin)
        ys = fromIntegral yscreen / (ymax - ymin)
        y0 = fromIntegral (yscreen - round ( (-ymin) * ys))
        x0 = fromIntegral $ round $ (-xmin) * xs



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

compile2Widget focus Mid.Chart{..} = do
    let coll@(V4 x y w h) = castV4 cacheRect
    pure $ Widget {
                isVisible = True,
                collider = coll,
                elements = [
                    WidgetElement {
                        el = SDLBox (backgroundToColor background),
                        offset = V2 0 0
                    },
                    WidgetElement {
                        --makeAxis xmin xmax w ymin ymax h 1 (mdBlueGray 100),
                        el = makeAxis (xmin dataSeries) (xmax dataSeries) w (ymin dataSeries) (ymax dataSeries) h 1 (mdBlueGray 300),
                        offset = V2 0 0
                    },
                    WidgetElement {
                        el = SDLSeriesLines {
                            color = mdAmber 700,
                            points = ds2storable dataSeries w h,
                            width = 1
                        },
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
    let xc  | xoff + tw > w = x + w
            | xoff < 0 = x
            | otherwise = x + xoff + tw
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
                            cursorPos = T.length text,
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

-- converting DataSeries into Storable Ints vector usable for rendering   
ds2storable :: (Unbox a, Unbox b, RealFrac a, RealFrac b) => DataSeriesN a b -> CInt -> CInt -> S.Vector (Point V2 CInt)
ds2storable DataSeriesN{..} xscreen yscreen = 
    let xs = fromIntegral xscreen / (xmax - xmin)
        ys = fromIntegral yscreen / (ymax - ymin)
        -- (vec :: U.Vector (Point V2 Int)) = G.map (\(x,y) -> P $ V2 (fromIntegral $ round $ x * xs) (fromIntegral (yscreen - round (y * ys))) ) dataPoints
        fn i = let (x,y) = dataPoints!i in P $ 
                    V2 (fromIntegral $ round $ (x - xmin) * xs) 
                       (fromIntegral (yscreen - round ( (y - ymin) * ys)))
    in  S.generate (G.length dataPoints) fn

--
        
