{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards, ExistentialQuantification #-}

module UI.Hatto.Widgets
where

import Data.Text
import Color

import Linear
import Foreign.C.Types (CInt)

import UI.Femto.Middle.Events

import qualified SDL

import Data.IORef
import Control.Monad.IO.Class (liftIO, MonadIO)

data TextStyle = TextStyle {
    fontName  :: Text
  , fontSize  :: !Int
  , fontColor :: Color
} deriving (Show, Eq)

data VAlign = VAlignTop  | VAlignMiddle | VAlignBottom deriving (Show, Eq)
data HAlign = HAlignLeft | HAlignMiddle | HAlignRight  deriving (Show, Eq)

-- types of backgrounds - color or image, maybe add gradient
data Background = BGColor Color | BGImage Text deriving (Show, Eq)

-- checks if (x,y) is inside the rectangle (x',y',w,h)
isInsideRectangle :: V2 CInt -> V2 CInt -> V2 CInt -> Bool
isInsideRectangle (V2 x y) (V2 x' y') (V2 w h) = (x >= x') && (x <= (x' + w)) && (y >= y') && (y <= (y'+h))

isInsideWSkeleton :: V2 CInt -> WidgetSkeleton -> Bool
isInsideWSkeleton point WidgetSkeleton{..} = isInsideRectangle point pos size

{-
isInsideWidget :: V2 CInt -> Widget -> Bool
isInsideWidget point Widget{..} = if element == WENone then False else isInsideWSkeleton point (wsk element) 
-}
-- Used to keep track of the info common to all widgets - sizes, states etc
data WidgetSkeleton = WidgetSkeleton {
    wid :: Text
  , pos  :: V2 CInt
  , size :: V2 CInt
  , isInFocus :: Bool
  , isHovering :: Bool
} deriving (Show, Eq)

emptySkeleton = WidgetSkeleton {
    wid = "",
    pos = V2 0 0, size = V2 0 0, isInFocus = False, isHovering = False
}


data Element = 
  WEBox {
      background :: Background
  } |
  WETextLabel {
      text :: Text
    , textAlign :: (VAlign, HAlign)
    , textStyle :: Maybe TextStyle
  } |
  WEDebug String | 
  WENone
  deriving (Show, Eq)

-- type ComponentData s p = (s,p) -- state and props

-- Other approach: tree of monadic actions
data Widget m = Widget {
    element  :: Element,
    children :: [m (Widget m)],
    eventHandlers :: [Event -> m ()],
    skeleton :: WidgetSkeleton,
    texture :: Maybe SDL.Texture,
    rerender :: Bool
}

label :: IORef Int -> IO (Widget IO)
label i' = do 
    i <- liftIO $ readIORef i'
    pure Widget {
        element = WEDebug $ show i,
        children = [],
        eventHandlers = [\e -> modifyIORef' i' (+1)],
        skeleton = emptySkeleton,
        texture = Nothing,
        rerender = True
    }

renderDebug :: Widget m -> IO ()
renderDebug Widget{..} = putStrLn $ show element

processEventsInWidget :: MonadIO m => Event -> Widget m -> m ()
processEventsInWidget e Widget{..} = mapM_ (\a -> a e) eventHandlers