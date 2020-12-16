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

-- Wrapper around IORefs to keep our mutable state for Components in m monad
data MutState s = MutState {
    state :: IORef s
}
-- creating a new mutable state
newMutState :: MonadIO m => s -> m (MutState s)
newMutState s = liftIO (newIORef s) >>= \s' -> return $ MutState { state = s' }
-- reading
readMutState :: MonadIO m => MutState s -> m s
readMutState MutState{..} = liftIO $ readIORef state
-- modifying - strict only (why?)
updateMutState :: MonadIO m => MutState s -> (s -> s) -> m ()
updateMutState MutState{..} f = liftIO $ modifyIORef' state f


renderDebug :: MonadIO m => m (Widget m) -> m ()
renderDebug a = do 
    rd a
    a' <- a
    mapM_ rd (children a')
    where rd x = x>>= \Widget{..} -> liftIO $ putStrLn $ show element

processEventsInWidget :: MonadIO m => Event -> Widget m -> m ()
processEventsInWidget e Widget{..} = mapM_ (\a -> a e) eventHandlers

walkWidgetWithEvents :: MonadIO m => Event -> m (Widget m) -> m ()
walkWidgetWithEvents e mw = do
    w <- mw
    processEventsInWidget e w
    mapM_ (walkWidgetWithEvents e) (children w)

------------ some tests

label :: MonadIO m => MutState Int -> m (Widget m)
label ms = do 
    i <- readMutState ms
    pure Widget {
        element = WEDebug $ show i,
        children = [],
        eventHandlers = [\e -> updateMutState ms (+1)],
        skeleton = emptySkeleton,
        texture = Nothing,
        rerender = True
    }

-- simple tic-tac-toe-like-react example
box :: MonadIO m => Int -> (Event -> m ()) -> m (Widget m)
box i eh = pure Widget {
        element = WEDebug $ "Cell: " ++ show i,
        children = [],
        eventHandlers = [eh],
        skeleton = emptySkeleton,
        texture = Nothing,
        rerender = True
    }

board :: MonadIO m => MutState [Int] -> m (Widget m)
board ms = do
    st <- readMutState ms
    pure Widget {
        element = WEDebug $ "Board state is: "  ++ show st,
        children = [
            box (st!!0) (onLeftClick  $ updateMutState ms (const [1,0])),
            box (st!!1) (onRightClick $ updateMutState ms (const [0,1]))
        ],
        eventHandlers = [],
        skeleton = emptySkeleton,
        texture = Nothing,
        rerender = True
    }

onClick :: MonadIO m  => m () -> Event -> m ()
onClick handler (SDLEvent _ (MouseButtonEvent mbe)) = handler
onClick _ _ = pure ()

onLeftClick :: MonadIO m  => m () -> Event -> m ()
onLeftClick handler (SDLEvent _ (MouseButtonEvent mbe)) = if (SDL.mouseButtonEventButton mbe == SDL.ButtonLeft) then handler else pure ()
onLeftClick _ _ = pure ()

onRightClick :: MonadIO m  => m () -> Event -> m ()
onRightClick handler (SDLEvent _ (MouseButtonEvent mbe)) = if (SDL.mouseButtonEventButton mbe == SDL.ButtonRight) then handler else pure ()
onRightClick _ _ = pure ()