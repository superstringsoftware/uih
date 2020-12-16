{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards, ExistentialQuantification #-}

module UI.Hatto.Widgets
where

import Data.Text as T hiding (map)
import Color

import Linear
import Foreign.C.Types (CInt)

import UI.Hatto.Events

import qualified SDL
import qualified SDL.Font as SDL

import Data.IORef
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Functor ((<&>))

data TextStyle = TextStyle {
    fontName  :: Text
  , fontSize  :: !Int
  , fontColor :: Color
  , fontStyle :: [SDL.Style]
  , bgColor   :: Maybe Color
} deriving (Show, Eq)

defaultTextStyle = TextStyle {
        fontName = "Roboto",
        fontSize = 16,
        fontColor = mBlack,
        fontStyle = [],
        bgColor = Nothing
    }

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
    eventHandlers :: [EventHandlerM m],
    skeleton :: WidgetSkeleton,
    texture :: Maybe SDL.Texture,
    rerender :: Bool
}

-- Wrapper around IORefs to keep our mutable state for Components in m monad
newtype MutState s = MutState {
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

-- render widgets to console
renderDebug :: MonadIO m => m (Widget m) -> m ()
renderDebug a = do 
    a' <- a
    rd a'
    mapM_ renderDebug (children a')
    where rd Widget{..} = liftIO $ putStrLn $ show element

-- send a given event to all widgets in the tree
walkWidgetWithEvents :: MonadIO m => Event -> m (Widget m) -> m ()
walkWidgetWithEvents e mw = do
    w <- mw
    processEventsInWidget e w
    mapM_ (walkWidgetWithEvents e) (children w)
    where processEventsInWidget e Widget{..} = mapM_ (\a -> a e) eventHandlers

-- Walk a widget with a transformation function. Used e.g. for rendering caching etc.
-- this one does it top-down (so if f changes children to [], there's no walking down the tree)
-- Evaluating m Widget, then applying f to it, then mapping over children
transformWidget :: MonadIO m => (Widget m -> Widget m) -> m (Widget m) -> m (Widget m)
transformWidget f mw = mw <&> f >>= \w -> return $ w { children = map (transformWidget f) (children w) }

-------------- Basic widgets with behavior

-- Editable line
mkEditableLine :: MonadIO m => MutState Text -> m (Widget m)
mkEditableLine mt = do
    t <- readMutState mt
    pure Widget {
        element = WETextLabel { text = t, textAlign = (VAlignMiddle, HAlignMiddle), textStyle = Just defaultTextStyle},
        children = [],
        skeleton = emptySkeleton,
        texture = Nothing,
        rerender = True,
        eventHandlers = [hndlAlterText mt]
    }

hndlAlterText :: MonadIO m => MutState Text -> Event -> m ()
hndlAlterText mt evt = 
    case evt of
        SDLEvent _ evt' -> 
            case evt' of
                SDL.TextInputEvent ti -> updateMutState mt (<> SDL.textInputEventText ti)
                SDL.KeyboardEvent ev  -> do
                    let k = SDL.keysymKeycode $ SDL.keyboardEventKeysym ev
                    case k of
                        SDL.KeycodeBackspace -> 
                            -- checking press only 
                            if SDL.keyboardEventKeyMotion ev == SDL.Pressed 
                            then updateMutState mt (\txt -> if txt == "" then txt else T.init txt)
                            else pure ()
                        _ -> pure ()
                _ -> pure ()
        _ -> pure ()


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

board' :: MonadIO m => MutState [Int] -> MutState Text -> m (Widget m)
board' ms mt = do
    st <- readMutState ms
    pure Widget {
        element = WEDebug $ "Board state is: "  ++ show st,
        children = [
            box (st!!0) (onLeftClick  $ updateMutState ms (const [1,0])),
            box (st!!1) (onRightClick $ updateMutState ms (const [0,1])),
            mkEditableLine mt
        ],
        eventHandlers = [],
        skeleton = emptySkeleton,
        texture = Nothing,
        rerender = True
    }
