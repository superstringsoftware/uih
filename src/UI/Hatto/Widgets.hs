{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, RecordWildCards, ExistentialQuantification, FlexibleContexts #-}

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

import Data.Map as Map hiding (map)
import Foreign.C.Types (CFloat)


data SDLState = SDLState {
    mainWindow    :: SDL.Window
  , mainRenderer  :: SDL.Renderer
  , loadedFonts   :: Map.Map (Text, Int) SDL.Font -- map from font names and sizes to actual fonts
  , scaleXY       :: V2 CFloat -- in case we use highDPI, this will be the scale
  , autoScale     :: Bool -- apply scaling automatically so that same logical size is used on high dpi displays
  , defaultPixelFormat :: SDL.PixelFormat
} deriving Show

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
} deriving (Show, Eq)

emptySkeleton = WidgetSkeleton {
    wid = "",
    pos = V2 0 0, size = V2 0 0
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



-- need to cache mutable state somehow - StatefulSignals approach again?

data StatefulWidget m = StatefulWidget {
    renderS :: m Element,
    childrenS :: [StatefulWidget m],
    handlersS :: [EventHandlerM m]
} | PureWidget {

}

-- render widgets to console
renderDebugS :: MonadIO m => StatefulWidget m -> m ()
renderDebugS sw = do 
    rd sw
    mapM_ renderDebugS (childrenS sw)
    where rd StatefulWidget{..} = renderS >>= liftIO . putStrLn . show

-- send a given event to all widgets in the tree
walkWidgetWithEventsS :: MonadIO m => Event -> StatefulWidget m -> m ()
walkWidgetWithEventsS e sw = do
    processEventsInWidget e sw
    mapM_ (walkWidgetWithEventsS e) (childrenS sw)
    where processEventsInWidget e StatefulWidget{..} = mapM_ (\a -> a e) handlersS

-- only send events to the widget if the condition is met
walkWidgetWithEventsCondS :: MonadIO m => (StatefulWidget m -> Bool) -> Event -> StatefulWidget m -> m ()    
walkWidgetWithEventsCondS cond e sw = do
    if cond sw then processEventsInWidget e sw >> mapM_ (walkWidgetWithEventsCondS cond e) (childrenS sw)
    else mapM_ (walkWidgetWithEventsCondS cond e) (childrenS sw)
    where processEventsInWidget e StatefulWidget{..} = mapM_ (\a -> a e) handlersS


-- handwritten board in this approach -- WORKS!! Need a nicer interface!!!
boxS i = WEDebug $ "Cell: " ++ show i

boardS :: MonadIO m => m (StatefulWidget m)
boardS = do
    initState <- newMutState [0 :: Int,0 :: Int]
    el <- mkEditableLineS "Hello World NEW!"
    el1 <- newDependentWidgetM 
                initState
                (\s -> boxS (s !! 0))
                (onLeftClick  $ updateMutState initState (const [1,0]))
    el2 <- newDependentWidget 
                initState
                (\s -> boxS (s !! 1))
                (\e -> if isRightClick e then const [0,1] else id )
    pure $ StatefulWidget {
        renderS = readMutState initState >>= \s -> pure $ WEDebug $ "Board state is: "  ++ show s,
        handlersS = [],
        childrenS = [
            el, el1, el2
        ]
    }

-- New approach - hiding the state inside functions!
-- new widget with independent state
newStatefulWidget :: MonadIO m => s -> (s -> Element) -> (Event -> s -> s) -> m (StatefulWidget m)
newStatefulWidget initS pureRender pureHandler = do
    cache <- newMutState initS
    newDependentWidget cache pureRender pureHandler

-- widget dependent on some mutable state, used for creation of children!
newDependentWidget :: MonadIO m => MutState s -> (s -> Element) -> (Event -> s -> s) -> m (StatefulWidget m)
newDependentWidget cache pureRender pureHandler = do
    let mod f = updateMutState cache f 
    let ren = readMutState cache <&> pureRender
    let han = updateMutState cache . pureHandler
    pure $ StatefulWidget {
                renderS = ren
              , handlersS = [han]
              , childrenS = []
           }

newDependentWidgetM :: MonadIO m => MutState s -> (s -> Element) -> (Event -> m()) -> m (StatefulWidget m)
newDependentWidgetM cache pureRender han = do
    let mod f = updateMutState cache f 
    let ren = readMutState cache <&> pureRender
    pure $ StatefulWidget {
                renderS = ren
              , handlersS = [han]
              , childrenS = []
           }


mkEditableLineS :: MonadIO m => Text -> m (StatefulWidget m)
mkEditableLineS txt = 
    newStatefulWidget txt
                      (\t -> WETextLabel { text = t, textAlign = (VAlignMiddle, HAlignMiddle), textStyle = Just defaultTextStyle})
                      hndlAlterTextPure


hndlAlterTextPure :: Event -> Text -> Text
hndlAlterTextPure evt txt = 
    case evt of
        SDLEvent _ evt' -> 
            case evt' of
                SDL.TextInputEvent ti -> txt <> SDL.textInputEventText ti
                SDL.KeyboardEvent ev  -> do
                    let k = SDL.keysymKeycode $ SDL.keyboardEventKeysym ev
                    if (k == SDL.KeycodeBackspace) && (SDL.keyboardEventKeyMotion ev == SDL.Pressed)
                    then if txt == "" then txt else T.init txt
                    else txt    
                _ -> txt
        _ -> txt




