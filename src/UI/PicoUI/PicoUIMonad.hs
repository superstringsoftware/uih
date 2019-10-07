{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, FlexibleInstances #-}
module UI.PicoUI.PicoUIMonad where

-- | this is a huge state monad taking care of low-level SDL interactions
-- overall idea after several iterations is as follows:
-- we will have at least 2 levels:
-- * very low level of Raw.Widgets that (proobably) don't handle any state, but are only used for RENDERING
-- * middle-level widgets that are much more convenient for defining the UI programmatically,
--   we will store them in the monad as well, events will be handled by them (so they will have behavior), 
--   and they will COMPILE to the low level widgets as needed (ONLY WHEN CHANGED!!) which will then be rendered.
--
-- So the workflow on the high level is:
-- 1) define middleware widgets that describe the ui
-- 2) they get registered in the monad with corresponding *pure* handlers
-- 3) they get compiled to low level widgets
-- 4) event loop starts
-- 5) as middleware widgets change when responding to events, we recompile corresponding low-level widgets,
--      but not the whole tree.


import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception
import SDL as SDL hiding (get, Event)
import SDL.Font hiding (height)
import Data.Text
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IMap

import Control.Concurrent (threadDelay)

import Color

import UI.PicoUI.Raw.Widgets (WidgetId, Widget)
-- import UI.PicoUI.Raw.Rendering
-- import UI.PicoUI.Raw.PureHandlers
import UI.PicoUI.Raw.Events

import UI.PicoUI.Middle.PureHandlers
import qualified UI.PicoUI.Middle.AbstractWidgets as Mid

import UI.PicoUI.Reactive.Internal.StatefulSignals

import System.IO.Unsafe (unsafePerformIO)

import PreludeFixes

-- record to store current position etc of the cursor
data CursorStatus = CursorStatus {
      x      :: CInt
    , y      :: CInt
    , color  :: V4 Word8
    , height :: CInt
    , cursorTimer :: Maybe Timer
    , blink  :: Bool -- for checking status for redraw timer
    , prevTick :: Word32
} deriving Show

prolongCursor :: PicoUIM u ()
prolongCursor = do
    curTick <- ticks
    cur <- gets cursor
    modify' (\s -> s { cursor = cur { blink = True, prevTick = curTick } })

-- record to keep the current input devices state - it gets passed with Events!!
-- the issue is:
-- SDL reports mouse buttons held down while we are dragging the mouse
-- and some modifier keys when there's a keyboard event
-- But what about if we want to track if a user is dragging the mouse with the shift key pressed?
-- TBD
data InputDevicesState = InputDevicesState {

} deriving Show

type ReactiveWidget u = StatefulSignal (PicoUIM u) Mid.AbstractWidget
-- reactive signals in (PicoUIM u) monad
type PicoSignal u a = StatefulSignal (PicoUIM u) a

data EventSources u = EventSources {
    allEvents      :: StatefulSignal (PicoUIM u) Event
  , clickEvents    :: StatefulSignal (PicoUIM u) Event
  , textEvents     :: StatefulSignal (PicoUIM u) Event
  , keyboardEvents :: StatefulSignal (PicoUIM u) Event
  , focusEvents    :: StatefulSignal (PicoUIM u) Event -- source for the events that only a widget in focus should receive, see ReactiveWidgets
  , hoverEvents    :: StatefulSignal (PicoUIM u) Event
} deriving Show

-- record to keep our current SDL subsystem state
data SDLState u = SDLState {
    mainWindow    :: Window
  , mainRenderer  :: Renderer
  , loadedFonts   :: Map.Map Text Font -- map from font names to actual fonts
  , cursor        :: CursorStatus
  , bgColor       :: V4 Word8
  , rawWidgets    :: IMap.IntMap (StatefulSignal (PicoUIM u) Widget) -- cache of the low level widgets, DO NOT manipulate it directly
  , removeFocus   :: PicoUIM u () -- action to remove focus signal listener from a currently focused widget (see ReactiveWidgets addFocus etc)
  , focusWidget   :: Maybe (ReactiveWidget u)
  , rawIdCounter  :: !Int
  , scaleXY       :: V2 CFloat -- in case we use highDPI, this will be the scale
  , autoScale     :: Bool -- apply scaling automatically so that same logical size is used on high dpi displays
  , eventSources  :: EventSources u
  , userState     :: u
} | SDLEmptyState deriving Show

instance Show (PicoUIM u ()) where show _ = "PicoUIM u () action"

-- Stacking State and IO into a monad with potential user state
type PicoUIM u = StateT (SDLState u) IO
type PicoUI = PicoUIM () -- type synonim for a monad without user state

getUState :: PicoUIM u u
getUState = gets userState

putUState :: u -> PicoUIM u ()
putUState st = modify' (\s -> s{userState = st})

modifyUState' :: (u -> u) -> PicoUIM u ()
modifyUState' f = f <$> getUState >>= putUState

getsU :: (u -> a) -> PicoUIM u a
getsU f = f <$> getUState


quickEvalSDLIO :: PicoUIM u a -> IO a
quickEvalSDLIO act = evalStateT act SDLEmptyState

unsafePerformSDLIO :: PicoUIM u a -> a
unsafePerformSDLIO = unsafePerformIO • quickEvalSDLIO 

instance {-# OVERLAPPING #-} Show a => Show (StatefulSignal (PicoUIM u) a) where
    show = show • unsafePerformSDLIO • readVal

-- instance Show EventHandler where show _ = "[EventHandler]"

getRenderer :: PicoUIM u Renderer
getRenderer = mainRenderer <$> get
    
insertRawWidget :: StatefulSignal (PicoUIM u) Widget -> PicoUIM u ()
insertRawWidget w = do
    i <- gets rawIdCounter
    ws <- gets rawWidgets
    let ws' = IMap.insert i w ws
    modify' (\s-> s {rawWidgets = ws', rawIdCounter = i + 1})

instance Show Timer where
  show _ = "Timer present"

-- needs to be read from config!
mainWindowSettings = defaultWindow
  { windowBorder       = True
  -- There are issues with high DPI windows b/c we need to recalculate all coordinates when drawing / checking event
  -- coordinates, so its support is pending
  -- OpenGLContext defaultOpenGL
  , windowHighDPI      = True
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowGraphicsContext = OpenGLContext defaultOpenGL
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 1200 800
  }


-- dumpSDLState :: SDLIO ()
-- dumpSDLState = get >>= liftIO . print . show

-- main initialization functions
initState :: PicoUIM u ()
initState = do
    -- initializing event sources - they are SEPARATE, as event dispatching function
    -- will filter them when converting SDL events, as this is going to be more efficient
    -- Why is it going to be more efficient???
    es <- createStatefulSignal $ ENonEvent zeroSource
    --ce <- createStatefulSignal $ ENonEvent zeroSource
    -- for testing setting up ce via filter
    ce <- filterS isAnyClick es
    te <- filterS (\e -> (isBackspace e) || (isTextEvent e)) es
    -- te <- createStatefulSignal $ ENonEvent zeroSource
    ke <- createStatefulSignal $ ENonEvent zeroSource
    -- focus events: for now only text, but eventually needs to be more complicated
    fe <- filterS (\e -> (isBackspace e) || (isTextEvent e)) es
    he <- filterS isHover es
    let eS = EventSources es ce te ke fe he
    s  <- liftIO initStateIO
    put $ s { eventSources = eS, removeFocus = pure () } 

initStateIO :: IO (SDLState u)
initStateIO = do 
    r <- try $ do
            SDL.initializeAll
            window <- SDL.createWindow "My SDL Application" mainWindowSettings
            renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
            return $ SDLState {
                mainWindow = window,
                mainRenderer = renderer,
                loadedFonts = Map.empty,
                cursor = CursorStatus 50 50 (mdAmber 800) 20 Nothing False 0,
                bgColor = V4 210 210 210 0,
                rawWidgets = IMap.empty,
                focusWidget = Nothing,
                -- pureHandlers = Map.empty,
                scaleXY = V2 1 1,
                autoScale = True,
                rawIdCounter = 0
                -- readerEvent = NonEvent                
                }
    either (\e -> print (e::SDLException) >> fail "Could not initialize SDL")
           (\st -> putStrLn "Initialized SDL" >> return st) r
                 



----------------------------------------------------------------------------------------------------
-- font stuff
----------------------------------------------------------------------------------------------------
defaultFontPath = "./Roboto-Light.ttf"

{-
defaultFont :: Int -> IO Font
defaultFont size = load defaultFontPath size
-}

-- ALL FONT LOADING NEEDS TO BE DONE VIA THIS FUNCTION
-- it handles scaling for dpi etc
-- The logic is:
-- We make font size SCALE UP in high-dpi environments
-- when we render font related textures, we scale back to 0 so that they are rendered correctly
safeLoadFont :: String -> Int -> PicoUIM u (Maybe Font)
safeLoadFont path size = do
    autos <- gets autoScale
    V2 x y <- gets scaleXY
    let size' = if autos then round ( (fromIntegral size) * (x + y) / 2) else size
    r <- liftIO $ try $ load path size'
    either (\e -> liftIO $ print (e::SDLException) >> return Nothing)
                (\fnt -> return $ Just fnt) r

-- handles scaling of font related sizes used in rendering etc - need this for high dpi stuff
scaleFontSizeDown :: Int -> PicoUIM u Int
scaleFontSizeDown size = do
    V2 x y <- gets scaleXY
    autos <- gets autoScale
    let size' = if autos then round ( (fromIntegral size) / ((x + y) / 2)) else size
    return size'

                
data SDLFontData = SDLFontData {
    fntIsMonospace :: Bool,
    fntFamilyName :: Maybe Text,
    fntStyleName :: Maybe Text,
    fntLineSkip :: !Int
} deriving (Eq, Show)

getDefaultFont :: PicoUIM u Font
getDefaultFont = do 
    st <- get 
    let fntm = Map.lookup "__DEFAULT__" (loadedFonts st)
    maybe (fail "Could not find default font, impossible to continue!")
          (\fnt -> pure fnt) fntm

getFont :: Text -> PicoUIM u (Maybe Font)
getFont txt = do
    st <- get 
    pure $ Map.lookup txt (loadedFonts st)
          

getFontOrDefault :: Text -> PicoUIM u Font
getFontOrDefault txt = do
    st <- get 
    let fntm = Map.lookup txt (loadedFonts st)
    maybe getDefaultFont
          (\fnt -> pure fnt) fntm

initFonts :: PicoUIM u ()
initFonts = do
    fnt <- initDefaultFont
    st <- get
    let fonts = Map.insert "__DEFAULT__" fnt (loadedFonts st)
    fd_mns <- isMonospace fnt
    fd_fml <- familyName fnt
    fd_sn <- styleName fnt
    fd_ls <- lineSkip fnt
    let fd = SDLFontData fd_mns fd_fml fd_sn fd_ls
    liftIO $ putStrLn $ "Loaded font:\n" ++ show fd
    put st {loadedFonts = fonts}
    where initDefaultFont = do   
            mfont <- (SDL.Font.initialize >> safeLoadFont defaultFontPath 16)
            maybe (fail "Could not initialize TTF fonts!")
                  (\font -> return $ font
                   ) mfont