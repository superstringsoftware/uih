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

import Control.Concurrent (threadDelay)

import Color

import UI.PicoUI.Raw.Widgets (WidgetId, Widget)
-- import UI.PicoUI.Raw.Rendering
-- import UI.PicoUI.Raw.PureHandlers
import UI.PicoUI.Raw.Events

import UI.PicoUI.Middle.PureHandlers
import qualified UI.PicoUI.Middle.AbstractWidgets as Mid

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

-- record to keep the current input devices state - it gets passed with Events!!
-- the issue is:
-- SDL reports mouse buttons held down while we are dragging the mouse
-- and some modifier keys when there's a keyboard event
-- But what about if we want to track if a user is dragging the mouse with the shift key pressed?
-- TBD
data InputDevicesState = InputDevicesState {

} deriving Show

-- record to keep our current SDL subsystem state
data SDLState = SDLState {
    mainWindow    :: Window
  , mainRenderer  :: Renderer
  , loadedFonts   :: Map.Map Text Font -- map from font names to actual fonts
  , cursor        :: CursorStatus
  , bgColor       :: V4 Word8
  , widgets       :: Map.Map WidgetId ActiveWidget -- cache of the low level widgets, DO NOT manipulate it directly
  -- , widgets       :: Map.Map WidgetId 
  -- , pureHandlers  :: Map.Map WidgetId PureHandler -- lowest level handlers that work in concert with widgets
  , idCounter     :: !WidgetId
  , curFocusId    :: !WidgetId
  , curHoverId    :: !WidgetId
  , scaleXY       :: V2 CFloat -- in case we use highDPI, this will be the scale
  , autoScale     :: Bool -- apply scaling automatically so that same logical size is used on high dpi displays

  --, readerEvent   :: Event -- currently handling event value for event hadlers and Reader monad instance
} | SDLEmptyState deriving Show

-- non-pure event handler running in SDLIO
-- can (and will) encompass pure event handlers (see EventLoop)
type EventHandler = Event -> SDLIO ()

instance Show EventHandler where show _ = "[EventHandler]"

data ActiveWidget = ActiveWidget {
    widgetId :: WidgetId, -- needed for event handling etc
    widget :: Mid.Widget, -- original high-level widget
    compiledWidget :: Widget, -- "compiled" low-level cache for the widget
    handler :: EventHandler -- composed event handler function for the Event originating from this widget
} deriving Show

isInActiveWidget x y ActiveWidget{..} = Mid.isInWidget x y widget

{-
mkPureToEventHandler handlerPure event = do
    let (i,_,_) = source event
-}



-- 
-- set renderer to scale according to scale factor - it messes up fonts, so need to render them differently
-- scaleRendere
-- finds a pure handler for widget with id
{-
getPureHandler :: WidgetId -> SDLIO (Maybe PureHandler)
getPureHandler i = (Map.lookup i) <$> gets pureHandlers
-}
{-
-- finds id and widgets in which given coordinates are, empty list if nothing
getCollidingWidgets :: CInt -> CInt -> SDLIO [(WidgetId, Widget)]
getCollidingWidgets x y = do 
    ws <- widgets <$> get
    return $ Map.assocs $ Map.filter (isInWidget x y) ws
-}  
getRenderer :: SDLIO Renderer
getRenderer = mainRenderer <$> get

setCurFocusId i = modify' (\s -> s { curFocusId = i })
getFocusWidget :: SDLIO (Maybe ActiveWidget)
getFocusWidget = Map.lookup <$> (gets curFocusId) <*> (gets widgets)

setCurHoverId i = modify' (\s -> s { curHoverId = i }) -- >> liftIO (putStrLn $ "Set hover to: " ++ show i)
getHoverWidget :: SDLIO (Maybe ActiveWidget)
getHoverWidget = Map.lookup <$> (gets curHoverId) <*> (gets widgets)
    
-- update widget at a given id
updateWidget :: WidgetId -> ActiveWidget -> SDLIO ()
updateWidget i w = 
    (Map.insert i w) <$> (gets widgets) >>= 
        \ws -> modify' (\s-> s{widgets = ws})

getWidget :: WidgetId -> SDLIO (Maybe ActiveWidget)
getWidget i = Map.lookup i <$> gets widgets

-- given x,y coordinates finds a widget that contains them and returns it (if any)
-- this is ALL CRAZY INEFFICIENT
findEventSources :: V2 Int -> SDLIO [(WidgetId, ActiveWidget)]
findEventSources (V2 x y) = do
    ws <- widgets <$> get
    pure $ Map.assocs $ Map.filter (isInActiveWidget x y) ws
    

calculateCacheRect w h wid@ActiveWidget{..} = wid { widget = Mid.calculateCacheRect w h widget  }
-- recalculates sizes of all top level widgets after a resize as needed        
-- w h - new size of the screen
recalculateRectangles :: Int -> Int -> SDLIO ()
recalculateRectangles w h = do 
    ws <- widgets <$> get
    let ws' = Map.map (calculateCacheRect w h) ws
    modify' (\s -> s { widgets = ws'} )


initUI w h = recalculateRectangles w h

instance Show Timer where
  show _ = "Timer present"

-- Stacking State and IO into a monad
type SDLIO = StateT SDLState IO
    
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
initStateIO :: IO SDLState
initStateIO = do 
    r <- try $ do
            SDL.initializeAll
            window <- SDL.createWindow "My SDL Application" mainWindowSettings
            renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer {SDL.rendererTargetTexture = True}
            return $ SDLState {
                mainWindow = window,
                mainRenderer = renderer,
                loadedFonts = Map.empty,
                cursor = CursorStatus 50 50 (mRed 500) 20 Nothing False 0,
                bgColor = V4 210 210 210 0,
                widgets = Map.empty,
                -- pureHandlers = Map.empty,
                scaleXY = V2 1 1,
                autoScale = True,
                idCounter = 0,
                curFocusId = -1,
                curHoverId = -1
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
safeLoadFont :: String -> Int -> SDLIO (Maybe Font)
safeLoadFont path size = do
    autos <- gets autoScale
    V2 x y <- gets scaleXY
    let size' = if autos then round ( (fromIntegral size) * (x + y) / 2) else size
    r <- liftIO $ try $ load path size'
    either (\e -> liftIO $ print (e::SDLException) >> return Nothing)
                (\fnt -> return $ Just fnt) r

-- handles scaling of font related sizes used in rendering etc - need this for high dpi stuff
scaleFontSizeDown :: Int -> SDLIO Int
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

getDefaultFont :: SDLIO Font
getDefaultFont = do 
    st <- get 
    let fntm = Map.lookup "__DEFAULT__" (loadedFonts st)
    maybe (fail "Could not find default font, impossible to continue!")
          (\fnt -> pure fnt) fntm

getFont :: Text -> SDLIO (Maybe Font)
getFont txt = do
    st <- get 
    pure $ Map.lookup txt (loadedFonts st)
          

getFontOrDefault :: Text -> SDLIO Font
getFontOrDefault txt = do
    st <- get 
    let fntm = Map.lookup txt (loadedFonts st)
    maybe getDefaultFont
          (\fnt -> pure fnt) fntm

initFonts :: SDLIO ()
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