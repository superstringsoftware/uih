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
import SDL.Font
import Data.Text
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Color

import UI.PicoUI.Raw.Widgets
import UI.PicoUI.Raw.Rendering
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
  , widgets       :: Map.Map WidgetId Widget -- cache of the low level widgets
  , pureHandlers  :: Map.Map WidgetId PureHandler -- lowest level handlers that work in concert with widgets
  , idCounter     :: !WidgetId
  , curFocusId    :: !WidgetId
  , scaleXY       :: V2 CFloat -- in case we use highDPI, this will be the scale
  , autoScale     :: Bool -- apply scaling automatically so that same logical size is used on high dpi displays

  --, readerEvent   :: Event -- currently handling event value for event hadlers and Reader monad instance
} | SDLEmptyState deriving Show

-- non-pure event handler running in SDLIO
-- can (and will) encompass pure event handlers
type EventHandler = Event -> SDLIO ()
-- type PureHandler = AbstractWidget -> Reader Event AbstractWidget 
pureToEventHandler :: PureHandler -> EventHandler
pureToEventHandler = undefined

{-
mkPureToEventHandler handlerPure event = do
    let (i,_,_) = source event
-}


-- runSDLIO :: SDLIO a -> SDLState -> IO (a, SDLState)
runSDLIO program = runStateT 
    (do
        (liftIO initStateIO) >>= put
        initFonts
        window <- gets mainWindow
        V2 width height <- (window.-windowSize?=)
        V2 realw realh  <- window.-glGetDrawableSize   --vkGetDrawableSize
        liftIO $ putStrLn $ "Logical size: " ++ show width ++ "x" ++ show height
        liftIO $ putStrLn $ "Physical size: " ++ show realw ++ "x" ++ show realh
        let scale = V2 ( (fromIntegral realw) / (fromIntegral width)) ( (fromIntegral realh) / (fromIntegral height))
        modify' (\s-> s { scaleXY = scale })
        autos <- gets autoScale
        ren <- getRenderer
        if autos then rendererScale ren $= scale else pure ()
        program
    ) 
    SDLEmptyState

-- 
-- set renderer to scale according to scale factor - it messes up fonts, so need to render them differently
-- scaleRendere
-- finds a pure handler for widget with id
getPureHandler :: WidgetId -> SDLIO (Maybe PureHandler)
getPureHandler i = (Map.lookup i) <$> gets pureHandlers

-- finds id and widgets in which given coordinates are, empty list if nothing
getCollidingWidgets :: CInt -> CInt -> SDLIO [(WidgetId, Widget)]
getCollidingWidgets x y = do 
    ws <- widgets <$> get
    return $ Map.assocs $ Map.filter (isInWidget x y) ws
    
getRenderer :: SDLIO Renderer
getRenderer = mainRenderer <$> get

setCurFocusId i = modify' (\s -> s { curFocusId = i })

-- register new widget and increment the counter
registerWidget :: Widget -> SDLIO Int
registerWidget w = do
    ws <- gets widgets
    i  <- fmap (+1) (gets idCounter)
    -- liftIO $ putStrLn $ "Registering Widget #" ++ show i
    let ws' = Map.insert i w ws
    modify' (\s-> s{idCounter = i, widgets = ws'})
    return i

-- update widget at a given id
updateWidget :: WidgetId -> Widget -> SDLIO ()
updateWidget i w = 
    (Map.insert i w) <$> (gets widgets) >>= 
        \ws -> modify' (\s-> s{widgets = ws})

-- since handlers are composable, we ALWAYS accept only ONE handler for each widget
registerWidgetWHandler :: Widget -> PureHandler -> SDLIO () 
registerWidgetWHandler w h = do
    hs <- gets pureHandlers
    i  <- registerWidget w
    let nh = Map.insert i h hs 
    modify' (\s-> s { pureHandlers = nh })


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
                cursor = CursorStatus 0 0 (V4 255 255 255 0) 0 Nothing,
                bgColor = V4 210 210 210 0,
                widgets = Map.empty,
                pureHandlers = Map.empty,
                scaleXY = V2 1 1,
                autoScale = True,
                idCounter = 0,
                curFocusId = -1
                -- readerEvent = NonEvent
                }
    either (\e -> print (e::SDLException) >> fail "Could not initialize SDL")
           (\st -> putStrLn "Initialized SDL" >> return st) r
                 
-- 
-- some test widgets
testButton :: SDLIO Widget
testButton = do
    Just font <- getDefaultFont
    return Widget {
                isVisible = True,
                collider = V4 50 50 400 50,
                elements = [
                    WidgetElement {
                        offset = V2 0 0,
                        el = SDLBox (rgbaToV4Color $ mdGrey 700)
                    },
                    WidgetElement {
                        offset = V2 8 8,
                        el = SDLText {
                            text = "Hello!",
                            font = font,
                            color = rgbaToV4Color $ mdRed 500,
                            cursorPos = 0
                        }
                    }
                ]
            }



-- font stuff
defaultFontPath = "./Roboto-Light.ttf"

defaultFont :: Int -> IO Font
defaultFont size = load defaultFontPath size

safeLoadFont path size = 
    do r <- try $ load path size
       either (\e -> print (e::SDLException) >> return Nothing)
              (\fnt -> return $ Just fnt) r
                      
data SDLFontData = SDLFontData {
    fntIsMonospace :: Bool,
    fntFamilyName :: Maybe Text,
    fntStyleName :: Maybe Text,
    fntLineSkip :: !Int
} deriving (Eq, Show)

getDefaultFont :: SDLIO (Maybe Font)
getDefaultFont = get >>= \st -> return $ Map.lookup "__DEFAULT__" (loadedFonts st)

initFonts :: SDLIO ()
initFonts = do
    fnt <- liftIO initDefaultFont
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
            r <- try $ (SDL.Font.initialize >> defaultFont 16)
            either (\e -> print (e::SDLException) >> fail "Could not initialize TTF fonts!")
                   (\font -> return font) r