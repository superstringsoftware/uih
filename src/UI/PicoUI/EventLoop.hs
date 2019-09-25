{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.EventLoop where

-- this is a huge state monad taking care of low-level SDL interactions

import Control.Monad.Trans.State.Strict
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Exception
import SDL as SDL hiding (get)
import SDL.Font
import Data.Text hiding (any)
import Foreign.C.Types (CInt, CFloat)
import Data.Word
import qualified Data.Map.Strict as Map

import Data.Foldable as Map (length) 

import Color

import UI.PicoUI.Raw.Widgets
import UI.PicoUI.Raw.Rendering
import UI.PicoUI.Raw.WidgetCompiler
import UI.PicoUI.PicoUIMonad
import UI.PicoUI.Middle.PureHandlers
import UI.PicoUI.Middle.AbstractWidgets as P
import UI.PicoUI.Raw.Events as P

import PreludeFixes

handleResize w h = recalculateRectangles w h >> compileAllWidgets

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
        handleResize (fromIntegral width) (fromIntegral height)
        appLoop
    ) 
    SDLEmptyState


renderUI :: SDLIO ()
renderUI = do
    renderer <- getRenderer
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    rendererDrawColor renderer $= V4 255 255 255 0
    SDL.clear renderer
    ws <- gets widgets
    -- liftIO $ putStrLn $ "Got widgets: " ++ show (Map.length ws)
    liftIO $ mapM_ (fn renderer) ws
    SDL.present renderer
    where fn ren ActiveWidget{..} = renderWidgetToScreen compiledWidget ren
    

appLoop :: SDLIO ()
appLoop = do
    -- isDirty <- getDirty
    -- if isDirty then renderUI >> setClean else pure ()
    renderUI
    events <- SDL.pollEvents -- get the events queue from SDL
    results <- mapM fireEvent events -- gather results
    let quit = any (== True) results -- checking if any of the results is True
    unless quit appLoop

-- takes an SDL event, converts it into our event, and runs all registered event handlers - 
-- for now, only inside widgets, eventually all others as well.
fireEvent :: SDL.Event -> SDLIO Bool
fireEvent ev = do
    -- this fires an event to all widgets automatically, so we only need to fire to the other handlers
    event <- sdlEvent2Event ev
    -- ... but since now we don't have any other handlers, there's nothign to do
    return False

-- fire specific event to a handler in an ActiveWidget
fireEventInWidget :: P.Event -> ActiveWidget -> SDLIO ()
fireEventInWidget event ActiveWidget{..} = handler event

-- fires an event to all widgets that catch given position, collect WidgetIds of all relevant widgets    
fireEventToWidgets :: V2 Int -> P.Event -> SDLIO [WidgetId]
fireEventToWidgets (V2 x y) event = do
    gets widgets >>= mapM fn >>= 
        \ws' -> pure $ Map.foldlWithKey fn1 [] ws'
    where 
        fn1 acc i val = if val then (i:acc) else acc
        fn w = 
            if isInActiveWidget x y w 
            then (handler w) event >> pure True 
            else pure False

--
-- type PureHandler = AbstractWidget -> Reader Event AbstractWidget 

-- Ok, this is a bit tricky - we are creating a handler for an abstract widget with it's id
-- by memoizing id and widget values so that then we can use the id inside the monad
-- to update the widget. A bit crazy i know.
-- BOTTOM LINE: MUST USE THIS to generate event handlers that alter AbstractWidgets,
-- exactly by calling pureToEventHandler handler i -- so that it returns EventHandler function!
-- this is INTERNAL.
pureToEventHandler :: PureHandler -> WidgetId -> P.Event -> SDLIO ()
pureToEventHandler handler i event = do
    widm <- getWidget i
    maybe (pure ()) -- shouldn't be happening!!! need to LOG AN ERROR here
          (\wid -> do
                     -- first, use pure handler to transform the widget by running the reader action
                     let w' = runReader (handler (widget wid)) event
                     -- then, compiling new abstract widget to low level widget
                     -- TODO: be smarter here about the updates etc
                     cw <- compile2Widget w'
                     -- now, update the ActiveWidget in the monad
                     updateWidget i wid { widget = w', compiledWidget = cw }
          ) widm

--
-- So overall idea is: take a PureHandler, AbstractWidget and then call some function 
-- that sets up ActiveWidget with correct id, handler function etc.
-- THIS IS THE MAIN EXTERNAL FUNCTION TO REGISTER HANDLERS WITH WIDGETS!!!
registerWidgetWithHandler :: AbstractWidget -> PureHandler -> SDLIO Int
registerWidgetWithHandler w h = do
    ws <- gets widgets
    i  <- fmap (+1) (gets idCounter)
    -- creating a monadic handler from pure
    let handler = pureToEventHandler h i
    cw <- compile2Widget w
    let ws' = Map.insert i (ActiveWidget { widget = w, compiledWidget = cw, handler = handler })  ws
    modify' (\s-> s{idCounter = i, widgets = ws'})
    return i
    

-- converting SDL event to our event inside a monad - to be able to 
-- retrieve widgets right away    
sdlEvent2Event :: SDL.Event -> SDLIO P.Event
sdlEvent2Event event = 
    case SDL.eventPayload event of
            SDL.MouseButtonEvent mb -> 
                -- MouseButtonEventData {mouseButtonEventWindow = Just (Window 0x00007f9500c38fb0), 
                --    mouseButtonEventMotion = Released, mouseButtonEventWhich = Mouse 0, mouseButtonEventButton = ButtonLeft, 
                --    mouseButtonEventClicks = 1, mouseButtonEventPos = P (V2 407 170)}
                -- Pressed or Released. Analogous to key presses we will only think about Released as "clicks"
                -- SDL helpfully reports # of clicks = 0 in case the mouse was moved after pressing.
                -- This way, we can handle proper clicks as well as "press a button, drag, release" type of thing eventually
                let motion = SDL.mouseButtonEventMotion mb 
                    button = SDL.mouseButtonEventButton mb
                    clicks = SDL.mouseButtonEventClicks mb
                    SDL.P pos  = SDL.mouseButtonEventPos mb
                    cons   = if button == SDL.ButtonLeft then LeftClick else RightClick
                    pos'   = castV2 pos
                    src = EventSource [] pos' (SDL.eventTimestamp event)
                    evt    = cons src clicks
                    -- firing event to all widgets right away, updating source and returning the event
                in  do --fireEventToWidgets pos' evt >>= \ids -> pure $ evt { source = src { widgetIds = ids } } 
                        srcs <- findEventSources pos'
                        pure evt
            p -> pure $ RawSDLEvent (EventSource [] (V2 0 0) (SDL.eventTimestamp event)) p
    

-- EVENT HANDLING is tricky
-- There are no ready design decisions (yes, FRP, but banana forces you to IO which we don't want)
-- Looks like it makes sense to defer event handling to actual handlers, since we need to keep track of 
-- any modifier keys that are down when the new event occurs etc - so, some way to track the state
-- of the keyboard and mouse and pass it with an event

-- So, additional state of the input devices which is passed with each event and updated by the main cycle
-- as events are proceeding?
checkEventOLD :: SDL.Event ->  SDLIO Bool
checkEventOLD event = do
    --liftIO $ print $ show $ event
    case SDL.eventPayload event of
        SDL.WindowResizedEvent dt -> do
            let (SDL.V2 w h) = SDL.windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            -- renUI --(fromIntegral w) (fromIntegral h)
            -- setDirty
            -- setNeedsRecalculation True
            return False
        SDL.QuitEvent -> return True
        SDL.KeyboardEvent ev -> do
                -- liftIO $ print $ show ev
                let k = keysymKeycode $ keyboardEventKeysym ev
                --liftIO $ print $ show k
                case k of
                    KeycodeBackspace -> return False
                    {-
                        if (keyboardEventKeyMotion ev) == Pressed 
                        then do 
                            mevs <- getFocusWidget
                            maybe (return False)
                                (\evs -> fireEvent (MM.Event EvKbBackspace evs) >> return False)
                                mevs
                        else return False -}
                    KeycodeRight -> return False
                    {-
                        if (keyboardEventKeyMotion ev) == Pressed
                        then do 
                            liftIO $ putStrLn "Right!"
                            return False
                        else return False -}
                    KeycodeLeft -> return False
                    {-
                        if (keyboardEventKeyMotion ev) == Pressed
                        then do 
                            liftIO $ putStrLn "Left!"
                            return False
                        else return False -}
                    _                -> return False
        SDL.MouseButtonEvent mb -> do
            -- liftIO $ print mb
            -- MouseButtonEventData {mouseButtonEventWindow = Just (Window 0x00007f9500c38fb0), 
            --    mouseButtonEventMotion = Released, mouseButtonEventWhich = Mouse 0, mouseButtonEventButton = ButtonLeft, 
            --    mouseButtonEventClicks = 1, mouseButtonEventPos = P (V2 407 170)}
            -- Pressed or Released. Analogous to key presses we will only think about Released as "clicks"
            -- SDL helpfully reports # of clicks = 0 in case the mouse was moved after pressing.
            -- This way, we can handle proper clicks as well as "press a button, drag, release" type of thing eventually
            let motion = SDL.mouseButtonEventMotion mb 
            let button = SDL.mouseButtonEventButton mb
            let clicks = SDL.mouseButtonEventClicks mb
            let P (V2 x y) = SDL.mouseButtonEventPos mb
            -- ws <- getCollidingWidgets (fromIntegral x) (fromIntegral y)
            return False
        SDL.MouseMotionEvent me -> do 
            let P (V2 x y) = SDL.mouseMotionEventPos me
            -- liftIO $ putStrLn $ "Mouse moved to: " ++ show x ++ ", " ++ show y
            -- converting into mouse hover event if any widget is under the mouse and firing it to MM handlers
            -- ws <- getCollidingWidgets (fromIntegral x) (fromIntegral y)
            -- mapM_ (\(i,_) -> setCurFocusId i) ws
            return False
            {-
            mevs <- getEventSource (fromIntegral x) (fromIntegral y)
            maybe (return False)
                  (\evs@(i,_) -> fireEvent (MM.Event EvHover evs) >> setCurrentFocusId (Just i) >> return False)
                  mevs -}
        SDL.TextEditingEvent ev -> do
                liftIO $ print $ show ev
                return False
        SDL.TextInputEvent ev -> do
                liftIO $ print $ show ev
                return False
                {-
                mevs <- getFocusWidget
                maybe (return False)
                      (\evs -> fireEvent (MM.Event (EvTextInput (textInputEventText ev)) evs) >> return False)
                      mevs -}
        _ -> return False