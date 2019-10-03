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
import UI.PicoUI.Raw.Events as P
import UI.PicoUI.PicoUIMonad as Pico

import UI.PicoUI.Middle.PureHandlers
import UI.PicoUI.Middle.Handlers
import UI.PicoUI.Middle.AbstractWidgets as P

import UI.PicoUI.Reactive.Internal.StatefulSignals
import UI.PicoUI.Reactive.ReactiveWidgets

import PreludeFixes

handleResize w h = recalculateRectangles w h >> compileAllWidgets


-- runSDLIO :: SDLIO a -> SDLState -> IO (a, SDLState)
-- MAIN WRAPPER THAT HANDLES INITIALIZATION ETC
runSDLIO program = runStateT 
    (do
        initState
        window <- gets mainWindow
        V2 width height <- (window.-windowSize?=)
        V2 realw realh  <- window.-glGetDrawableSize   --vkGetDrawableSize
        liftIO $ putStrLn $ "Logical size: " ++ show width ++ "x" ++ show height
        liftIO $ putStrLn $ "Physical size: " ++ show realw ++ "x" ++ show realh
        let scale = V2 ( (fromIntegral realw) / (fromIntegral width)) ( (fromIntegral realh) / (fromIntegral height))
        modify' (\s-> s { scaleXY = scale })
        autos <- gets autoScale
        ren <- getRenderer
        -- setting renderer scale in case we are in high-dpi
        if autos then rendererScale ren $= scale else pure ()
        -- need to init fonts after the scale has been set
        initFonts
        -- setup the initial FRP network
        let logSink e = liftIO (putStrLn ("[EVENT][" ++ show (timestamp $ source e) ++ "]") >> putStrLn (show e))
        sdlSources <- allEvents <$> gets eventSources
        -- main window events
        clickMainWindowEvents <- filterS 
                (\e -> (isSourceMainWindow e) && (isLeftClick 1 e))
                sdlSources
        -- removing focus in case main window was clicked                
        -- sink clickMainWindowEvents $ const doRemoveFocus
        -- sdlS' <- filterS (not • isRawSDL) sdlSource
        -- (addListener sdlS') logSink
        -- (addListener sdlSource) logSink
        -- running the program
        program
        handleResize (fromIntegral width) (fromIntegral height)
        appLoop
    ) 
    SDLEmptyState


renderUI :: SDLIO ()
renderUI = do
    renderer <- getRenderer
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    bgClr <- gets Pico.bgColor
    rendererDrawColor renderer $= bgClr
    SDL.clear renderer
    -- THIS RECALCULATES EVERYTHING EVERY CYCLE
    -- window <- gets mainWindow
    -- V2 width height <- (window.-windowSize?=)
    -- handleResize (fromIntegral width) (fromIntegral height)
    -- END OF RECALCULATION
    ws <- gets widgets
    -- liftIO $ putStrLn $ "Got widgets: " ++ show (Map.length ws)
    mapM_ (fn renderer) ws
    -- now for reactive widgets
    rws <- gets rawWidgets
    mapM_ (fnr renderer) rws
    SDL.rendererRenderTarget renderer $= Nothing
    {-
    cf <- gets curFocusId
    if cf >= 0 then renderCursor renderer else pure ()
    -}
    mfw <- gets focusWidget
    maybe (pure ()) (const $ renderCursor renderer) mfw
    SDL.present renderer
    where fn ren ActiveWidget{..} = renderWidgetToScreen compiledWidget ren
          fnr ren rw = readVal rw >>= \w -> renderWidgetToScreen w ren
    

appLoop :: SDLIO ()
appLoop = do
    -- isDirty <- getDirty
    -- if isDirty then renderUI >> setClean else pure ()
    renderUI
    events <- SDL.pollEvents -- get the events queue from SDL
    results <- mapM fireEvent events -- gather results
    let quit = any (== True) results -- checking if any of the results is True
    if quit 
    then get >>= liftIO . putStrLn . show >> liftIO (putStrLn "Good-bye.")
    else appLoop

-- takes an SDL event, converts it into our event, and runs all registered event handlers - 
-- for now, only inside widgets, eventually all others as well.
fireEvent :: SDL.Event -> SDLIO Bool
fireEvent ev = do
    -- this fires an event to all widgets automatically, so we only need to fire to the other handlers
    event <- sdlEvent2Event ev
    -- firing in the reactive sdl source - eventually all logic needs to move here!!!
    sdlSource <- allEvents <$> gets eventSources
    fire sdlSource event
    -- liftIO $ putStrLn $ "Firing event: " ++ show event
    -- fire <$> (gets eventSource) <*> pure event
    -- handling event with main window default handlers
    mHndlMainWindow event
    return $ isQuit event

-- fire specific event to a handler in an ActiveWidget
fireEventToWidget :: P.Event -> ActiveWidget -> SDLIO ()
fireEventToWidget event ActiveWidget{..} = handler event

-- fire event to focus widget if one exists and return the possibly modified with id as a source event
fireEventToFocusWidget :: P.Event -> SDLIO P.Event
fireEventToFocusWidget ee = do
    mw <- getFocusWidget
    maybe (pure ee) -- return original event if there's no widget in focus
          (\w -> let e = setSrcId (widgetId w) ee in fireEventToWidget e w >> pure e) mw

-- fire event to currently hovered widget - primarily internally used for sending "stop hovering" events          
fireEventToHoverWidget :: P.Event -> SDLIO P.Event
fireEventToHoverWidget ee = do
    mw <- getHoverWidget
    maybe (pure ee) -- return original event if there's no widget in focus
        (\w -> let e = setSrcId (widgetId w) ee in fireEventToWidget e w >> pure e) mw
          
-- fires an event to all widgets that catch given position, collect WidgetIds of all relevant widgets    
fireEventToWidgets :: V2 Int -> P.Event -> SDLIO [WidgetId]
fireEventToWidgets (V2 x y) event = do
    gets widgets >>= mapM fn >>= 
        \ws' -> pure $ Map.foldlWithKey fn1 [] ws'
    where 
        fn1 acc i val = if val then (i:acc) else acc
        fn w = 
            if isInActiveWidget x y w 
            then 
                -- since we don't have sources here yet, setting the source to the current widget for proper handling
                let event' = event { source = (source event) { widgetIds = [widgetId w] } }
                in  (handler w) event' >> pure True 
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
                     cf <- gets curFocusId
                     cw <- if (widgetId wid) == cf then compile2Widget True w' else compile2Widget False w'
                     -- now, update the ActiveWidget in the monad
                     updateWidget i wid { widget = w', compiledWidget = cw }
          ) widm

composeManyHandlers :: [EventHandler] -> P.Event -> SDLIO ()
composeManyHandlers hs evt = mapM_ (fn evt) hs where fn evt h = h evt

----------------------------------------------------------------------------------------
-- So overall idea is: take a PureHandler, AbstractWidget and then call some function 
-- that sets up ActiveWidget with correct id, handler function etc.
-- THIS IS THE MAIN EXTERNAL FUNCTION TO REGISTER HANDLERS WITH WIDGETS!!!
----------------------------------------------------------------------------------------
registerWidgetWithHandler :: AbstractWidget -> PureHandler -> SDLIO Int
registerWidgetWithHandler w h = do
    ws <- gets widgets
    i  <- fmap (+1) (gets idCounter)
    -- creating a monadic handler from pure
    let handler = pureToEventHandler h i
    cf <- gets curFocusId
    cw <- if i == cf then compile2Widget True w else compile2Widget False w
    -- cw <- compile2Widget w
    let ws' = Map.insert i (ActiveWidget { widgetId = i, widget = w, compiledWidget = cw, handler = handler })  ws
    modify' (\s-> s{idCounter = i, widgets = ws'})
    return i

-- register widget with already composed in reader monad pure handler and optional
-- monadic in SDLIO handlers; same as above but adds monadic
registerWidgetWithHandlers :: AbstractWidget -> PureHandler -> [EventHandler] -> SDLIO Int
registerWidgetWithHandlers w h hs = do
    ws <- gets widgets
    i  <- fmap (+1) (gets idCounter)
    -- creating a monadic handler from pure
    let h' = pureToEventHandler h i
    let handler = composeManyHandlers (h':hs)
    cf <- gets curFocusId
    cw <- if i == cf then compile2Widget True w else compile2Widget False w
    -- cw <- compile2Widget w
    let ws' = Map.insert i (ActiveWidget { widgetId = i, widget = w, compiledWidget = cw, handler = handler })  ws
    modify' (\s-> s{idCounter = i, widgets = ws'})
    return i
    
-- converting SDL event to our event inside a monad - to be able to 
-- retrieve widgets right away and fire events to them inside here as needed, too
-- main function connecting SDL with our world
sdlEvent2Event :: SDL.Event -> SDLIO P.Event
sdlEvent2Event event = 
    case SDL.eventPayload event of
            p@(SDL.MouseButtonEvent mb) -> 
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
                    cons   = if button == SDL.ButtonLeft then ELeftClick else ERightClick
                    pos'   = castV2 pos
                    src = EventSource [] pos' (SDL.eventTimestamp event)
                    evt    = cons src clicks
                    -- firing event to all widgets right away, updating source and returning the event
                in  if motion == Released 
                    then fireEventToWidgets pos' evt >>= (\ids -> pure $ evt { source = src { widgetIds = ids } } )
                    else pure $ RawSDLEvent (emptySource event) p
            SDL.MouseMotionEvent me ->  
                let SDL.P pos = SDL.mouseMotionEventPos me
                    pos'  = castV2 pos
                    src   = EventSource [] pos' (SDL.eventTimestamp event)
                    evt   = EMouseHover src
                in  fireEventToWidgets pos' evt >>= 
                        \ids ->
                            -- this has to do with hovering so need to process currently hovering id correctly right here
                            if ids == []
                            then 
                                -- need to send "stopped hovering" event to currently being hovered widget
                                fireEventToHoverWidget (EMouseStoppedHover src)
                                -- setting current hover id to -1 which is mainwindow
                                >> setCurHoverId (-1)
                                >> pure (evt { source = src { widgetIds = ids } })
                            else
                                -- setting current hover id to the first item in the fired events list
                                -- TODO: FIX THIS, has to be handled differently
                                setCurHoverId (Prelude.head ids) 
                                >> pure (evt { source = src { widgetIds = ids } })
            SDL.WindowResizedEvent dt -> 
                let src           = emptySource event
                    size@(V2 w h) = castV2 $ SDL.windowResizedEventSize dt
                in  handleResize w h >> pure (EWindowResized src size)
            SDL.QuitEvent -> pure $ EQuit $ emptySource event
            -- keyboard events are a bit tricky
            -- need to add checks whether TEXT INPUT is on, otherwise process keypresses differently potentially
            -- also, may be easier to move this to filters and here simply send an event further
            SDL.TextInputEvent ev -> do
                -- liftIO $ print $ show ev
                prolongCursor
                let ee = ETextInput (emptySource event) (textInputEventText ev)
                fireEventToFocusWidget ee
            p@(SDL.KeyboardEvent ev) -> do
                prolongCursor
                -- liftIO $ print $ show ev
                let k = keysymKeycode $ keyboardEventKeysym ev
                --liftIO $ print $ show k
                case k of
                    KeycodeBackspace -> 
                        -- checking press only 
                        if (keyboardEventKeyMotion ev) == Pressed 
                        then let ee = EBackspace (emptySource event) in fireEventToFocusWidget ee
                        else pure $ RawSDLEvent (emptySource event) p
                    _ -> pure $ RawSDLEvent (emptySource event) p
        
            p -> pure $ RawSDLEvent (emptySource event) p
    

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