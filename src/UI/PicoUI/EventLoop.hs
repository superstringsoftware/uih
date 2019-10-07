{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
    RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.EventLoop where

-- this is a huge state monad taking care of low-level SDL interactions

import Control.Monad.Trans.State.Strict
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Exception
import SDL hiding (get)
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
import UI.PicoUI.Middle.AbstractWidgets as P

import UI.PicoUI.Reactive.Internal.StatefulSignals
import UI.PicoUI.Reactive.ReactiveWidgets

import PreludeFixes

-- handleResize w h = recalculateRectangles w h >> compileAllWidgets


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
        let scale = V2 ( fromIntegral realw / fromIntegral width) ( fromIntegral realh / fromIntegral height)
        modify' (\s-> s { scaleXY = scale })
        autos <- gets autoScale
        ren <- getRenderer
        -- setting renderer scale in case we are in high-dpi
        if autos then rendererScale ren $= scale else pure ()
        -- need to init fonts after the scale has been set
        initFonts
        -- setup the initial FRP network
        let logSink e = liftIO (putStrLn ("[EVENT][" ++ show (timestamp $ source e) ++ "]") >> print e)
        sdlSources <- allEvents <$> gets eventSources
        -- main window events
        clickMainWindowEvents <- filterS 
                (\e -> isSourceMainWindow e && isLeftClick 1 e)
                sdlSources
        -- removing focus in case main window was clicked                
        -- sink clickMainWindowEvents $ const doRemoveFocus
        -- sdlS' <- filterS (not • isRawSDL) sdlSource
        -- (addListener sdlS') logSink
        -- (addListener sdlSource) logSink
        -- running the program
        program
        -- handleResize (fromIntegral width) (fromIntegral height)
        -- send initial resize event so that reactive widgets react
        let size = V2 (fromIntegral width) (fromIntegral height)
        fire sdlSources (EWindowResized zeroSource size)
        appLoop True
    ) 
    SDLEmptyState


renderUI :: SDLIO ()
renderUI = do
    renderer <- getRenderer
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    bgClr <- gets Pico.bgColor
    rendererDrawColor renderer $= bgClr
    SDL.clear renderer
    -- now for reactive widgets
    rws <- gets rawWidgets
    mapM_ (fnr renderer) rws
    SDL.rendererRenderTarget renderer $= Nothing
    -- rendering cursor if there's a focus widget
    mfw <- gets focusWidget
    maybe (pure ()) (const $ renderCursor renderer) mfw
    SDL.present renderer
    where fnr ren rw = readVal rw >>= \w -> renderWidgetToScreen w ren
    

appLoop :: Bool -> SDLIO ()
appLoop draw = do
    -- isDirty <- getDirty
    -- if isDirty then renderUI >> setClean else pure ()
    if draw then renderUI else pure ()
    events <- SDL.pollEvents -- get the events queue from SDL
    case events of 
        [] -> appLoop False
        _  -> do
                results <- mapM fireEvent events -- gather results
                let quit = True `elem` results -- checking if any of the results is True
                if quit 
                then get >>= liftIO . print >> liftIO (putStrLn "Good-bye.")
                else appLoop True

-- takes an SDL event, converts it into our event, and runs all registered event handlers - 
-- for now, only inside widgets, eventually all others as well.
fireEvent :: SDL.Event -> SDLIO Bool
fireEvent ev = do
    event <- sdlEvent2Event ev
    -- firing in the reactive sdl source
    sdlSource <- allEvents <$> gets eventSources
    fire sdlSource event
    return $ isQuit event


composeManyHandlers :: [EventHandler] -> P.Event -> SDLIO ()
composeManyHandlers hs evt = mapM_ (fn evt) hs where fn evt h = h evt

    
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
                    then pure evt
                    else pure $ RawSDLEvent (emptySource event) p
            SDL.MouseMotionEvent me ->  
                let SDL.P pos = SDL.mouseMotionEventPos me
                    pos'  = castV2 pos
                    src   = EventSource [] pos' (SDL.eventTimestamp event)
                    evt   = EMouseHover src
                in  pure evt
            SDL.WindowResizedEvent dt -> 
                let src           = emptySource event
                    size@(V2 w h) = castV2 $ SDL.windowResizedEventSize dt
                in  pure (EWindowResized src size)
            SDL.QuitEvent -> pure $ EQuit $ emptySource event
            -- keyboard events are a bit tricky
            -- need to add checks whether TEXT INPUT is on, otherwise process keypresses differently potentially
            -- also, may be easier to move this to filters and here simply send an event further
            SDL.TextInputEvent ev -> do
                -- liftIO $ print $ show ev
                prolongCursor
                let ee = ETextInput (emptySource event) (textInputEventText ev)
                pure ee
            p@(SDL.KeyboardEvent ev) -> do
                prolongCursor
                -- liftIO $ print $ show ev
                let k = keysymKeycode $ keyboardEventKeysym ev
                --liftIO $ print $ show k
                case k of
                    KeycodeBackspace -> 
                        -- checking press only 
                        if keyboardEventKeyMotion ev == Pressed 
                        then let ee = EBackspace (emptySource event) in pure ee
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