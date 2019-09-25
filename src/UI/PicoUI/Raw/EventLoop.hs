{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.Raw.EventLoop where

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
import UI.PicoUI.Raw.SDLIO
import UI.PicoUI.Raw.PureHandlers

import PreludeFixes

-- to process an event, we have to simply runReader with an event!
handleEventPure :: Event -> Widget -> PureHandler -> Widget
handleEventPure e w handler = runReader (handler w) e

renderUI :: SDLIO ()
renderUI = do
    renderer <- getRenderer
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    rendererDrawColor renderer $= V4 255 255 255 0
    SDL.clear renderer
    ws <- gets widgets
    -- liftIO $ putStrLn $ "Got widgets: " ++ show (Map.length ws)
    liftIO $ mapM_ (flip renderWidgetToScreen $ renderer) ws
    SDL.present renderer
    

appLoop :: SDLIO ()
appLoop = do
    -- isDirty <- getDirty
    -- if isDirty then renderUI >> setClean else pure ()
    renderUI
    events <- SDL.pollEvents -- get the events queue from SDL
    results <- mapM checkEvent events -- gather results
    let quit = any (== True) results -- checking if any of the results is True
    unless quit appLoop

-- EVENT HANDLING is tricky
-- There are no ready design decisions (yes, FRP, but banana forces you to IO which we don't want)
-- Looks like it makes sense to defer event handling to actual handlers, since we need to keep track of 
-- any modifier keys that are down when the new event occurs etc - so, some way to track the state
-- of the keyboard and mouse and pass it with an event

-- So, additional state of the input devices which is passed with each event and updated by the main cycle
-- as events are proceeding?
checkEvent :: SDL.Event ->  SDLIO Bool
checkEvent event = do
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
            ws <- getCollidingWidgets (fromIntegral x) (fromIntegral y)
            mapM_ (\(i,w) -> do
                            liftIO $ putStrLn $ "Clicked widget #" ++ show i 
                                                  ++ ": " ++ show button ++ show clicks
                            hm <- getPureHandler i
                            maybe (pure ())
                                  (\h -> do 
                                            let w' = handleEventPure event w h
                                            updateWidget i w'
                                  ) hm
                            ) ws
            return False
        SDL.MouseMotionEvent me -> do 
            let P (V2 x y) = SDL.mouseMotionEventPos me
            -- liftIO $ putStrLn $ "Mouse moved to: " ++ show x ++ ", " ++ show y
            -- converting into mouse hover event if any widget is under the mouse and firing it to MM handlers
            ws <- getCollidingWidgets (fromIntegral x) (fromIntegral y)
            mapM_ (\(i,_) -> setCurFocusId i) ws
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