{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators #-}

module UI.PicoUI.Raw.EventLoop where

-- this is a huge state monad taking care of low-level SDL interactions

import Control.Monad.Trans.State.Strict
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

import PreludeFixes


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

-- Main Event Loop - this method plays central role in connecting underlying SDLIO monad from where
-- SDL events originate and ManagerMonad, which handles firing transformed events to event handlers
-- returns True if need to quit, false otherwise
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