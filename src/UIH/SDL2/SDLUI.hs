module UIH.SDL2.SDLUI where

-- stitching high-level ManagerMonad and low-level SDLIO (RenderMonad) together to handle SDL based UI
-- CENTRAL ENTRY POINT INTO UI!!!

import UIH.UI.ManagerMonad as MM

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad (unless)
import Control.Monad.Trans.Class

import SDL.Font
import Data.Text hiding (copy, any)
import Control.Exception
import SDL.Exception

import SDL as SDL hiding (initializeAll)

import UIH.SDL2.RenderMonad
import UIH.SDL2.Fonts

import UIH.UI.BasicWidgets
import UIH.SDL2.RenderWidgets

import Color

-- stacking manager monad and SDLIO together
-- u is the user state type that is *for the UI only*!
-- So, users of the library will use SDLUI MyState as their UI program type
-- and can stack another monad on top of this one should they want to
type SDLUI u = ManagerMonadT SDLIO u

-- running main SDLUI program
runSDLUI :: SDLUI u a -> IO a
runSDLUI prog = runSDLIO $ evalStateT prog (initUIState Nothing)

initializeAll :: SDLUI u ()
initializeAll = lift ((liftIO initStateIO) >>= put >> initFonts)

-- hh
renderUI :: SDLUI u ()    
renderUI = do
    renderer <- lift $ gets mainRenderer
    ws <- gets widgets
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    rendererDrawColor renderer $= V4 255 255 255 0
    SDL.clear renderer
    lift $ mapM_ renderScreen ws
    SDL.present renderer
    
appLoop :: SDLUI u ()
appLoop = do
    -- renderUI
    renderer <- lift $ gets mainRenderer
    isDirty <- getDirty
    if isDirty then renderUI >> setClean else pure ()
    events <- SDL.pollEvents -- get the events queue from SDL
    results <- mapM checkEvent events -- gather results
    let quit = any (== True) results -- checking if any of the results is True
    unless quit appLoop

-- Main Event Loop - this method plays central role in connecting underlying SDLIO monad from where
-- SDL events originate and ManagerMonad, which handles firing transformed events to event handlers
-- returns True if need to quit, false otherwise
checkEvent :: SDL.Event ->  SDLUI u Bool
checkEvent event = do
    --liftIO $ print $ show $ event
    renderer <- lift $ gets mainRenderer
    case SDL.eventPayload event of
        SDL.WindowResizedEvent dt -> do
            let (SDL.V2 w h) = SDL.windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            -- renUI --(fromIntegral w) (fromIntegral h)
            setDirty
            return False
        SDL.QuitEvent -> return True
        SDL.KeyboardEvent ev -> do
                --liftIO $ print $ show ev
                let k = keysymKeycode $ keyboardEventKeysym ev
                -- liftIO $ print $ show k
                case k of
                    KeycodeBackspace -> do 
                        mevs <- getFocusWidget
                        maybe (return False)
                              (\evs -> fireEvent (MM.Event EvKbBackspace evs) >> return False)
                              mevs
                    _                -> return False
        SDL.MouseMotionEvent me -> do 
            let P (V2 x y) = SDL.mouseMotionEventPos me
            -- liftIO $ putStrLn $ "Mouse moved to: " ++ show x ++ ", " ++ show y
            -- converting into mouse hover event if any widget is under the mouse and firing it to MM handlers
            mevs <- getEventSource (fromIntegral x) (fromIntegral y)
            maybe (return False)
                  (\evs@(i,_) -> fireEvent (MM.Event EvHover evs) >> setCurrentFocusId (Just i) >> return False)
                  mevs
        SDL.TextEditingEvent ev -> do
                liftIO $ print $ show ev
                return False
        SDL.TextInputEvent ev -> do
                -- liftIO $ print $ show ev
                mevs <- getFocusWidget
                maybe (return False)
                      (\evs -> fireEvent (MM.Event (EvTextInput (textInputEventText ev)) evs) >> return False)
                      mevs
        _ -> return False