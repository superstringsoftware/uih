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

import UIH.UI.Widgets
import UIH.SDL2.RenderWidgets

import Color

-- stacking manager monad and SDLIO together
type SDLUI = ManagerMonadT SDLIO

runSDLUI :: SDLUI a -> IO a
runSDLUI prog = runSDLIO $ evalStateT prog initUIState

initializeAll :: SDLUI ()
initializeAll = lift ((liftIO initStateIO) >>= put >> initFonts >> dumpSDLState)

-- hh
renderUI :: SDLUI ()    
renderUI = do
    renderer <- lift $ gets mainRenderer
    ws <- gets widgets
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    rendererDrawColor renderer $= V4 255 255 255 0
    SDL.clear renderer
    lift $ mapM_ renderScreen ws
    SDL.present renderer
    
appLoop :: SDLUI ()
appLoop = do
    renderUI
    renderer <- lift $ gets mainRenderer
    events <- SDL.pollEvents -- get the events queue from SDL
    results <- mapM (checkEvent renderUI) events -- gather results
    let quit = any (== True) results -- checking if any of the results is True
    unless quit appLoop

-- Main Event Loop
-- returns True if need to quit, false otherwise
checkEvent :: SDLUI () -> SDL.Event ->  SDLUI Bool
checkEvent renUI event = do
    --liftIO $ print $ show $ event
    renderer <- lift $ gets mainRenderer
    case SDL.eventPayload event of
        SDL.WindowResizedEvent dt -> do
            let (SDL.V2 w h) = SDL.windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            renUI --(fromIntegral w) (fromIntegral h)
            return False
        SDL.QuitEvent -> return True
        SDL.KeyboardEvent ev -> do
                --liftIO $ print $ show ev
                return False
        SDL.MouseMotionEvent me -> do 
            let P (V2 x y) = SDL.mouseMotionEventPos me
            -- liftIO $ putStrLn $ "Mouse moved to: " ++ show x ++ ", " ++ show y
            -- converting into mouse hover event if any widget is under the mouse
            mevs <- getEventSource (fromIntegral x) (fromIntegral y)
            maybe (return False)
                  (\evs@(i,_) -> fireEvent (MM.Event EvHover evs) >> setCurrentFocusId (Just i) >> return False)
                  mevs
        SDL.TextEditingEvent ev -> do
                liftIO $ print $ show ev
                return False
        SDL.TextInputEvent ev -> do
                liftIO $ print $ show ev
                -- alterTextWidget (textInputEventText ev)
                return False
        _ -> return False