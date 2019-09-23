{-# LANGUAGE PostfixOperators #-}
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

import UIH.UI.AbstractWidgets
import UIH.UI.Renderable

import UIH.SDL2.SDLWidgets
import UIH.SDL2.RenderableSDLWidgets
import UIH.SDL2.RenderableAbstractWidgets

import Data.Map.Strict as Map

import Color

import PreludeFixes

-- stacking manager monad and SDLIO together
-- u is the user state type that is *for the UI only*!
-- So, users of the library will use SDLUI MyState as their UI program type
-- and can stack another monad on top of this one should they want to
type SDLUI u = ManagerMonadT SDLIO u

-- running main SDLUI program
runSDLUI :: SDLUI u a -> IO a
runSDLUI prog = runSDLIO $ evalStateT prog (initUIState Nothing)

initializeAll :: SDLUI u ()
initializeAll = do 
    -- initializing SDL
    liftIO $ putStr "Initializing SDL..."
    lift ((liftIO initStateIO) >>= put >> initFonts)
    liftIO $ putStrLn " done."
    -- initializing ManagerMonad
    pure ()

-- hh
renderUI :: SDLUI u ()    
renderUI = do
    nrc <- getNeedsRecalculation
    if nrc then do
        window <- lift $ gets mainWindow
        V2 width height <- (window.-windowSize?=) -- ok we are reading statevar here from a record field with operators
        -- calculate initial rectangles based on UI layout
        initUI (fromIntegral width) (fromIntegral height)
        widgets <- gets widgets
        liftIO $ mapM_ print widgets
        setNeedsRecalculation False
    else pure ()
    renderer <- lift $ gets mainRenderer
    -- request only dirty widgets map from ManagerMonad
    ws <- getDirtyWidgets
    -- get cached SDLWidgets
    sdlws <- lift $ gets cachedWidgets    
    -- mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
    -- let sdlws' :: SDLIO (Map.Map WidgetId SDLWidget)
    -- "render" them to SDLWidgets
    sdlws' <- lift $ mapM render ws
    -- combine old map and a new map - how to fuse these two operations???
    -- foldrWithKey' :: (k -> a -> Map k b -> Map k b) -> Map k b -> Map k a -> Map k b
    let sdlws'' = Map.foldrWithKey' fn sdlws sdlws'
    SDL.rendererRenderTarget renderer $= Nothing -- rendering to Screen
    rendererDrawColor renderer $= V4 255 255 255 0
    SDL.clear renderer
    lift $ mapM_ renderScreen sdlws''
    SDL.present renderer
    lift $ modify' (\s -> s { cachedWidgets = sdlws'' } )
    where fn k wdg wmap = Map.insert k [wdg] wmap
    
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