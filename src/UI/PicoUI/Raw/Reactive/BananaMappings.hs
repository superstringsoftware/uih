{-# LANGUAGE ScopedTypeVariables #-}
    -- allows pattern signatures like
    -- do
    --     (b :: Behavior Int) <- stepper 0 ...
{-# LANGUAGE RecursiveDo #-}
    -- allows recursive do notation
    -- mdo
    --     ...
module UI.PicoUI.Raw.Reactive.BananaMappings where

-- initial hook up into reactive banana
-- UNFORTUNATELY, MomentIO monad forces us to be in the IO, and we don't want ot be in the IO.
-- So, no reactive banana for now.

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

import qualified SDL


{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

data SDLEventSources = SDLEventSources {
    sdlKeyboardEventS :: EventSource SDL.Event
  , sdlTextEventS     :: EventSource SDL.Event
  , sdlMouseEventS    :: EventSource SDL.Event
  , sdlGenericEventS  :: EventSource SDL.Event
}

makeSDLEventSources :: IO SDLEventSources
makeSDLEventSources = SDLEventSources <$> newAddHandler <*> newAddHandler <*> newAddHandler <*> newAddHandler
    
-- mkEventStream = newAddHandler >>= fromAddHandler . addHandler

sdlEventLoop :: SDLEventSources -> IO ()
sdlEventLoop sdlSource = loop
    where 
    loop = do
        events <- SDL.pollEvents -- get the events queue from SDL
        mapM_ fn events -- gather results
        loop
        where fn event = 
                    case SDL.eventPayload event of
                            SDL.KeyboardEvent    ev -> fire (sdlKeyboardEventS sdlSource) event
                            SDL.TextEditingEvent ev -> fire (sdlTextEventS     sdlSource) event
                            SDL.TextInputEvent   ev -> fire (sdlTextEventS     sdlSource) event
                            SDL.MouseMotionEvent ev -> fire (sdlMouseEventS    sdlSource) event
                            SDL.MouseButtonEvent ev -> fire (sdlMouseEventS    sdlSource) event
                            SDL.MouseWheelEvent  ev -> fire (sdlMouseEventS    sdlSource) event
                            _                       -> fire (sdlGenericEventS  sdlSource) event

-- base network
sdlNetworkDescription :: SDLEventSources -> MomentIO ()
sdlNetworkDescription sdlSource = do
    sdlEvents <- fromAddHandler $ addHandler (sdlTextEventS sdlSource)

    reactimate $ putStrLn . show <$> sdlEvents
    return ()
