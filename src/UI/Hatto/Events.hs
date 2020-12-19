-- reusing SDL Events as is for now - as they are already a wrapper on top of the Raw SDL Events, so adding another abstraction is not the best idea
{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module UI.Hatto.Events 
(
    SE.EventPayload(..)
  , EventHandler
  , EventHandlerM
  , Event(..)
  , SE.Timestamp
  , EventData(..)
  , sdlEvent2Event
  , onClick
  , onLeftClick
  , isLeftClick
  , onRightClick
  , isRightClick
)

where

import qualified SDL.Event as SE
import Data.Text
import Control.Monad.Reader

-- repackaging SDL Events into our aggregate type
data Event = SDLEvent {
    timestamp :: SE.Timestamp
  , payload :: SE.EventPayload
} | Event {
    timestamp :: SE.Timestamp
  , sourceId :: Text
  , eventPayload :: EventData
}

data EventData = Click | NoEvent

sdlEvent2Event :: SE.Event -> Event
sdlEvent2Event (SE.Event ts pl) = SDLEvent ts pl

type EventHandler  w = w -> Reader Event w
type EventHandlerM m = Event -> m ()

instance Show (EventHandler w) where show _ = "[PureEventHandler]"

-- various filters for events
filterEvent :: MonadIO m  => m () -> (Event -> Bool) -> EventHandlerM m
filterEvent handler cond e = if cond e then handler else pure ()

filterEventPayload :: MonadIO m  => m () -> (SE.EventPayload -> Bool) -> EventHandlerM m
filterEventPayload handler cond e@(SDLEvent _ epl) = if cond epl then handler else pure ()

onClick :: MonadIO m  => m () -> EventHandlerM m
onClick handler (SDLEvent _ (SE.MouseButtonEvent mbe)) = handler
onClick _ _ = pure ()

onLeftClick :: MonadIO m  => m () -> EventHandlerM m
onLeftClick handler e = if isLeftClick e then (liftIO $ putStrLn "REALLY LeftClick!") >> handler else pure ()

isLeftClick :: Event -> Bool
isLeftClick (SDLEvent _ (SE.MouseButtonEvent mbe)) = SE.mouseButtonEventButton mbe == SE.ButtonLeft
isLeftClick _ = False

isRightClick :: Event -> Bool
isRightClick (SDLEvent _ (SE.MouseButtonEvent mbe)) = SE.mouseButtonEventButton mbe == SE.ButtonRight 
isRightClick _ = False

onRightClick :: MonadIO m  => m () -> EventHandlerM m
onRightClick handler e = if isRightClick e then handler else pure ()
