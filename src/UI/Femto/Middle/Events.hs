-- reusing SDL Events as is for now - as they are already a wrapper on top of the Raw SDL Events, so adding another abstraction is not the best idea
{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module UI.Femto.Middle.Events 
(
    SE.EventPayload(..)
  , PureEventHandler
  , MonadEventHandler
  , Event(..)
  , SE.Timestamp
  , EventData(..)
  , sdlEvent2Event
)

where

import qualified SDL.Event as SE

import Data.Text

import UI.Femto.SDL.SDLMonad

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

type PureEventHandler  w = w -> Reader Event w
type MonadEventHandler u = Event -> FemtoUIM ()

instance Show (PureEventHandler w) where show _ = "[PureEventHandler]"

