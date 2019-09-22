{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main where

import Color
import Linear
import Data.Text hiding (any)
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad

import UIH.SDL2.RenderMonad
import UIH.SDL2.System
import qualified SDL as SDL

runProgram :: SDLIO a -> IO a
runProgram prog = evalStateT prog SDLEmptyState

main :: IO ()
main = do
  putStrLn "Starting main..."
  runProgram program











{-
textLabel = MkWidget {
    widget = "Hello World" :: Text
  , box = Box {
            globalX = 300
          , globalY = 100
          , parentX = 300
          , parentY = 100
          , width   = 0
          , height  = 0
      }
  }

solidBlackBorder = Just $ Border {
    width = 1
  , color = mdBlack
  , style = Solid
}

buttonPanel = MkWidget {
    box = Box {
            globalX = 300
          , globalY = 400
          , parentX = 300
          , parentY = 400
          , width   = 0
          , height  = 0
      }
  , widget = Panel {
        shadow = Nothing
      , borderTop = solidBlackBorder
      , borderRight = solidBlackBorder
      , borderBottom = solidBlackBorder
      , borderLeft = solidBlackBorder
      , padding = V4 10 10 10 10
      , color   = mdGrey 500
      }
  }

buttonLabel = MkWidget {
    widget = "OK" :: Text
  , box = Box {
            globalX = 0
          , globalY = 0
          , parentX = 10
          , parentY = 10
          , width   = 0
          , height  = 0
      }
  }

mainWidget = MkWidget {
      widget = ()
    , box = Box {
          globalX = 0
        , globalY = 0
        , parentX = 0
        , parentY = 0
        , width   = 0
        , height  = 0
    }
  }


-- building UI via cool monoid
-- button :: WidgetTree SDLState
-- button = (injectWidget buttonPanel) <> (injectWidget buttonLabel)
mainUI :: WidgetTree SDLState
mainUI = (injectWidget mainWidget)  <> (injectWidget textLabel) -- <> button

-}
