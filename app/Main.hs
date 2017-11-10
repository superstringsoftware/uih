{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main where

import Screen.LowLevelWidgets
import Color
import Linear
import Data.Text
import Data.Monoid


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


button = (injectWidget buttonPanel) <> (injectWidget buttonLabel)
mainUI = (injectWidget mainWidget) <> (injectWidget textLabel) <> button

instance Widget () where
  render _ = return ()

instance Widget Panel where
  render = print

instance Widget Text where
  render = print


main = do
  render mainUI

{-
program :: SDLIO ()
program = do
    dumpSDLState >> initializeAll >> dumpSDLState
    st <- get
    let renderer = mainRenderer st
    renderUI 1200 800
    appLoop

runProgram :: SDLIO a -> IO a
runProgram prog = evalStateT prog SDLEmptyState

main :: IO ()
main = do
  putStrLn "Starting main..."

  --fls <- listDirectory "/Library/Fonts"
  --mapM_ print fls
  runProgram program



appLoop :: SDLIO ()
appLoop = do
  renderer <- gets mainRenderer
  events <- liftIO SDL.pollEvents -- get the events queue from SDL
  results <- mapM checkEvent events -- gather results
  let quit = any (== True) results -- checking if any of the results is True
  unless quit appLoop

renderUI :: Int -> Int -> SDLIO ()
renderUI w h = do
    renderer <- gets mainRenderer
    setRenderDrawColorRGBA renderer $ mdWhite
    -- putStrLn "Inside renderUI: set color"
    -- rendererDrawColor renderer $= V4 0 0 0 0
    SDL.clear renderer
    -- putStrLn "Cleared renderer"
    mapM_ renderGlobal (fullUI w h)
    renderGlobal testBox

    -- putStrLn "Mapped all rendering actions"
    SDL.present renderer
    -- putStrLn "Presented renderer"

-- returns True if need to quit, false otherwise
checkEvent :: SDL.Event -> SDLIO Bool
checkEvent event = do
    --liftIO $ print $ show $ event
    renderer <- gets mainRenderer
    case SDL.eventPayload event of
        SDL.WindowResizedEvent dt -> do
            let (V2 w h) = SDL.windowResizedEventSize dt
            -- putStrLn $ "Window resized - " ++ show w ++ " " ++ show h
            renderUI (fromIntegral w) (fromIntegral h)
            return False
        SDL.QuitEvent -> return True
        SDL.KeyboardEvent keyboardEvent -> return False
        SDL.TextInputEvent ev -> do
                                    liftIO $ print $ show ev
                                    return False
        _ -> return False

-}
