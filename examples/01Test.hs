{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists,
    TypeSynonymInstances, RecordWildCards #-}
module Main where

import Color
import Linear
import Data.Text hiding (any)
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad

import UI.PicoUI.PicoUIMonad
import UI.PicoUI.EventLoop
import UI.PicoUI.Middle.PureHandlers
import UI.PicoUI.Middle.Handlers
import UI.PicoUI.Middle.AbstractWidgets

-- import Data.Foldable

import qualified SDL as SDL

-- import SDL.Raw.Types (Rect(..))
testProgram :: SDLIO ()
testProgram = 
  registerWidgetWithHandlers 
    fig1
    compositeHandler -- pure handler that changes bg colors
    [filteredHandlerSDLIO (isLeftClick 1) (\e -> liftIO $ putStrLn "Clicked button UP")] >>
  registerWidgetWithHandlers 
    fig2
    compositeHandler -- pure handler that changes bg colors
    [filteredHandlerSDLIO (isLeftClick 1) (\e -> liftIO $ putStrLn "Clicked button DOWN")] -- IO handler for hover
    
  >> registerWidgetWithHandler but pure
  >> pure ()


{-
Example from reactive banana - how can we do the same in our framework?

eup   <- event0 bup   command
edown <- event0 bdown command

(counter :: Behavior Int)
    <- accumB 0 $ unions
        [ (+1)       <$ eup
        , subtract 1 <$ edown
        ]

sink output [text :== show <$> counter] 
-}  

type Time = Int
newtype Signal a = Signal { runSignal :: Time -> Maybe a }

never :: Signal a
never = Signal (\t -> Nothing)

unionWith :: (a -> a -> a) -> Signal a -> Signal a -> Signal a
unionWith f sig1 sig2 = 
    let g t = 
                let v1 = (runSignal sig1) t
                    v2 = (runSignal sig2) t
                    res = case v1 of
                            Nothing -> case v2 of
                                        Nothing -> Nothing
                                        Just _  -> v2
                            Just x1  -> case v2 of
                                        Nothing -> v1
                                        Just x2 -> Just $ f x1 x2                                                
                in  res
    in  Signal g

unions :: [Signal (a -> a)] -> Signal (a -> a)
unions [] = never
unions xs = Prelude.foldr1 (unionWith (.)) xs

-- Ok, naive approach breaks on accum :)
{-
accum :: a -> Signal (a -> a) -> Signal a
accum ini sigFunc = 
    let g acc t = 
                    let mf   = (runSignal sigFunc) t
                        res = case mf of
                                Nothing -> acc
                                Just f  -> f acc
                    in  res
        acc' = g acc t
    in  Signal (g acc')
-}        


instance Functor Signal where
    -- fmap :: (a -> b) -> Event a -> Event b
    fmap f Signal{..} = 
        let g' t = case runSignal t of
                        Nothing -> Nothing
                        Just v  -> Just $ f v
        in Signal g'


    
redOn2Click = filteredHandler (isLeftClick 2) (changeBackground $ BGColor $ mRed 500)
blueOnClick = filteredHandler (isLeftClick 1) (changeBackground $ BGColor $ mBlue 500)
-- composite handler combining the 2
-- compositeHandler w = redOn2Click w >>= blueOnClick

-- This is a typical button that highlights when hovered upon
greyOnStopHover = filteredHandler isStoppedHover (changeBackground $ BGColor $ mGrey 500)
blueOnHover     = filteredHandler isHover        (changeBackground $ BGColor $ mBlue 500)
compositeHandler w = greyOnStopHover w >>= blueOnHover

main :: IO ()
main = runSDLIO testProgram >> pure ()

fig1 = Label {
  fontData = FontDataDefault,
  text = "Up",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 40 40 60 40,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 0 0 0 0
}

fig2 = Label {
  fontData = FontDataDefault,
  text = "Down",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 120 40 60 40,
  background = BGColor $ mGrey 700, 
  cacheRect = V4 0 0 0 0
}

but = Label {
  fontData = FontDataDefault,
  text = "0",
  valign = CenterAlign, halign = CenterAlign,
  layout = l_TL 40 100 140 60,
  background = BGColor $ mGrey 500, 
  cacheRect = V4 0 0 0 0
}






