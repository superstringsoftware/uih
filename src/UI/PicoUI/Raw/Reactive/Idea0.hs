{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, 
FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- VERY EASY TO USE REACTIVE VAR IN THE MONAD TYPE
-- it is immutable now, may want to add a mutable version with the same interface
-- via type families

module UI.PicoUI.Raw.Reactive.Idea0

where

import Data.Map.Strict as Map hiding (unions)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when)

import Data.IORef

type Id = Int

-- new value, monad
type Listener m a = a -> m ()

data Signal m a = Signal {
    value :: a,
    listeners :: Map.Map Id (Listener m a),
    curId :: !Id
}

type StatefulSignal m a = (m a, -- read value
    (a -> a) -> m (), -- modify value
    Listener m a -> m ()) -- add listener

newSignal a = Signal {value = a, listeners = Map.empty, curId = 0}

-- Well, since the below works we might as well create an explicit object with IORef-d state,
-- since then we can share functions code.
{-
data StatefulSignal m a = StatefulSignal {
    signal    :: IORef (Signal m a),
    readVal   :: StatefulSignal m a -> a,
    modifyVal :: StatefulSignal m a -> a -> m (StatefulSignal m a),
    addListen :: StatefulSignal m a -> Listener m a -> m (StatefulSignal m a)
}
-}

-- trying typical reactive interface

-- never :: m (StatefulSignal m a)
-- never = createStatefulSignal Nothing

-- Functor (which might not be really a functor due to fmap id law being probably broken...)
-- Also, because we have to be in the monad...
fmapM :: MonadIO m => (a -> b) -> StatefulSignal m a -> m (StatefulSignal m b)
fmapM f sig = do
    initVal <- readVal sig
    ret <- createStatefulSignal (f initVal)
    let conn = (\x -> (modifyVal ret) (const (f x)) )
    (addListener sig) conn
    return ret

filterE :: MonadIO m => (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
filterE filt sig = do
    (initVal :: a) <- readVal sig
    ret <- createStatefulSignal initVal
    let conn = (\x -> when (filt x) $ (modifyVal ret) id )
    (addListener sig) conn
    return ret

-- we don't really have simultaneous events in our model, do we???
-- so this function is like combine
unionWith :: MonadIO m => (a -> a -> a) -> StatefulSignal m a -> StatefulSignal m a -> m (StatefulSignal m a)
unionWith f sig1 sig2 = do
    iv1 <- readVal sig1
    iv2 <- readVal sig2
    ret <- createStatefulSignal (f iv1 iv2)
    let conn = (\x -> (modifyVal ret) (const x) )
    (addListener sig1) conn
    (addListener sig2) conn
    return ret 

-- analog of Semigroup <>    
combine :: (MonadIO m, Semigroup a) => StatefulSignal m a -> StatefulSignal m a -> m (StatefulSignal m a)
combine sig1 sig2 = do
    iv1 <- readVal sig1
    iv2 <- readVal sig2
    ret <- createStatefulSignal (iv1 <> iv2)
    let conn = (\x -> (modifyVal ret) (const x) )
    (addListener sig1) conn
    (addListener sig2) conn
    return ret

unions :: MonadIO m => [StatefulSignal m (a -> a)] -> m (StatefulSignal m (a -> a))
-- unions [] = never
-- unions xs = foldr1 (unionWith (.)) xs
unions signals = do
    iv <- readVal (head signals)
    ret <- createStatefulSignal iv
    let conn = (\x -> (modifyVal ret) (const x) )
    mapM_ (\sig -> (addListener sig) conn) signals
    return ret

-- accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a)
-- accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a)
accum :: MonadIO m => a -> StatefulSignal m (a->a) -> m (StatefulSignal m a)
accum initVal sig = do
    ret <- createStatefulSignal initVal
    let conn = (\f -> (modifyVal ret) f )
    (addListener sig) conn
    return ret

-- We want to create a Signal, stateful, that ALSO can hookup into other
-- Signal via listeners. For this, we need to create a generic listener function:
-- for Signal m a we need to create a connector (b -> m ()) that will be able to connect
-- to  Signal m b, via function f::b->a so that we don't keep any b type information in our
-- a signal. The challenge is - this way our signal will be able to hook up only to signals of one
-- type. 
-- Is this an issue?
-- createStatefulSignal :: a -> ...
createStatefulSignal
  :: MonadIO m => a
     -> m
          (m a, -- read value
          (a -> a) -> m (), -- modify value
          Listener m a -> m ()) -- add listener          
createStatefulSignal initVal = do
    cache <- liftIO $ newIORef (newSignal initVal)
    let read = value <$> liftIO (readIORef cache)
    let mod f = do
            v <- liftIO $ readIORef cache
            let val = f (value v)
            let ls = listeners v
            liftIO $ writeIORef cache v { value = val }
            -- run listeners:
            mapM_ (\l -> l val ) ls
    let addL l = liftIO $ modifyIORef' cache (addListenerPure l)
    pure (read, mod, addL)

addListenerPure :: Listener m a -> Signal m a -> Signal m a
addListenerPure l r = 
    let ls  = listeners r
        ci  = curId r
        ls' = Map.insert ci l ls
    in  r { listeners = ls', curId = ci + 1 }

--
readVal (x,_,_) = x
modifyVal (_,x,_) = x
addListener (_,_,x) = x
-- connectors are the most important:
-- they take a function g :: b -> a -> a
-- it takes OLD value, incoming b value, and produces new a value.
-- they are used as a listener from "this" object to any Singal m b
connector s g bval = (modifyVal s) (g bval)

plusOne :: Int -> Int
plusOne x = x + 1

{-
Ok let's try this again.
eup   <- event0 bup   command
edown <- event0 bdown command
(counter :: Behavior Int)
    <- accumB 0 $ unions
        [ (+1)       <$ eup
        , subtract 1 <$ edown
        ]
sink output [text :== show <$> counter] 
-}

_test_signals1 = do
    eup   <- createStatefulSignal "Up"
    edown <- createStatefulSignal "Down"
    f1 <- fmapM (const (+1) ) eup
    f2 <- fmapM (const (subtract 1) ) edown
    u  <- unions [f1,f2]
    counter <- accum 0 u

    let sink i = putStrLn $ "Counter is: " ++ show i
    (addListener counter) sink

    let inp = do
            l <- getLine 
            if l == "-" 
            then (modifyVal edown) (const "Down") 
            else (modifyVal eup)   (const "Up") 
            inp

    putStrLn "Running network"
    inp


_test_signals = do
    eup <- createStatefulSignal "Click"
    pl1 <- createStatefulSignal plusOne
    let conn1 = (connector pl1) (\g -> const plusOne)
    (addListener eup) conn1
    
    ac0 <- createStatefulSignal (0 :: Int)
    let conn2 = (connector ac0) ($)
    (addListener pl1) conn2

    let sink i = putStrLn $ "Counter is: " ++ show i
    (addListener ac0) sink

    let inp = do
            getLine 
            (modifyVal eup) (const "Click") -- firing "click" events
            inp
    
    putStrLn "Running network"
    inp



