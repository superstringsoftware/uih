{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, 
FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- VERY EASY TO USE REACTIVE VAR IN THE MONAD TYPE
-- it is immutable now, may want to add a mutable version with the same interface
-- via type families

module UI.PicoUI.Reactive.Internal.StatefulSignals
(
    Listener,
    StatefulSignal,

    fmapM,
    filterS,
    unionWith,
    combine,
    unions,
    accum,

    fire,
    createStatefulSignal,
    readVal,
    modifyVal,
    addListener
)

where

import Data.Map.Strict as Map hiding (unions, unionWith)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when)

import Data.IORef

type Id = Int

-- new value, monad
type Listener m a = a -> m ()

data Signal m a = Signal {
    value :: !a,
    listeners :: Map.Map Id (Listener m a),
    curId :: !Id
}

type StatefulSignal m a = (m a, -- read value
    (a -> a) -> m (), -- modify value
    Listener m a -> m ()) -- add listener

-- Getters for specific functions for our stateful object
readVal :: MonadIO m => StatefulSignal m a -> m a
readVal (x,_,_) = x
modifyVal :: MonadIO m => StatefulSignal m a -> ((a -> a) -> m ())
modifyVal (_,x,_) = x
addListener :: MonadIO m => StatefulSignal m a -> (Listener m a -> m ())
addListener (_,_,x) = x
-- "fire signal eventValue" simply sets the value to eventValue and then fires it to all subscribers
fire sig val = (modifyVal sig) (const val)



newtype StatefulSignalM m a = SS { unSS :: StatefulSignal m a }

newSignal a = Signal {value = a, listeners = Map.empty, curId = 0}

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

-- what if we try this trick for functor?? 
{-
instance MonadIO m => Functor (StatefulSignalM m) where
    fmap f siga = (SS <$> fmapM f (unSS siga))
-}

filterS :: MonadIO m => (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
filterS filt sig = do
    (initVal :: a) <- readVal sig
    ret <- createStatefulSignal initVal
    let conn = (\x -> when (filt x) $ (modifyVal ret) (const x) )
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

-- just a convenience function to make code look better - sort of a lifted unions
unionsM :: MonadIO m => [m (StatefulSignal m (a -> a)) ] -> m (StatefulSignal m (a -> a))
-- unions [] = never
-- unions xs = foldr1 (unionWith (.)) xs
unionsM signals = do
    s  <- (head signals)
    iv <- readVal s
    ret <- createStatefulSignal iv
    let conn = (\x -> (modifyVal ret) (const x) )
    mapM_ (\sig -> sig >>= \s -> (addListener s) conn) signals
    return ret
    

-- accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a)
-- accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a)
-- takes initial value and a signal of functions and applies it with each change
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
    -- liftIO $ putStrLn "Creating stateful signal"
    cache <- liftIO $ newIORef (newSignal initVal)
    let read = value <$> liftIO (readIORef cache)
    let mod f = do
            v <- liftIO $ readIORef cache
            let val = f (value v)
            let ls = listeners v
            -- liftIO (putStrLn $ "Calling modify with " ++ show (length ls) ++ " listeners")
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

plusOne :: Int -> Int
plusOne x = x + 1

------------------------------------------------------------------------------------------
-- REACTIVE BANANA EXAMPLES RECREATED
------------------------------------------------------------------------------------------

{-
Ok let's try this again, example from banana:

eup   <- event0 bup   command
edown <- event0 bdown command
(counter :: Behavior Int)
    <- accumB 0 $ unions
        [ (+1)       <$ eup
        , subtract 1 <$ edown
        ]
sink output [text :== show <$> counter] 
-}

_test_signals = do
    eup   <- createStatefulSignal "Up"
    edown <- createStatefulSignal "Down"

    counter <- unionsM [  fmapM (const (+1) ) eup
                        , fmapM (const (subtract 1) ) edown
                       ] >>= accum 0

    list    <- unionsM [  fmapM (:) eup
                        , fmapM (const tail) edown
                       ] >>= accum []

    let sink  i = putStrLn $ "Counter is: " ++ show i
    let sink1 i = putStrLn $ "List is: " ++ show i
    (addListener counter) sink
    (addListener list) sink1

    let inp = do
            l <- getLine 
            if l == "-" 
            then (modifyVal edown) (const "Down") 
            else (modifyVal eup)   (const "Up") 
            inp

    putStrLn "Running network"
    inp





