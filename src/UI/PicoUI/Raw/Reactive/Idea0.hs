{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, FlexibleInstances #-}

-- VERY EASY TO USE REACTIVE VAR IN THE MONAD TYPE
-- it is immutable now, may want to add a mutable version with the same interface
-- via type families

module UI.PicoUI.Raw.Reactive.Idea0

where

import Data.Map.Strict as Map

import Data.IORef

type Id = Int

-- new value, monad
type Listener m a = a -> m ()

data Signal m a = Signal {
    value :: a,
    listeners :: Map.Map Id (Listener m a),
    curId :: !Id
}

newSignal a = Signal {value = a, listeners = Map.empty, curId = 0}


-- We want to create a Signal, stateful, that ALSO can hookup into other
-- Signal via listeners. For this, we need to create a generic listener function:
-- for Signal m a we need to create a connector (b -> m ()) that will be able to connect
-- to  Signal m b, via function f::b->a so that we don't keep any b type information in our
-- a signal. The challenge is - this way our signal will be able to hook up only to signals of one
-- type. 
-- Is this an issue?
-- createStatefulSignal :: a -> ...
createStatefulSignal initVal = do
    cache <- newIORef (newSignal initVal)
    let read = readIORef cache
    let mod f = do
            v <- readIORef cache
            let val = f (value v)
            let ls = listeners v
            writeIORef cache v { value = val }
            -- run listeners:
            mapM_ (\l -> l val ) ls
    let addListener l = do
            v <- readIORef cache
            writeIORef cache (addListenerPure v l)
    -- connectors are the most important:
    -- they take a function g :: b -> a -> a
    -- it takes OLD value, incoming b value, and produces new a value.
    let conn g bval = do
            v <- readIORef cache
            let val = g bval (value v)
            let ls = listeners v
            writeIORef cache v { value = val }
            -- run listeners:
            mapM_ (\l -> l val ) ls
    pure (read, mod, addListener, conn)

addListenerPure :: Signal m a -> Listener m a -> Signal m a
addListenerPure r l = 
    let ls  = listeners r
        ci  = curId r
        ls' = Map.insert ci l ls
    in  r { listeners = ls', curId = ci + 1 }

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
read (x,_,_,_) = x
modify (_,x,_,_) = x
addListener (_,_,x,_) = x
connector   (_,_,_,x) = x

plusOne :: Int -> Int
plusOne x = x + 1

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
            (modify eup) (const "Click") 
            inp
    
    putStrLn "Running network"
    inp



{-
-- create new reactive var
newVar :: Monad m => a -> ReactiveVar m a
newVar v = ReactiveVar {value = v, listeners = Map.empty, curId = 0}

-- getter
getValue :: Monad m => ReactiveVar m a -> a
getValue r = value r

-- setter: set the new value and run all listeners
setValue :: Monad m => ReactiveVar m a -> a -> m (ReactiveVar m a)
setValue r v = do
    let ls = listeners r
    let oldVal = value r
    mapM_ (\l -> l oldVal v) ls
    pure $ r { value = v }

-- add listener and return an updated ReactiveVar and a function that removes a listener!
addListener :: Monad m => ReactiveVar m a -> Listener m a -> (ReactiveVar m a, ReactiveVar m a -> ReactiveVar m a)
addListener r l = 
    let ls  = listeners r
        ci  = curId r
        ls' = Map.insert ci l ls
        r'  = r { listeners = ls', curId = ci + 1 }
        removeListener rv = 
            let ls1  = listeners rv
                ls1' = Map.delete ci ls1
            in  rv { listeners = ls1' }
    in  (r', removeListener)

-}
