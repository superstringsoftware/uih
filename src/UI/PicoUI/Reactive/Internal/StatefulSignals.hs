{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
    RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, 
    FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeFamilies, InstanceSigs, LambdaCase #-}

-- VERY EASY TO USE REACTIVE VAR IN THE MONAD TYPE
-- it is immutable now, may want to add a mutable version with the same interface
-- via type families

module UI.PicoUI.Reactive.Internal.StatefulSignals
(
    Listener,
    StatefulSignal,

    fmapM,
    fmapMM,
    (<^$>),
    (<^$),
    (<^$^),
    (<^@>),
    filterS,
    unionWith,
    combine,
    unions,
    unionsM,
    accum,
    apply,
    applyE,
    liftA2M,
    depend,
    filterApply,
    filterApplyE,
    filterJust,
    filterJustInit,

    fire,
    sink,
    createStatefulSignal,
    readVal,
    modifyVal,
    addListener,
    addListenerWRemove
)

where

import Data.Map.Strict as Map hiding (unions, unionWith)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when, join)
import System.IO.Unsafe (unsafePerformIO)

import Data.IORef

import PreludeFixes

infixl 4 <^$>
infixl 4 <^$
infixl 4 <^$^
-- infixl 4 $$>
infixl 4 <^@>

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
    Listener m a -> m (), -- add listener
    Listener m a -> m (m ())) -- add listener and return remove listener action


-- Getters for specific functions for our stateful object
-- reads the value
readVal :: MonadIO m => StatefulSignal m a -> m a
readVal (x,_,_,_) = x
-- modifies the value and notifies listeners
modifyVal :: MonadIO m => StatefulSignal m a -> (a -> a) -> m ()
modifyVal (_,x,_,_) = x
-- adds a listener
addListener :: MonadIO m => StatefulSignal m a -> Listener m a -> m ()
addListener (_,_,x,_) = x
-- add listener, return remove function
addListenerWRemove :: MonadIO m => StatefulSignal m a -> Listener m a -> m (m())
addListenerWRemove (_,_,_,x) = x
-- "fire signal eventValue" simply sets the value to eventValue and then fires it to all subscribers
fire sig val = modifyVal sig (const val)
-- listens to changes in the signal and executes an action in the monad in response
-- again, a simple shortcut to addListener
sink :: MonadIO m => StatefulSignal m a -> (a -> m()) -> m ()
sink = addListener




-- newtype StatefulSignalM m a = SS { unSS :: StatefulSignal m a }

newSignal a = Signal {value = a, listeners = Map.empty, curId = 0}

-- trying typical reactive interface

-- never :: m (StatefulSignal m a)
-- never = createStatefulSignal Nothing

-- Functor (which might not be really a functor due to fmap id law being probably broken...)
-- Also, because we have to be in the monad...
fmapM :: MonadIO m => (a -> b) -> StatefulSignal m a -> m (StatefulSignal m b)
fmapM f sig = do
    initVal <- readVal sig
    ret     <- createStatefulSignal (f initVal)
    addListener sig (modifyVal ret • const • f)
    return ret

-- this hints that StatefulSignal is a monad, even if not a functor. Or does it?  
-- or some sort of a meta monad???? How do we express this relationship???
-- we can do unsafePerformIO of course, but it's a longer exploration so leaving for the future.  
-- The signature is almost like: 
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- However, since to be traversable StatefulSignal has to be Functor and Foldable,
-- we can't really do that. Also, we ask for MonadIO, not just any monad.
fmapMM :: MonadIO m => (a -> m b) -> StatefulSignal m a -> m (StatefulSignal m b)
fmapMM f sig = do
    initVal <- readVal sig
    iv <- f initVal
    ret <- createStatefulSignal iv
    let conn e = f e >>= modifyVal ret • const
    addListener sig conn        
    return ret

-- sort of like pure in Applicative but again, since we are in a monad...    
-- pureM :: MonadIO m => a -> m (StatefulSignal m a)


-- using ^ to indicate it's monadic    
f <^$> sig = fmapM f sig
x <^$  sig = tagM x sig  
x <^$^ sig = tagMM x sig 

-- analog of Functor's (<$):: a -> f b -> f a  
tagM :: MonadIO m => b -> StatefulSignal m a -> m (StatefulSignal m b)
tagM val sig = do
    ret <- createStatefulSignal val
    let conn x = modifyVal ret (const val)
    addListener sig conn
    return ret

tagMM :: MonadIO m => b -> m (StatefulSignal m a) -> m (StatefulSignal m b)    
tagMM val msig = msig >>= tagM val

-- probably the most versatile function:
-- takes a *pure* function from signal b and an existing value of signal a, that produces a new value of signal a
-- makes signal a depend on signal b via applying this function.
-- This function allowed to implement some non-standard widget functionality, e.g.
-- checking hover events via event coordinates etc. Also, since it allows establishing links
-- after the signals were created, gives a lot of flexibility. Maybe at the expense of theoretical soundness.

-- OK, TURNS OUT IT CAN EASILY BE CHANGED INTO fmapM f >>= accum ... !!!
depend :: MonadIO m => (b -> a -> a) -> StatefulSignal m a -> StatefulSignal m b -> m ()
depend f sig onSig = do
    let conn b = modifyVal sig (f b)
    addListener onSig conn
    
    

-- Filter is simple on one hand, on the other - take a case of SDL, where 
-- we do want to filter events into different types.
-- This straightforward approach will have us go AS MANY TIMES AS THERE ARE FILTERS
-- through the list of subscribers, even though more efficient would be to go through it once
-- and dispatch only correct events to correct listeners.

-- One approach to make it: have another map that maps filter functions to signals and make dispatching
-- in one go. So, instead of creating a new signal here, create only one and then expand it.
filterS :: MonadIO m => (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
filterS filt sig = do
    ret <- readVal sig >>= createStatefulSignal
    let conn x = when (filt x) $ modifyVal ret (const x)
    addListener sig conn
    return ret

-- same as filterS, but we give it an initial value explicitly
filterSInit :: MonadIO m => a -> (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
filterSInit initVal filt sig = do
    ret <- createStatefulSignal initVal
    let conn x = when (filt x) $ modifyVal ret (const x)
    addListener sig conn
    return ret

filterJustInit :: MonadIO m => a -> StatefulSignal m (Maybe a) -> m (StatefulSignal m a)
filterJustInit initVal sig = do
    ret <- createStatefulSignal initVal
    let conn x = when (filt x) $ let (Just x') = x in modifyVal ret (const x')
    addListener sig conn
    return ret 
    where filt e = case e of
            Nothing -> False
            Just _  -> True
    
-- returns Maybe, but in fact it is guaranteed to only have Just values - the issue is 
-- in the initial value...    
filterJust :: MonadIO m => StatefulSignal m (Maybe a) -> m (StatefulSignal m (Maybe a))
filterJust = filterS (\case
                        Nothing -> False
                        Just _  -> True ) 


-- we don't really have simultaneous events in our model, do we???
-- so this function is like combine
unionWith :: MonadIO m => (a -> a -> a) -> StatefulSignal m a -> StatefulSignal m a -> m (StatefulSignal m a)
unionWith f sig1 sig2 = do
    iv1 <- readVal sig1
    iv2 <- readVal sig2
    ret <- createStatefulSignal (f iv1 iv2)
    let conn = modifyVal ret • const
    addListener sig1 conn
    addListener sig2 conn
    return ret 

apply :: MonadIO m => StatefulSignal m (a -> b) -> StatefulSignal m a -> m (StatefulSignal m b)
apply fsig sig = do
    inf <- readVal fsig
    inv <- readVal sig
    ret <- createStatefulSignal (inf inv)
    let conn1 f = readVal sig  >>= \v -> modifyVal ret (const (f v))
    let conn2 v = readVal fsig >>= \f -> modifyVal ret (const (f v))
    addListener fsig conn1
    addListener  sig conn2
    return ret

f <^@> s = apply f s

-- see the note for filterApplyE
applyE :: MonadIO m => StatefulSignal m (a -> b) -> StatefulSignal m a -> m (StatefulSignal m b)
applyE fsig sig = do
    inf <- readVal fsig
    inv <- readVal sig
    ret <- createStatefulSignal (inf inv)
    let conn1 f = readVal sig  >>= \v -> modifyVal ret (const (f v))
    addListener fsig conn1
    return ret


-- only let the signal through if a filter signal is true
filterApply :: MonadIO m => StatefulSignal m (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
filterApply filtSig sig = do
    inv <- readVal sig
    ret <- createStatefulSignal inv
    let conn1 f = readVal sig     >>= \e -> when (f e) $ modifyVal ret (const e)
    let conn2 e = readVal filtSig >>= \f -> when (f e) $ modifyVal ret (const e)
    addListener filtSig conn1
    addListener     sig conn2
    return ret

-- ok, the above case showed (when we tried to define on click) that the difference between Behaviors and
-- Events is not just aesthetics. E.g., we need to ONLY apply filter when the Filter changes in some cases -
-- so in essence, it's an event. For now, introducing a different function, that only listens to changes
-- in the first and not the 2nd.    
-- Probably need the same for apply??
filterApplyE :: MonadIO m => StatefulSignal m (a -> Bool) -> StatefulSignal m a -> m (StatefulSignal m a)
filterApplyE filtSig sig = do
    inv <- readVal sig
    ret <- createStatefulSignal inv
    let conn1 f = readVal sig     >>= \e -> when (f e) $ modifyVal ret (const e)
    addListener filtSig conn1
    return ret
    

-- analog Applicative liftA2, liftA2M f a b = f <^$> a <^@> b
liftA2M :: MonadIO m => (a -> b -> c) -> StatefulSignal m a -> StatefulSignal m b -> m (StatefulSignal m c)
liftA2M f sigA sigB = do
    iv1 <- readVal sigA
    iv2 <- readVal sigB
    ret <- createStatefulSignal (f iv1 iv2)
    let conn1 = (\v1 -> readVal sigB >>= \v2 -> (modifyVal ret) (const (f v1 v2)) )
    let conn2 = (\v2 -> readVal sigA >>= \v1 -> (modifyVal ret) (const (f v1 v2)) )
    (addListener sigA) conn1
    (addListener sigB) conn2
    return ret


-- combines 2 signals of different types with a combination function into a third one    
{-
unionAB :: MonadIO m => (a -> b -> c) -> StatefulSignal m a -> StatefulSignal m b -> m (StatefulSignal m c)
unionAB f sigA sigB = do
    iv1 <- readVal sig1
    iv2 <- readVal sig2
-}

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

-- combines a list of function signals into one signal    
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
-- useful in combination with unions...
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
          Listener m a -> m (), -- add listener     
          Listener m a -> m (m ())) -- add listener and return remove listener function    
createStatefulSignal initVal = do
    -- liftIO $ putStrLn "Creating stateful signal"
    cache <- liftIO $ newIORef (newSignal initVal)
    let read = value <$> liftIO (readIORef cache)
    -- can we make at least read operation pure???
    let readP = value $ unsafePerformIO $ readIORef cache
    let mod f = do
            v <- liftIO $ readIORef cache
            let val = f (value v)
            let ls = listeners v
            -- liftIO (putStrLn $ "Calling modify with " ++ show (length ls) ++ " listeners" )
            liftIO $ writeIORef cache v { value = val }
            -- run listeners:
            mapM_ (\l -> l val ) ls
    let addL l = liftIO $ modifyIORef' cache (addListenerPure l)                     
    -- add listener and return a remove listener function
    -- CAN BE OPTIMIZED TO NOT READ CACHE SEVERAL TIMES
    let addLR l = do
            addL l
            id <- curId <$> (liftIO $ readIORef cache)
            let removeL = liftIO $ modifyIORef' cache (removeListenerPure id) 
            pure removeL           
    pure (read, mod, addL, addLR)

addListenerPure :: Listener m a -> Signal m a -> Signal m a
addListenerPure l r = 
    let ls  = listeners r
        ci  = (curId r) + 1
        ls' = Map.insert ci l ls
    in  r { listeners = ls', curId = ci }

removeListenerPure :: Id -> Signal m a -> Signal m a    
removeListenerPure i r = 
    let ls  = listeners r
        ls' = Map.delete i ls
    in  r { listeners = ls' }

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

    counter <- unionsM [  (+1) <^$ eup
                        , (subtract 1) <^$ edown
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





