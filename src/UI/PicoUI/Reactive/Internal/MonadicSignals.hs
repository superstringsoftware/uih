{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
    RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, 
    FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module UI.PicoUI.Reactive.Internal.MonadicSignals

where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when, join)
import Control.Applicative

import qualified UI.PicoUI.Reactive.Internal.StatefulSignals as SS

import PreludeFixes

-- hiding our StatefulSignal and it's function inside a monad completely with newtype
-- so that we can make functor etc instances and make a cleaner interface for reactive stuff

-- TODO: IT DOESNT WORK - MONADIC ACTIONS DO NOT GET CALLED. NEED TO INTRODUCE LOGGING TO CHECK WHATS HAPPENING.
newtype SignalM m a = MSS { unMSS :: m (SS.StatefulSignal m a) }

--------------------------------------------------------------------------------
-- Low-level direct access interface
--------------------------------------------------------------------------------
readVal :: MonadIO m => SignalM m a -> m a
readVal msig = unMSS msig >>= SS.readVal
-- modifies the value and notifies listeners
modifyVal :: MonadIO m => SignalM m a -> (a -> a) -> m ()
modifyVal msig f = unMSS msig >>= \s -> SS.modifyVal s f
-- adds a listener
addListener :: MonadIO m => SignalM m a -> SS.Listener m a -> m ()
addListener msig ls = unMSS msig >>= \s -> SS.addListener s ls
-- add listener, return remove function
addListenerWRemove :: MonadIO m => SignalM m a -> SS.Listener m a -> m (m())
addListenerWRemove msig ls = unMSS msig >>= \s -> SS.addListenerWRemove s ls
-- "fire signal eventValue" simply sets the value to eventValue and then fires it to all subscribers
fire sig val = unMSS sig >>= \s -> SS.fire s val
-- listens to changes in the signal and executes an action in the monad in response
-- again, a simple shortcut to addListener
sink :: MonadIO m => SignalM m a -> (a -> m()) -> SignalM m a
_sink sig act = do
    s <- unMSS sig
    SS.sink s act
    pure s
sink sig act = MSS $! _sink sig act

sink' sig act = do
    s <- unMSS sig
    SS.sink s act
    

forceSignal :: MonadIO m => SignalM m a -> m ()
forceSignal msig = readVal msig >>= \val -> pure ()

--------------------------------------------------------------------------------
-- Class instances
--------------------------------------------------------------------------------
instance MonadIO m => Functor (SignalM m) where
    -- fmap :: (a -> b) -> MStatefulSignal m a -> MStatefulSignal m b
    fmap f msig = MSS $ join ((SS.fmapM f) <$> (unMSS msig))

instance MonadIO m => Applicative (SignalM m) where
    pure = MSS • SS.createStatefulSignal
            
        -- MSS (SS.createStatefulSignal x >>= \v -> return v)
        -- MSS • SS.createStatefulSignal
    -- apply :: MonadIO m => StatefulSignal m (a -> b) -> StatefulSignal m a -> m (StatefulSignal m b)
    (<*>) :: SignalM m (a -> b) -> SignalM m a -> SignalM m b
    mf <*> ms = MSS $ join (liftA2 SS.apply (unMSS mf) (unMSS ms))

_pure :: MonadIO m => a -> m (SS.StatefulSignal m a)
_pure x = SS.createStatefulSignal x >>= \v -> return v    


-- MSS { unMSS :: m (SS.StatefulSignal m b) }    
instance MonadIO m => Monad (SignalM m) where
    (>>=) :: forall a b. SignalM m a -> (a -> SignalM m b) -> SignalM m b
    sig >>= fsig = MSS $ readVal sig >>= \s -> unMSS (fsig s)
        
        

-- combine :: (MonadIO m, Semigroup a) => StatefulSignal m a -> StatefulSignal m a -> m (StatefulSignal m a)
instance (MonadIO m, Semigroup a) => Semigroup (SignalM m a) where
    s1 <> s2 = MSS $ join (liftA2 SS.combine (unMSS s1) (unMSS s2))


--------------------------------------------------------------------------------
-- Standard-(ish) FRP functions
--------------------------------------------------------------------------------
unionWith :: MonadIO m => (a -> a -> a) -> SignalM m a -> SignalM m a -> SignalM m a
unionWith f s1 s2 = MSS $ join (liftA2 (SS.unionWith f) (unMSS s1) (unMSS s2))

filterS :: MonadIO m => (a -> Bool) -> SignalM m a -> SignalM m a
filterS f s = MSS $ join $ (SS.filterS f) <$> (unMSS s)

filterJust :: MonadIO m => a -> SignalM m (Maybe a) -> SignalM m a
filterJust inv msig = MSS $ join $ (SS.filterJustInit inv) <$> (unMSS msig)

apply :: MonadIO m => SignalM m (a -> b) -> SignalM m a -> SignalM m b
apply = (<*>)

unions :: MonadIO m => [ SignalM m (a -> a) ] -> SignalM m (a -> a)
unions ss = MSS $ SS.unionsM $! (map unMSS ss)

accum :: MonadIO m => a -> SignalM m (a->a) -> SignalM m a
accum inv msig = MSS $ join $ (SS.accum inv) <$> (unMSS msig)


--------------------------------------------------------------------------------
-- Some tests etc
--------------------------------------------------------------------------------

_test_monadic = do
    let (s1 :: SignalM IO Int) = pure 0
    s1' <- unMSS s1
    putStrLn $ "Hurray" 


_test_signals :: IO ()
_test_signals = do
    let (eup   :: SignalM IO String) = MSS $ _pure "Up"
    -- let (edown :: SignalM IO String) = pure "Down"

    let (edown :: SignalM IO String) = sink (pure "Down") (\s -> putStrLn $ "Got signal: " ++ show s)
    -- let (eup :: SignalM IO String) = sink (pure "Up") (\s -> putStrLn $ "Got signal: " ++ show s)
    let eup' = sink eup (\s -> putStrLn $ "Got signal: " ++ s)

    
    let (counter :: SignalM IO Int) = accum 0 $ 
                    unions [  (+1) <$ eup
                            , (subtract 1) <$ edown
                           ] 
    
{-
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
-}
    let c = sink counter (\i -> putStrLn $ "Counter is: " ++ show i)    
    let inp = do
            l <- getLine 
            if l == "-" 
            then fire edown "Down"
            else fire eup' "Up" -- unMSS eup >>= \e -> SS.fire e "Up"
            inp

    putStrLn "Running network"
    inp    