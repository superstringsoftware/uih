{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, 
FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module UI.PicoUI.Reactive.Internal.MonadicSignals

where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when, join)
import Control.Applicative

import UI.PicoUI.Reactive.Internal.StatefulSignals as SS

-- hiding our StatefulSignal and it's function inside a monad completely with newtype
-- so that we can make functor etc instances and make a cleaner interface for reactive stuff

newtype SignalM m a = MSS { unMSS :: m (StatefulSignal m a) }

instance MonadIO m => Functor (SignalM m) where
    -- fmap :: (a -> b) -> MStatefulSignal m a -> MStatefulSignal m b
    fmap f msig = MSS $ join ((fmapM f) <$> (unMSS msig))

instance MonadIO m => Applicative (SignalM m) where
    pure x = MSS $ createStatefulSignal x
    -- apply :: MonadIO m => StatefulSignal m (a -> b) -> StatefulSignal m a -> m (StatefulSignal m b)
    (<*>) :: SignalM m (a -> b) -> SignalM m a -> SignalM m b
    mf <*> ms = MSS $ join (liftA2 SS.apply (unMSS mf) (unMSS ms))

-- combine :: (MonadIO m, Semigroup a) => StatefulSignal m a -> StatefulSignal m a -> m (StatefulSignal m a)
instance (MonadIO m, Semigroup a) => Semigroup (SignalM m a) where
    s1 <> s2 = MSS $ join (liftA2 combine (unMSS s1) (unMSS s2))

-- then for the functions following reactive banana:

unionWith :: MonadIO m => (a -> a -> a) -> SignalM m a -> SignalM m a -> SignalM m a
unionWith f s1 s2 = MSS $ join (liftA2 (SS.unionWith f) (unMSS s1) (unMSS s2))

filterS :: MonadIO m => (a -> Bool) -> SignalM m a -> SignalM m a
filterS f s = MSS $ join (fmap (SS.filterS f) (unMSS s))

apply :: MonadIO m => SignalM m (a -> b) -> SignalM m a -> SignalM m b
apply = (<*>)