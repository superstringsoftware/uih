{-# LANGUAGE ScopedTypeVariables #-}
module UI.PicoUI.Reactive.Internal.PureSignals

where

import Data.IntMap.Strict as Map hiding (unions, unionWith)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when, join)
import System.IO.Unsafe (unsafePerformIO)

import Data.IORef

data Signal m a = Signal {
    value :: !a,
    listeners :: Map.IntMap (a -> m()),
    curId :: !Int
}

newSignal a = Signal {value = a, listeners = Map.empty, curId = 0}

type StatefulSignal m a = (a, (a -> a) -> m (), (a -> m ()) -> IO ())

{-
createStatefulSignal
  :: Monad m => a
     ->   (a, -- read value
          (a -> a) -> m (), -- modify value
          (a -> m ()) -> IO ()) -- add listener     
-}
createStatefulSignal initVal = unsafePerformIO $ do
    -- liftIO $ putStrLn "Creating stateful signal"
    cache <- newIORef (newSignal initVal)
    -- can we make at least read operation pure???
    let read = value $ unsafePerformIO $ readIORef cache
    let mod f = do
            let v = unsafePerformIO $ readIORef cache
            let val = f (value v)
            let ls = listeners v
            -- liftIO (putStrLn $ "Calling modify with " ++ show (length ls) ++ " listeners" )
            pure $ unsafePerformIO $ writeIORef cache v { value = val }
            -- putStrLn $ "Value: " ++ show val
            -- run listeners:
            mapM_ (\l -> l val ) ls
    let addL l = modifyIORef' cache (addListenerPure l)                     
    pure (read, mod, addL)

readVal (x,_,_) = x
modifyVal (_,x,_) = x
addListener (_,_,x) = x    

addListenerPure :: Monad m => (a -> m ()) -> Signal m a -> Signal m a
addListenerPure l r = 
    let ls  = listeners r
        ci  = (curId r) + 1
        ls' = Map.insert ci l ls
    in  r { listeners = ls', curId = ci }    

_test_signals :: IO ()
_test_signals = do    
    let (sig :: StatefulSignal IO String) = createStatefulSignal "Hello"
    (addListener sig) (\s -> putStrLn $ "Showing " ++ s)
    (addListener sig) (\s -> putStrLn $ "Showing 2 " ++ s)
    let inp = do
            l <- getLine 
            (modifyVal sig) (const l) 
            putStrLn $ "Value is: " ++ (readVal sig)
            inp
    putStrLn "Running network"
    inp