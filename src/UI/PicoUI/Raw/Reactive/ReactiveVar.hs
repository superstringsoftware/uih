{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
RecordWildCards, OverloadedLists, PostfixOperators, TypeSynonymInstances, 
FlexibleInstances, RecursiveDo #-}

-- VERY EASY TO USE REACTIVE VAR IN THE MONAD TYPE
-- it is immutable now, may want to add a mutable version with the same interface
-- via type families

module UI.PicoUI.Raw.Reactive.ReactiveVar 
(
    ReactiveVar,
    Listener,

    newVar,
    getValue,
    setValue,
    addListener
)
where

import Data.Map.Strict as Map

type Id = Int

-- old value, new value, monad
type Listener m a = a -> a -> m ()

data ReactiveVar m a = ReactiveVar {
    value :: a,
    listeners :: Map.Map Id (Listener m a),
    curId :: !Id
}


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

addListenerM :: Monad m => ReactiveVar m a -> Listener m a -> m (ReactiveVar m a)
addListenerM r l = 
    let ls  = listeners r
        ci  = curId r
        ls' = Map.insert ci l ls
        r'  = r { listeners = ls', curId = ci + 1 }
    in  pure r'
    
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


_simple_test_listener :: String -> String -> IO ()
_simple_test_listener oldVal newVal = 
    putStrLn $ "Value was: " ++ oldVal ++ " but changed to: " ++ newVal

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

{-
Ok, from the below exersize the way is starting to be seen:

- we "simply" have to connect different reactive vars with pretty typical listeners

The challenges are:

- order of listeners connection, may face the need to use mdo
- Keeping State - this one is the biggest for now.

E.g., in the example below, it all works except accum - because it has to, well, accumulate
the value, but we have nowhere to store the "i" state inside this listener. 

Storing it extenally presents other issues, as we want to be able to work with listeners of any types, 
which means nothing immutable seems to do the job. HOW TO STORE STATE INSIDE THE FUNCTION???

One idea: use listeners that return updated reactive var, and then call them with this returned value the next time.
Adjusted: ... return A NEW LISTENER that we have to call the next time!!! (like a continuation of sorts...)

EXPLORE!!!

Ok, it doesn't work.

Idea2: Mutable State. Might not be as bad in this case, as we will not be *writing* to values from different threads?
Banana does use mutable vars and a lot of unsafePerformIO under the hood. This way things look easy - 
simply store IORef inside a function and update it with accumulating value...

Listeners will play major role of connectors between different reactive vars.
So will contain the var to which they are attached???

-}
    

_simple_test :: IO ()
_simple_test = do
    -- reactive var that sends events, source (eup)
    let e :: ReactiveVar IO String
        e = newVar "Click"
        -- Result of (+1) <$ eup:
        -- reactive var that substitutes event occurences to function application occurences
        f :: ReactiveVar IO (Int -> Int)
        f = newVar (+1)
        -- listener from f to e, that simply sends (+1)s when a "Click" occurs
        l1 :: ReactiveVar IO (Int -> Int) -> String -> String -> IO ()
        l1 rv old new = setValue rv (+1) >> pure ()
        -- adding a listener
        -- (e',_) = addListener e l1
        -- Result of accumB 0 $ ... 
        i :: ReactiveVar IO Int
        i = newVar 0
        -- listener connecting i to f, accum should apply incoming functions in order
        l2 :: (ReactiveVar IO Int) -> (Int -> Int) -> (Int -> Int) -> IO ()
        l2 rv old new = setValue rv (new (getValue rv)) >> pure () 
        -- adding a listener
        -- (f',_) = addListener f l2
        -- now, "sinking" output listener
        ls :: Int -> Int -> IO ()
        ls old new = putStrLn $ "Counter is: " ++ show new
        -- (i',_) = addListener i ls
        

    putStrLn "Running the network"
    i' <- addListenerM i ls
    f' <- addListenerM f (l2 i')
    e' <- addListenerM e (l1 f')
    -- connecting input events function
    let inp = getLine >> setValue e' "Click" >> inp
    inp
