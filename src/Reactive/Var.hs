{-# LANGUAGE OverloadedStrings, ExistentialQuantification, NoMonomorphismRestriction  #-}
module Reactive.Var where

-- reactive data type: value and functions with oldvalue, newvalue and a monad
data ReactiveVar m a = Reactive a [a -> a -> m ()]

instance Show a => Show (ReactiveVar m a) where
  show (Reactive v fs) = show v ++ " " ++ show (length fs) ++ " observers"

observe :: ReactiveVar m a -> (a -> a -> m ()) -> ReactiveVar m a
observe (Reactive x fs) f = Reactive x (f:fs)

new :: a -> ReactiveVar m a
new x = Reactive x []

readR :: ReactiveVar m a -> a
readR (Reactive a _) = a

update :: Monad m => ReactiveVar m a -> a -> m (ReactiveVar m a)
update (Reactive x fs) y = do
  sqs fs x y  -- mapping function application to call all registered functions
  return $ Reactive y fs -- returning an updated var in the monad

-- ($=) = update

-- there must be a better way to do it via Applicative or something?!
-- applies all functions in order to the same arguments
sqs :: Monad m => [a -> b -> m c] -> a -> b -> m ()
sqs [] x y = return ()
sqs (f:fs) x y = f x y >> sqs fs x y

f1 x y = putStrLn $ "Value " ++ show x ++ " changed to " ++ show y
r = observe (new 4) f1
