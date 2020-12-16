{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, 
    RecordWildCards, OverloadedLists, PostfixOperators, 
    TypeSynonymInstances, FlexibleInstances, ExistentialQuantification,
    MultiParamTypeClasses #-}
module UI.Femto.Middle.Components 
(
    Component(..),
    createComponent,
    renderComponent,
    updateProps
)
where

import Control.Monad.Trans.State.Strict hiding (state)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception ( try )
import Data.Text
import qualified Data.Map.Strict as Map
import Data.IORef

import Color
import PreludeFixes

import UI.Femto.Middle.Widgets
import UI.Femto.Middle.Events


-- ok, making a good pure event system with different STATE types seems nothing but impossible at this point.
-- So we are reverting to the "Stateful Signals" approach from the FRP version, where we store status right inside components.

type Id = Int

-- new value, monad
type Listener m s = s -> m ()

data PureComponent m s = PureComponent {
    value :: !s,
    listeners :: Map.Map Id (Listener m s),
    curId :: !Id
}

newPureComponent :: s -> PureComponent m s
newPureComponent s = PureComponent {value = s, listeners = Map.empty, curId = 0}

-- stateful component in an IO monade (will use refs to manage state)
data Component m s p = Component { 
        -- these functions are created from createComponent - they rely on IORefs to keep the state
        readState :: m s, -- read value
        modifyState :: (s -> s) -> m (), -- modify value
        addListenerOnly :: Listener m s -> m (), -- add listener
        addListener :: Listener m s -> m (m ()), -- add listener and return remove listener action
        -- these are the pure stuff
        props :: p, -- similar to react
        pureRender :: s -> p -> Widget,-- "rendering" function that converts our state and props to a Widget, inaccessible to end users, they'll have a proper render in the monad
        debugRender :: s -> p -> String
        
    }

data SealedComponent m = forall s p. SealedComponent (Component m s p)
renderSealed :: MonadIO m =>  SealedComponent m -> m (Widget)
renderSealed (SealedComponent c) = renderComponent c

fire :: MonadIO m => Component m s p -> s -> m ()
fire sig val = modifyState sig (const val)

-- Interface function to render a Component to widget using current props and state
renderComponent :: MonadIO m => Component m s p -> m (Widget)
renderComponent Component{..} = do
    s <- readState
    pure $ pureRender s props

-- Interface function to update props in a Component
updateProps :: p -> Component m s p -> Component m s p
updateProps p c = c { props = p }

-- "Constructor" for creating Compponents - will be VISIBLE to users
createComponent :: MonadIO m => s -> p -> (s -> p -> Widget) -> m (Component m s p)
createComponent initVal initProps renderFunc = do
    -- liftIO $ putStrLn "Creating stateful signal"
    cache <- liftIO $ newIORef (newPureComponent initVal)
    let read = value <$> liftIO (readIORef cache)
    -- can we make at least read operation pure???
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
    pure $ Component {
              readState = read
            , modifyState = mod
            , addListenerOnly = addL
            , addListener = addLR
            , props = initProps
            , pureRender = renderFunc 
           }

addListenerPure :: Listener m a -> PureComponent m a -> PureComponent m a
addListenerPure l r = 
    let ls  = listeners r
        ci  = curId r + 1
        ls' = Map.insert ci l ls
    in  r { listeners = ls', curId = ci }

removeListenerPure :: Id -> PureComponent m a -> PureComponent m a    
removeListenerPure i r = 
    let ls  = listeners r
        ls' = Map.delete i ls
    in  r { listeners = ls' }


-- Some tests -------------------------------------------------------------------------------------------
-- following react tutorial with ttt board:
mkBoard :: MonadIO m => m (Component m [Int] ())
mkBoard = createComponent [0,0,0] () (\s p -> emptyWidget)
-- depending on which square is clicked we are changing the state correspondingly
handleClick :: MonadIO m => Int -> Component m [Int] () -> m ()
handleClick 0 brd = fire brd [1,0,0]
handleClick 1 brd = fire brd [0,1,0]
handleClick 2 brd = fire brd [0,0,1]

mkSquare :: MonadIO m => Int -> m (Component m () Int)
mkSquare i = createComponent () i (\s p -> emptyWidget)

-- now we just need to somehow connect event handling inside mkSquare and handleClick with the right board - what interface could we have?