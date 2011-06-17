module Rx where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.IORef

data Observable a = Observable {subscribe :: Subscribe a}

data Observer a = Observer { next :: (a -> IO ()), end :: IO(), error :: String -> IO() }

type Subscribe a = (Observer a -> IO Disposable)

type Disposable = IO ()

class Source s where
  getObservable :: s a -> Observable a

instance Functor Observable where
  fmap = select

instance Monad Observable where
  return a = observableList [a]
  (>>=) = selectMany

instance MonadPlus Observable where
  mzero = observableList []
  mplus = merge 

toObservable :: Subscribe a -> Observable a
toObservable subscribe = Observable subscribe

toObserver :: (a -> IO()) -> Observer a
toObserver next = Observer next (return ()) fail

observableList :: [a] -> Observable a
observableList list = toObservable subscribe 
  where subscribe observer = do mapM (next observer) list
                                end observer
                                return (return ())

select :: (a -> b) -> Observable a -> Observable b
select convert source = do a <- source
                           return $ convert a

filter :: (a -> Bool) -> Observable a -> Observable a
filter predicate source = do 
  a <- source
  if (predicate a) then return a else mzero

selectMany :: Observable a -> (a -> Observable b) -> Observable b
selectMany source spawner = toObservable ((subscribe source) . spawningObserver)
  where spawningObserver observer = observer { next = spawnSingle observer }
        spawnSingle observer a = subscribe (spawner a) observer { end = return() } >> return ()
        {- TODO: dispose will never be called on the spawned Observables -}
concat :: Observable a -> Observable a -> Observable a
concat a' b' = toObservable concat'
  where concat' observer = do disposeRef <- newIORef (return ())
                              disposeFunc <- subscribe a' observer { end = switchToB disposeRef observer}
                              {- TODO: what if subscribe call leads to immediate call to end. now the following line will override dispose-b with dispose-a -}
                              writeIORef disposeRef disposeFunc
                              return $ (join . readIORef) disposeRef 
        switchToB disposeRef observer = subscribe b' observer >>= (writeIORef disposeRef)
 
merge :: Observable a -> Observable a -> Observable a
merge left right = toObservable merge'
  where merge' observer = do endLeft <- newIORef (False)
                             endRight <- newIORef (False)
                             disposeLeft <- subscribe left observer { end = barrier endLeft endRight (end observer)}
                             disposeRight <- subscribe right observer { end = barrier endRight endLeft (end observer)}
                             return (disposeLeft >> disposeRight)
        barrier myFlag otherFlag done = do writeIORef myFlag True
                                           otherDone <- readIORef otherFlag
                                           when otherDone done

takeWhile :: (a -> Bool) -> Observable a -> Observable a
takeWhile condition source = toObservable takeWhile'
  where takeWhile' observer = do disposeRef <- newIORef (return ())
                                 disposeFunc <- subscribe source observer { next = forward disposeRef (next observer) }
                                 {- TODO: what if subscribe call leads to immediate call to end. now the following line will override dispose-b with dispose-a -}
                                 writeIORef disposeRef disposeFunc
                                 return disposeFunc
        forward disposeRef next a = if (condition a)
                            then next a
                            else do disposeFunc <- readIORef disposeRef
                                    disposeFunc
 

skipWhile :: (a -> Bool) -> Observable a -> Observable a
skipWhile condition source = toObservable skipWhile'
  where skipWhile' observer = do doneRef <- newIORef False
                                 subscribe source observer { next = forward doneRef (next observer) }
        forward doneRef next a = do done <- readIORef doneRef
                                    if (done || not (condition a)) 
                                       then when (not done) (writeIORef doneRef True) >> next a
                                       else return()

data Valve a = Valve (Observable a) (TVar Bool)

valve :: Observable a -> Bool -> STM (Valve a)
valve observable open = newTVar open >>= return . Valve observable

openValve :: Valve a -> STM ()
openValve = setValveState True 

closeValve :: Valve a -> STM()
closeValve = setValveState False

setValveState :: Bool -> Valve a -> STM ()
setValveState newState (Valve _ state) = writeTVar state newState

instance Source Valve where
  getObservable (Valve (Observable subscribe) state) = toObservable subscribe'
    where subscribe' = subscribe . valvedObserver state

valved :: TVar Bool -> Observable a -> Observable a
valved state observable = getObservable $ Valve observable state

valvedObserver :: TVar Bool -> Observer a -> Observer a
valvedObserver state (Observer next end error) = Observer (valved1 next) (valved end) (valved1 error)
  where valved action = atomically (readTVar state) >>= \open -> when open action
        valved1 action input = atomically (readTVar state) >>= \open -> when open (action input)

{- TODO: *Until types should be Observable a -> Observable a -> Observable a -}
{- TODO: Use Control.Concurrent.STM -}
