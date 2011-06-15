module Rx where

import Control.Monad
import Data.IORef

data Observable a = Observable {subscribe :: Subscribe a}

data Observer a = Observer { next :: (a -> IO ()), end :: IO(), error :: String -> IO() }

type Subscribe a = (Observer a -> IO Disposable)

type Disposable = IO ()

instance Functor Observable where
  fmap = select

instance Monad Observable where
  return a = observableList [a]
  (>>=) = selectMany

instance MonadPlus Observable where
  mzero = observableList []
  mplus = Rx.concat

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
       
concat :: Observable a -> Observable a -> Observable a
concat a' b' = toObservable concat'
  where concat' observer = do disposeRef <- newIORef (return ())
                              disposeFunc <- subscribe a' observer { end = switch disposeRef observer}
                              writeIORef disposeRef disposeFunc
                              return $ callRef disposeRef 
        callRef ref = do val <- readIORef ref
                         val
        switch disposeRef observer = subscribe b' observer >>= (writeIORef disposeRef)
 
takeWhile :: (a -> Bool) -> Observable a -> Observable a
takeWhile condition source = undefined
