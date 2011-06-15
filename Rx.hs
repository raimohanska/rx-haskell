module Rx where

import Control.Monad

data Observable a = Observable {subscribe :: Subscribe a}

data Observer a = Observer { next :: (a -> IO ()), end :: IO(), error :: String -> IO() }

type Subscribe a = (Observer a -> IO Disposable)

type Disposable = IO ()

instance Functor Observable where
  fmap = select

instance Monad Observable where
  return a = observableList [a]
  (>>=) = selectMany

toObservable :: Subscribe a -> Observable a
toObservable subscribe = Observable subscribe

toObserver :: (a -> IO()) -> Observer a
toObserver next = Observer next (return ()) fail

observableList :: [a] -> Observable a
observableList list = toObservable subscribe 
  where subscribe observer = mapM (next observer) list >> end observer >> return (return ())

select :: (a -> b) -> Observable a -> Observable b
select convert (Observable subscribe) = toObservable subscribe'
  where subscribe' observer = subscribe observer { next = (next observer . convert)}

filter :: (a -> Bool) -> Observable a -> Observable a
filter predicate (Observable subscribe) = toObservable subscribe'
  where subscribe' observer = subscribe observer { next = (filtered $ next observer) }
        filtered nextFunc a = if (predicate a) then (nextFunc a) else return ()

selectMany :: Observable a -> (a -> Observable b) -> Observable b
selectMany source spawner = toObservable ((subscribe source) . spawningObserver)
  where spawningObserver observer = observer { next = spawnSingle observer }
        spawnSingle observer a = subscribe (spawner a) observer { end = return() } >> return ()
        
                                          
