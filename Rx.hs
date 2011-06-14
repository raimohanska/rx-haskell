module Rx where

import Control.Monad

type Observer a = (a -> IO ())

instance Functor Observable where
  fmap = select

instance Monad Observable where
  return a = observableList [a]
  (>>=) = selectMany
  
type Disposable = IO ()

data Observable a = Observable {subscribe :: Subscribe a}

type Subscribe a = (Observer a -> IO Disposable)

toObservable :: Subscribe a -> Observable a
toObservable subscribe = Observable subscribe

observableList :: [a] -> Observable a
observableList list = toObservable subscribe 
  where subscribe observer = mapM observer list >> return (return ())

select :: (a -> b) -> Observable a -> Observable b
select convert (Observable subscribe) = toObservable subscribe'
  where subscribe' observer = subscribe (observer . convert)

filter :: (a -> Bool) -> Observable a -> Observable a
filter predicate (Observable subscribe) = toObservable subscribe'
  where subscribe' observer = subscribe (filtered observer)
        filtered observer a = if (predicate a) then (observer a) else return ()

selectMany :: Observable a -> (a -> Observable b) -> Observable b
selectMany source spawner = toObservable ((subscribe source) . spawn)
  where spawn observer a = subscribe (spawner a) observer >> return ()
                                          
