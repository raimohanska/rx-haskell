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
 

skipUntil :: (a -> Bool) -> Observable a -> Observable a
skipUntil condition source = toObservable skipUntil'
  where skipUntil' observer = do doneRef <- newIORef False
                                 subscribe source observer { next = forward doneRef (next observer) }
        forward doneRef next a = do done <- readIORef doneRef
                                    if (done || condition a) 
                                       then when (not done) (writeIORef doneRef True) >> next a
                                       else return()

skipWhile condition source = skipUntil (\a -> not $ condition a) source
takeUntil condition source = Rx.takeWhile (\a -> not $ condition a) source

{- TODO: *Until types should be Observable a -> Observable a -> Observable a -}
