{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

module Rx where

import Data.IORef

{- Generic interfaces -}

class Observable x a where
	subscribe :: a -> Observer x -> Disposable

type Observer x = (x -> IO ())

type Disposable = IO ()

{- Sample implementation : PushCollection -}

data PushCollection a = PushCollection (IORef [Observer a])

instance Observable a (PushCollection a) where
  subscribe (PushCollection listRef) subscriber = do
    observers <- readIORef listRef
    writeIORef listRef $ subscriber : observers 

push :: PushCollection a -> a -> IO ()
push (PushCollection listRef) item = do
    observers <- readIORef listRef
    mapM_ (\ observer -> observer item) observers

{- "Main" for testing it -}

stringObservable :: IO (PushCollection (String)) 
stringObservable = do
  ioRef <- newIORef []
  return (PushCollection ioRef)

stringObserver :: Observer String
stringObserver x = putStrLn x

main :: IO ()
main = do
  pushCollection <- stringObservable 
  let subscriber = stringObserver 
  disposable <- subscribe pushCollection subscriber
  push pushCollection "epic"
  putStrLn "done"

