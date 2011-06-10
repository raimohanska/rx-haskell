{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

module Rx where

import Data.IORef

{- Generic interfaces -}

type SubscribeResult a = IO a

class Observable x a where
	subscribe :: a -> Subscriber x -> Disposable

type Subscriber x = (x -> IO ())

type Disposable = SubscribeResult ()

{- Sample implementation -}

data PushCollection a = PushCollection (IORef [Subscriber a])

instance Observable a (PushCollection a) where
  subscribe (PushCollection listRef) subscriber = undefined

stringObservable :: IO (PushCollection (String)) 
stringObservable = do
  ioRef <- newIORef []
  return (PushCollection ioRef)

stringSubscriber :: Subscriber String
stringSubscriber x = putStrLn x

main :: IO ()
main = do
  pushCollection <- stringObservable 
  let subscriber = stringSubscriber 
  disposable <- subscribe pushCollection subscriber
  putStrLn "done"

