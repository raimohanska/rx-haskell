{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Rx where

import Data.IORef

{- Generic interfaces -}

type SubscribeResult a = IO a

class Observable x a where
	subscribe :: Disposable d => a -> Subscriber x -> SubscribeResult d

type Subscriber x = (x -> IO ())

class Disposable a where
	dispose :: a -> SubscribeResult ()

{- Sample implementation -}

data PushCollection a = PushCollection (IORef [a])

instance Observable a (PushCollection (Subscriber a)) where
  subscribe (PushCollection listRef) subscriber = undefined
{-
main = do
  listRef <- newIORef []
  let pushCollection = PushCollection listRef
  let subscriber = (\ (x :: String) -> putStrLn x)
  subscribe pushCollection subscriber
-}
