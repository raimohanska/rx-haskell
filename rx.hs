{-# LANGUAGE MultiParamTypeClasses #-}

module Rx where

type SubscribeResult a = IO a

class Observable a x where
	subscribe :: Disposable d => a -> Subscriber x -> SubscribeResult d

type Subscriber x = (x -> IO ())

class Disposable a where
	dispose :: a -> SubscribeResult ()
	
