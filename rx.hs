{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Rx where

import Data.Array.IO

{- Generic interfaces -}

type SubscribeResult a = IO a

class Observable a x where
	subscribe :: Disposable d => a -> Subscriber x -> SubscribeResult d

type Subscriber x = (x -> IO ())

class Disposable a where
	dispose :: a -> SubscribeResult ()

{- Sample implementation -}

data PushCollection a = PushCollection (IOArray Int a)

instance Observable a (PushCollection a) where
  subscribe coll subscriber = undefined

