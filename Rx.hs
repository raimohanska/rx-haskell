{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances #-}
module Rx where

import Control.Monad

class Observable a observable where
	subscribe :: observable -> Subscribe a

type Observer a = (a -> IO ())

type Disposable = IO ()

type Subscribe a = (Observer a -> IO Disposable)

instance Observable a (Subscribe a) where
  subscribe func observer = func observer 

instance Observable a ([a]) where
  subscribe list observer = do
    mapM observer list 
    return (return ())

data Delegator a = ToObservable (Subscribe a)

instance Observable a (Delegator a) where
  subscribe (ToObservable sub) observer = sub observer
