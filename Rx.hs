{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances #-}
module Rx where

import Control.Monad

class Observable a observable where
  subscribe :: observable -> Observer a -> IO Disposable 

type Observer a = (a -> IO ())

type Disposable = IO ()

type Subscribe a = (Observer a -> IO Disposable)

instance Observable a (Observer a -> IO Disposable) where
  subscribe func observer = func observer 

instance Observable a ([a]) where
  subscribe list observer = do
    mapM observer list 
    return (return ())
