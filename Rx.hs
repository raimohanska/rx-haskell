{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances #-}
module Rx where

class Observable x a where
	subscribe :: a -> Observer x -> IO Disposable

type Observer x = (x -> IO ())

type Disposable = IO ()

type Subscribe a = (Observer a -> IO Disposable)

instance Observable x (Subscribe x) where
  subscribe func observer = func observer 
