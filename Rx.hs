{-# LANGUAGE MultiParamTypeClasses #-}

module Rx where

class Observable x a where
	subscribe :: a -> Observer x -> IO Disposable

type Observer x = (x -> IO ())

type Disposable = IO ()


