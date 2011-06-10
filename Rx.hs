{-# LANGUAGE MultiParamTypeClasses #-}

module Rx where

import Control.Monad

class Observable x a where
	subscribe :: a -> Observer x -> IO Disposable

type Observer x = (x -> IO ())

type Disposable = IO ()


