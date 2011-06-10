{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

module Rx where

import Data.IORef
import Control.Monad

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

newPushCollection = liftM PushCollection (newIORef [])

push :: PushCollection a -> a -> IO ()
push (PushCollection listRef) item = do
    observers <- readIORef listRef
    mapM_  (applyTo item) observers
  where applyTo = flip ($)

{- "Main" for testing it -}

main :: IO ()
main = do
  pushCollection <- newPushCollection 
  disposable <- subscribe pushCollection putStrLn
  push pushCollection "epic"
  putStrLn "done"

