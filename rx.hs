{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

module Rx where

import Data.IORef
import Control.Monad

{- Generic interfaces -}

class Observable x a where
	subscribe :: a -> Observer x -> IO Disposable

type Observer x = (x -> IO ())

type Disposable = IO ()

{- Sample implementation : PushCollection -}

data Subscription a = Subscription (Observer a) Int
instance Eq (Subscription q) where
  (==) (Subscription _ a) (Subscription _ b) = a == b 

data PushCollection a = PushCollection (IORef ([Subscription a], Int))

instance Observable a (PushCollection a) where
  subscribe (PushCollection ref) observer = do
    (observers, id) <- readIORef ref
    let subscription = Subscription observer id
    writeIORef ref $ (subscription : observers, id+1) 
    return (removeFromListRef ref subscription)

removeFromListRef ref subscriber = do
    (observers, id) <- readIORef ref
    writeIORef ref $ (filter (/= subscriber) observers, id)
  
newPushCollection = liftM PushCollection (newIORef ([], 1))

push :: PushCollection a -> a -> IO ()
push (PushCollection listRef) item = do
    (observers, _) <- readIORef listRef
    mapM_  (applyTo item) observers
  where applyTo item (Subscription observer _) = observer item

{- "Main" for testing it -}

main :: IO ()
main = do
  pushCollection <- newPushCollection 
  dispose <- subscribe pushCollection putStrLn
  dispose2 <- subscribe pushCollection putStrLn
  push pushCollection "Should be printed twice"
  dispose
  push pushCollection "Should be printed once"
  putStrLn "done"


