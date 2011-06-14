{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,IncoherentInstances #-}
module Rx where

import Control.Monad

type Observer a = (a -> IO ())

type Disposable = IO ()

type Subscribe a = (Observer a -> IO Disposable)

observableList :: [a] -> Subscribe a
observableList list observer = do
    mapM observer list 
    return (return ())

{- Try: select show (Combinators.filter even $ observableList [1, 2]) putStrLn -}
