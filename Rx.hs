module Rx where

import Control.Monad

type Observer a = (a -> IO ())

type Disposable = IO ()

type Observable a = (Observer a -> IO Disposable)

observableList :: [a] -> Observable a
observableList list observer = do
    mapM observer list 
    return (return ())

select :: (a -> b) -> Observable a -> Observable b
select convert subscribe observer = subscribe (observer . convert)

filter :: (a -> Bool) -> Observable a -> Observable a
filter predicate subscribe observer = subscribe filteredObserver
  where filteredObserver a = if (predicate a) then (observer a) else return ()

{- Try: select show (Rx.filter even $ observableList [1, 2]) putStrLn -}
