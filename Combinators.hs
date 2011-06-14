{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances #-}
module Combinators where

import Rx

select :: (a -> b) -> (Subscribe a) -> Observer b -> IO Disposable
select convert subscribe observer = subscribe (observer . convert)

filter :: (a -> Bool) -> (Subscribe a) -> Observer a -> IO Disposable
filter predicate subscribe observer = subscribe filteredObserver
  where filteredObserver a = if (predicate a) then (observer a) else return ()
