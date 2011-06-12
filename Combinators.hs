{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances #-}
module Combinators where

import Rx

select :: Observable a a' => (a -> b) -> a' -> Subscribe b 
select func observable = (\ observerB -> subscribe observable (convert observerB))
  where convert observerB = observerB . func

filter :: Observable a a' => (a -> Bool) -> a' -> Subscribe a
filter predicate observable = (\ observer -> subscribe observable (filterObserver observer))
  where filterObserver observer a | predicate a = observer a
                                  | otherwise = return ()
