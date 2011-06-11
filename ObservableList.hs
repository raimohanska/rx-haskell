{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ObservableList where

import Rx

instance Observable a ([a]) where
  subscribe list observer = undefined 
