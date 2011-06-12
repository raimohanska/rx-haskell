{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ObservableList where

import Rx
import Control.Monad

instance Observable a ([a]) where
  subscribe list observer = do
    mapM observer list 
    return (return ())
