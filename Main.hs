import Rx
import PushCollection

testPushCollection :: IO ()
testPushCollection = do
  pushCollection <- newPushCollection 
  dispose <- observablePushCollection pushCollection putStrLn
  dispose2 <- observablePushCollection pushCollection putStrLn
  push pushCollection "Should be printed twice"
  dispose
  push pushCollection "Should be printed once"
  putStrLn "done"

testObservableList = do
  let list = map show [1, 2, 3]
  dispose <- observableList list putStrLn
  dispose
  putStrLn "done"

testCombinators = select show (Rx.filter even $ observableList [1, 2]) putStrLn
