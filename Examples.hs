import Rx
import PushCollection

testPushCollection :: IO ()
testPushCollection = do
  pushCollection <- newPushCollection 
  dispose <- subscribe (observablePushCollection pushCollection) putStrLn
  dispose2 <- subscribe (observablePushCollection pushCollection) putStrLn
  push pushCollection "Should be printed twice"
  dispose
  push pushCollection "Should be printed once"
  putStrLn "done"

testObservableList = do
  let list = map show [1, 2, 3]
  dispose <- subscribe (observableList list) putStrLn
  dispose
  putStrLn "done"

testCombinators = subscribe (select show (Rx.filter even $ observableList [1, 2])) putStrLn

testSelectMany = do
  let fiveTimes = observableList . (replicate 5)
  subscribe (selectMany (observableList ["a", "b"]) fiveTimes) putStrLn
