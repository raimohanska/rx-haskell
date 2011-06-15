import Rx
import PushCollection

putStrLnObserver = toObserver putStrLn

testPushCollection :: IO ()
testPushCollection = do
  pushCollection <- newPushCollection 
  dispose <- subscribe (observablePushCollection pushCollection) putStrLnObserver 
  dispose2 <- subscribe (observablePushCollection pushCollection) putStrLnObserver
  push pushCollection "Should be printed twice"
  dispose
  push pushCollection "Should be printed once"

testObservableList = do
  putStrLn "Should print 1, 2, 3"
  let list = map show [1, 2, 3]
  dispose <- subscribe (observableList list) putStrLnObserver
  dispose

testCombinators = do
  putStrLn "Should print number 2"
  subscribe (select show (Rx.filter even $ observableList [1, 2])) putStrLnObserver

testSelectMany = do
  putStrLn "Should print five a's and five b's"
  let fiveTimes = observableList . (replicate 5)
  subscribe (selectMany (observableList ["a", "b"]) fiveTimes) putStrLnObserver
