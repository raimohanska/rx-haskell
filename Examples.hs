import Rx
import PushCollection

putStrLnObserver = onEnd (toObserver putStrLn) (putStrLn "end") 

testPushCollection :: IO ()
testPushCollection = do
  pushCollection <- newPushCollection 
  dispose <- subscribe (observablePushCollection pushCollection) putStrLnObserver 
  dispose2 <- subscribe (observablePushCollection pushCollection) putStrLnObserver
  push pushCollection "Should be printed twice"
  dispose
  push pushCollection "Should be printed once"

testObservableList = do
  putStrLn "Should print 1, 2, 3, end"
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

testConcat = do
  putStrLn "Should print a, b, c"
  subscribe (Rx.concat (observableList ["a", "b"]) (observableList ["c"])) putStrLnObserver

testMerge = do
  putStrLn "Should print a, b, c, 1, 2, 3"
  subscribe (merge alphabets (select show numbers)) putStrLnObserver 

testZip = do
  putStrLn "Should print (a, 1) (b, 2) (c, 3)"
  let zipped = Rx.zip alphabets numbers
  subscribe (select show zipped) putStrLnObserver

testScan = do
  putStrLn "Should print 1, 3, 6"
  subscribe (select show (Rx.scan (+) 0 numbers)) putStrLnObserver

alphabets = observableList ["a", "b", "c"]
numbers = observableList [1, 2, 3]
