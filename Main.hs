import Rx
import PushCollection
import ObservableList

testPushCollection :: IO ()
testPushCollection = do
  pushCollection <- newPushCollection 
  dispose <- subscribe pushCollection putStrLn
  dispose2 <- subscribe pushCollection putStrLn
  push pushCollection "Should be printed twice"
  dispose
  push pushCollection "Should be printed once"
  putStrLn "done"

testObservableList = do
  let list = map show [1, 2, 3]
  dispose <- subscribe list putStrLn
  dispose
  putStrLn "done"
