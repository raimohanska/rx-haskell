import Rx
import PushCollection

main :: IO ()
main = do
  pushCollection <- newPushCollection 
  dispose <- subscribe pushCollection putStrLn
  dispose2 <- subscribe pushCollection putStrLn
  push pushCollection "Should be printed twice"
  dispose
  push pushCollection "Should be printed once"
  putStrLn "done"


