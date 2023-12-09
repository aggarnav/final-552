import GameTests qualified as G
import ParserTests qualified as P
import Test.HUnit

main :: IO ()
main = do
  putStrLn "*** Testing Parser ***"
  P.test_all
  P.qc
  putStrLn "\n*** Testing Game Functionality ***"
  G.test_all
  G.qc
  putStrLn "\n*** Testing Finished ***"