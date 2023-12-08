import GameTests qualified as G
import ParserTests qualified as P
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "*** Testing Parser ***"
  P.test_all
  putStrLn "\n*** Testing Game Functionality ***"
  G.test_all
  G.qc
  putStrLn "\n*** Testing Finished ***"
