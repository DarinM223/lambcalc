import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified LlvmPrettyTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ LlvmPrettyTest.tests
  ]