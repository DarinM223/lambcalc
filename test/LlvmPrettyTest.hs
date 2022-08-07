module LlvmPrettyTest (tests) where

import Lambcalc.Llvm (Ty (..))
import Lambcalc.Shared (Pretty (pretty))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "LLVM pretty tests"
  [ testCase "Test pretty struct" $
    pretty (Struct [I64, Ptr I8, Struct [Void]]) @?= "{ i64, i8*, { void } }"
  ]