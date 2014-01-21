-- | main entry point for tests
module Main where

import Test.Framework
import Test.Unit
import Test.Pretty
import Test.Diff
import Test.Arbitrary
          
-- entry point for the test-suite
main = defaultMain tests

tests = [tests_diff, test_arbitrary, tests_pretty, tests_unit]
