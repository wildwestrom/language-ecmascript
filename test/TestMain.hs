-- | main entry point for tests
module Main where

import Test.Framework
import Test.Unit
import Test.Pretty
import Test.Diff
import Test.Arbitrary
          
-- entry point for the test-suite
main = defaultMain tests

tests = [tests_diff
        ,tests_unit
        ,test_arbitrary
        --,tests_pretty -- disabled the pretty tests until version 1.0
        ]
