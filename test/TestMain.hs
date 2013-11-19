-- | main entry point for tests
module Main where

import Test.Framework
import Test.Unit
import Test.Pretty
import Test.Diff
          
-- entry point for the test-suite
main = defaultMain tests

tests = [tests_diff
        ,tests_unit
        --,tests_pretty -- disabled the pretty tests until version 1.0
        ]
