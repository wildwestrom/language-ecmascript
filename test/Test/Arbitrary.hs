-- | A test for the Arbitrary instances for AST's. Checks that the
-- instances generated are always valid per the 'isValid' predicate in
-- 'Syntax'.
module Test.Arbitrary where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary

test_arbitrary :: Test
test_arbitrary = testProperty "Arbitrary generates valid ASTs"
                 arbitraryGeneratesValidASTs

arbitraryGeneratesValidASTs :: JavaScript () -> Bool
arbitraryGeneratesValidASTs = isValid
