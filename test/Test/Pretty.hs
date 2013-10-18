{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pretty where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary()
import Language.ECMAScript3.Syntax.Annotations
--import System.Exit
import Language.ECMAScript3.SourceDiff
import Test.QuickCheck
import Data.List
import Data.Generics.Uniplate.Data
import Control.Applicative
import Data.Default.Class
import Data.Data (Data)

tests_pretty :: Test
tests_pretty = testGroup "Pretty-printer tests"
               [testProperty "Parse is the inverse of pretty" prettyParseEquivalence
               ,testProperty "Expressions not safe to print in an Expression Statement" unsafeExprStmtProp]

-- main :: IO ()
-- main = 
--   let qcArgs = Args {maxSuccess = 50
--                     ,maxDiscardRatio = 10
--                     ,maxSize = 10
--                     ,replay = Nothing
--                     ,chatty = False}
--   in quickCheckWithResult qcArgs prettyParseEquivalence >>= \res ->
--   case res of
--     Success {} -> putStrLn "All tests passes"
--     GaveUp {} -> putStrLn "Gave up"
--     Failure {} -> putStrLn "Test failed" >> exitFailure
--     NoExpectedFailure {} -> putStrLn "Unexpected failure" >> exitFailure

prettyParseEquivalence :: JavaScript () -> Property
prettyParseEquivalence orig =
  let aor = adaptAST orig
      pp = show $ prettyPrint aor
  in case parseFromString pp of
    Left e -> 
      let err = "Can't parse pretty-printed code. The error was: " ++ (show e) ++
                "\nThe pretty-printed code in question:\n" ++ pp
      in whenFail (putStrLn err) False
    Right parsed ->
      let eq = (removeAnnotations parsed) == aor
          msg ="The parse of the pretty-printed AST didn't match the original\n"
               ++"Diff:\n" ++ jsDiff aor (reannotate (const ()) parsed)
      in whenFail (putStrLn msg) eq

unsafeExprStmtProp :: Expression () -> Bool
unsafeExprStmtProp e =
  let se = show $ prettyPrint e
      actuallyUnsafe = "{" `isPrefixOf` se || "function" `isPrefixOf` se
      oracleUnsafe = unsafeInExprStmt e
  in  actuallyUnsafe == oracleUnsafe

-- | Adapt the AST to account for (non-critical) discrepancies between
-- the parser and the pretty-printer.
adaptAST :: JavaScript () -> JavaScript ()
adaptAST = adaptTryBlock . flattenListExpr

adaptTryBlock :: JavaScript () -> JavaScript ()
adaptTryBlock = transformBi adaptTryBlock_
   where adaptTryBlock_ s = case s of
           TryStmt a tb mc mf -> TryStmt a (blockerize tb) (blockerizeCC <$> mc)
                                           (blockerize <$> mf)
           _ -> s
         blockerize s = case s of
           BlockStmt _ _ -> s
           _             -> BlockStmt () [s]
         blockerizeCC (CatchClause a id s) = CatchClause a id (blockerize s)

flattenListExpr :: JavaScript () -> JavaScript ()
flattenListExpr = transformBi flattenListExpr_
  where flattenListExpr_ :: Expression () -> Expression ()
        flattenListExpr_ e = case e of
          ListExpr a es -> ListExpr a (concatMap gatherExprs es)
          _ -> e
        gatherExprs e = case e of
          ListExpr a es -> es
          _ -> [e]
