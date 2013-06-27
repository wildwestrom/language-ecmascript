{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | QuickCheck $Arbitrary$ instances for ECMAScript 3 abstract
-- syntax.

module Language.ECMAScript3.Syntax.Arbitrary where

import Language.ECMAScript3.Syntax
import Test.QuickCheck hiding (Prop)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Data.Map hiding (map,null,filter,foldr)
import Data.List (nub,delete)
import Data.Data
import Data.Char
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Str
import Control.Monad
import Control.Monad.State
import Data.Maybe (maybeToList)

-- | The generator state: a stack of labels enclosing the current context
data ArbState = ArbState [String]

type SGen s a = StateT s Gen a

-- | A stateful arbitrary class
class StatefulArbitrary s a where
  sarbitrary :: StateT s Gen a
  sshrink :: a -> State s [a]
  sshrink a = return [a]

toArbitrary :: StatefulArbitrary s a => s -> Gen a
toArbitrary s = evalStateT sarbitrary s

toShrink :: StatefulArbitrary s a => s -> (a -> [a])
toShrink s = \a -> evalState (sshrink a) s

soneof :: [SGen s a] -> SGen s a
soneof [] = error "soneof called with an empty list"
soneof sgens = do i <- lift $ choose (0, length sgens - 1)
                  sgens !! i

instance StatefulArbitrary ArbState AssignOp where
  sarbitrary = 
    lift $ elements
             [OpAssign, OpAssignAdd, OpAssignSub, OpAssignMul, OpAssignDiv, 
              OpAssignMod, OpAssignLShift, OpAssignSpRShift, OpAssignZfRShift,
              OpAssignBAnd, OpAssignBXor, OpAssignBOr]

instance StatefulArbitrary ArbState InfixOp where
  sarbitrary = 
    lift $ elements [OpLT, OpLEq, OpGT, OpGEq , OpIn , OpInstanceof, OpEq, OpNEq, 
                     OpStrictEq, OpStrictNEq, OpLAnd, OpLOr,
                     OpMul, OpDiv, OpMod , OpSub, OpLShift, OpSpRShift,
                     OpZfRShift, OpBAnd, OpBXor, OpBOr, OpAdd]
  
instance StatefulArbitrary ArbState (UnaryAssignOp) where
  sarbitrary = 
    lift $ elements [PrefixInc, PrefixDec, PostfixInc, PostfixDec]
  
instance StatefulArbitrary ArbState PrefixOp where
  sarbitrary = 
    lift $ elements [PrefixLNot, PrefixBNot, PrefixPlus, PrefixMinus, 
                     PrefixTypeof, PrefixVoid, PrefixDelete]


instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (Id a) where
  sarbitrary = liftM2 Id sarbitrary identifier
  sshrink (Id a s) = do ns <- sshrink s
                        na <- sshrink a
                        return [Id na' ns' | ns' <- ns, na' <- na]

instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState [a] where
  sarbitrary = lift arbitrary
  sshrink = return . shrink

instance StatefulArbitrary ArbState Char where
  sarbitrary = lift arbitrary
  sshrink = return . shrink

instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (CaseClause a) where
  sarbitrary = soneof [caseclause, casedefault]
    where caseclause = liftM3 CaseClause sarbitrary sarbitrary sarbitrary
          casedefault = liftM2 CaseDefault sarbitrary sarbitrary
  sshrink (CaseClause a expr stmts) = 
    [CaseClause na ne ns | na <- sshrink a, ne <- sshrink expr, ns <- sshrink stmts]
  sshrink (CaseDefault a stmts) = 
    [CaseDefault na ns | na <- sshrink a, ns <- sshrink stmts]
                         
instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (Prop a) where
  sarbitrary = soneof [liftM2 PropId sarbitrary sarbitrary,
                     liftM2 PropString sarbitrary nonEmptyString,
                     liftM2 PropNum sarbitrary nonNegative
                    ]
  sshrink (PropId a id) = [PropId na nid | nid <- sshrink id, na <- sshrink a] 
  sshrink (PropString a s) = [PropString na ns | ns <- sshrink s, na <- sshrink a] 
  sshrink (PropNum a i) = [PropNum na ni | ni <- sshrink i, na <- sshrink a] 
  
instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (LValue a) where  
  sarbitrary = soneof [liftM2 LVar sarbitrary identifier,
                     liftM3 LDot sarbitrary sarbitrary identifier,
                     liftM3 LBracket sarbitrary sarbitrary sarbitrary]
  sshrink (LVar a s) = [LVar na ns | ns <- sshrink s, na <- sshrink a]
  sshrink (LDot a e s) = [LDot na ne ns | ne <- sshrink e, ns <-shrink s, na <-shrink a]
  sshrink (LBracket a e1 e2) = [LBracket na ne1 ne2 | ne1 <- sshrink e1, ne2 <-shrink e2, na <- sshrink a]
  
csshrink :: StatefulArbitrary ArbState a => [a] -> State s [a]
csshrink = (liftM concat) . sshrink

identifier :: StateT s Gen String
identifier = lift $ sized sizedIdent
    where sizedIdent :: Int -> Gen String
          sizedIdent n = do s <- identStart
                            rest <- identRest (n-1)
                            return (s:rest)
          identStart = arbitrary `suchThat` isIdentStart
          identRest n | n < 1 = return ""
          identRest n = do p <- identPart
                           rest <- identRest (n-1)
                           return (p:rest)
          identPart = do arbitrary `suchThat` isIdentPart
          isIdentStart c = isLetter c || c == '$' || c == '_'
          isIdentPart c = isIdentStart c || isMark c || isNumber c

-- minimum size generator
type MSGen a = (Int, Gen a)

sGen :: [MSGen a] -> Gen a
sGen gens = 
  sized f 
  where f n | n >= 0 = soneof $ map snd (filter (\(m, _) -> n >= m) gens)
        f _          = f 0

recursive :: Gen a -> Gen a
recursive g = sized (\n -> resize (n-1) g)

rarbitrary :: StatefulArbitrary ArbState a => Gen a
rarbitrary = recursive sarbitrary

rrarbitrary :: StatefulArbitrary ArbState a => Gen a
rrarbitrary = recursive $ recursive sarbitrary

atLeastOfSize :: StatefulArbitrary ArbState a => Int -> Gen a -> Gen a
atLeastOfSize l gen = sized $ \s -> if s < l then resize l gen else gen

listOfN :: StatefulArbitrary ArbState a => Int -> Gen a -> Gen [a]
listOfN l gen = sized $ \n ->
  let l' = l `max` 0
  in do k <- choose (l', l' `max` n)
        vectorOf k gen

nonEmptyString :: Gen String
nonEmptyString = sized $ \s -> if s < 1 then stringOfLength 1 else stringOfLength s

regexpBody = nonEmptyString

nonNegative :: (StatefulArbitrary ArbState a, Num a) => Gen a
nonNegative = liftM abs sarbitrary

stringOfLength :: Int -> Gen String
stringOfLength 0 = return ""
stringOfLength n = do c <- sarbitrary
                      rs <- stringOfLength (n-1)
                      return (c:rs)

instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (Expression a) where
  sarbitrary = 
    sGen [(0, liftM  ThisRef sarbitrary),
          (0, liftM  NullLit sarbitrary),
          (0, liftM2 StringLit sarbitrary sarbitrary),
          (0, liftM2 NumLit sarbitrary nonNegative),
          (0, liftM2 IntLit sarbitrary nonNegative),
          (0, liftM2 BoolLit sarbitrary sarbitrary),
          (0, liftM4 RegexpLit sarbitrary regexpBody sarbitrary sarbitrary),
          (1, liftM2 ArrayLit sarbitrary rarbitrary),
          (1, liftM2 ObjectLit sarbitrary rarbitrary),
          (0, liftM2 VarRef sarbitrary sarbitrary),
          (1, liftM3 DotRef sarbitrary rarbitrary sarbitrary),
          (2, liftM3 BracketRef sarbitrary rarbitrary rarbitrary),
          (3, liftM3 NewExpr sarbitrary rarbitrary rrarbitrary),
          (1, liftM3 PrefixExpr sarbitrary sarbitrary rarbitrary),
          (2, liftM3 UnaryAssignExpr sarbitrary sarbitrary rarbitrary),
          (2, liftM4 InfixExpr sarbitrary sarbitrary rarbitrary rarbitrary),
          (3, liftM4 CondExpr sarbitrary rarbitrary rarbitrary rarbitrary),
          (3, liftM4 AssignExpr sarbitrary rarbitrary rarbitrary rarbitrary),
          (3, liftM2 ListExpr sarbitrary (recursive (listOfN 2 sarbitrary))),
          (3, liftM3 CallExpr sarbitrary rarbitrary rrarbitrary),
          (1, liftM4 FuncExpr sarbitrary sarbitrary sarbitrary rarbitrary)]
    
  sshrink (StringLit a s) = [StringLit na ns | na <- sshrink a, ns <- sshrink s]
  sshrink (RegexpLit a s b1 b2) = [RegexpLit na ns nb1 nb2 | na <- sshrink a, nb1 <- sshrink b1, nb2 <- sshrink b2, ns <- sshrink s]
  sshrink (NumLit a d) = [NumLit na nd | na <- sshrink a, nd <- sshrink d]
  sshrink (IntLit a i) = [IntLit na ni | na <- sshrink a, ni <- sshrink i]
  sshrink (BoolLit a b) = [BoolLit na nb | na <- sshrink a, nb <- sshrink b]
  sshrink (NullLit a) = [NullLit na | na <- sshrink a]
  sshrink (ArrayLit a xs) = (csshrink xs) ++ xs ++ [ArrayLit na nxs | na <- sshrink a, nxs <- sshrink xs]
  sshrink (ObjectLit a xs) =  
    let es = map snd xs in
    (csshrink es) ++ es ++
    [ObjectLit na nxs | na <- sshrink a, nxs <- sshrink xs]
  sshrink (ThisRef a) = [ThisRef na | na <- sshrink a]
  sshrink (VarRef a id) = [VarRef na nid | na <- sshrink a, nid <- sshrink id]
  sshrink (DotRef a e id) = [DotRef na ne nid | na <-shrink a, nid <- sshrink id,  ne <- sshrink e]
  sshrink (BracketRef a o k) = [BracketRef na no nk | na <- sshrink a, no <-shrink o, nk <- sshrink k]
  sshrink (NewExpr a c xs) = (shrink c) ++ [c] ++ (csshrink xs) ++ xs ++ [NewExpr na nc nxs | na <- sshrink a, nc <- sshrink c,  nxs <- sshrink xs]
  sshrink (PrefixExpr a op e) = (shrink e) ++ [e] ++ [PrefixExpr na nop ne | na <- sshrink a, nop <-shrink op, ne <- sshrink e]
  sshrink (UnaryAssignExpr a op v) = [UnaryAssignExpr na nop nv | na <- sshrink a, nop <- sshrink op, nv <- sshrink v]
  sshrink (InfixExpr a op e1 e2) = (shrink e1) ++ [e1] ++ (shrink e2) ++ [e2] ++ [InfixExpr na nop ne1 ne2 | na <- sshrink a, nop <- sshrink op, ne1 <- sshrink e1, ne2 <- sshrink e2]
  sshrink (CondExpr a e1 e2 e3) = (shrink e1) ++ [e1] ++ (shrink e2) ++ [e2] ++ (shrink e3) ++ [e3] ++ [CondExpr na ne1 ne2 ne3 | na <- sshrink a, ne1 <- sshrink e1, ne2 <- sshrink e2, ne3 <- sshrink e3]
  sshrink (AssignExpr a op v e) = (shrink e) ++ [e] ++ [AssignExpr na nop nv ne | na <- sshrink a, nop <- sshrink op, nv <- sshrink v, ne <-shrink e] 
  sshrink (ListExpr a es) = (csshrink es) ++ es ++ [ListExpr na nes | na <- sshrink a, nes <- sshrink es]
  sshrink (CallExpr a e es) = (shrink e) ++ [e] ++ (csshrink es) ++ es ++ [CallExpr na ne nes | na <- sshrink a, ne <- sshrink e, nes <- sshrink es]
  sshrink (FuncExpr a mid ids s) = [FuncExpr na nmid nids ns | na <- sshrink a, nmid <-  sshrink mid, nids <- sshrink ids, ns <- sshrink s]

instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (ForInInit a) where
  sarbitrary = soneof [liftM ForInVar sarbitrary,
                     liftM ForInLVal sarbitrary]
  sshrink (ForInVar id) = [ForInVar nid | nid <- sshrink id]
  sshrink (ForInLVal id) = [ForInLVal nid | nid <- sshrink id]
  
instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (ForInit a) where  
  sarbitrary = 
    frequency [
      (2, return NoInit),
      (1, liftM VarInit sarbitrary),
      (1, liftM ExprInit sarbitrary)]
  sshrink (NoInit) = []
  sshrink (VarInit vds) = [VarInit nvds | nvds <- sshrink vds]
  sshrink (ExprInit e) = [ExprInit ne | ne <- sshrink e]

instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (CatchClause a) where
  sarbitrary = liftM3 CatchClause sarbitrary sarbitrary sarbitrary
  sshrink (CatchClause a id s) = [CatchClause na nid ns | na <- sshrink a, nid <- sshrink id, ns <- sshrink s]
  
instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (VarDecl a) where
  sarbitrary = liftM3 VarDecl sarbitrary sarbitrary sarbitrary
  sshrink (VarDecl a id me) = [VarDecl na nid nme | na <- sshrink a, nid <- sshrink id, nme <- sshrink me]

instance StatefulArbitrary ArbState a => StatefulArbitrary ArbState (Statement a) where
  sarbitrary = 
    sGen [(2, liftM2 BlockStmt sarbitrary rrarbitrary),
          (0, liftM  EmptyStmt sarbitrary),
          (1, liftM2 ExprStmt sarbitrary rarbitrary),
          (3, liftM4 IfStmt sarbitrary rarbitrary rarbitrary rarbitrary),
          (2, liftM3 IfSingleStmt sarbitrary rarbitrary rarbitrary),
          (3, liftM3 SwitchStmt sarbitrary rarbitrary rrarbitrary),
          (2, liftM3 WhileStmt sarbitrary rarbitrary rarbitrary),
          (2, liftM3 DoWhileStmt sarbitrary rarbitrary rarbitrary),
          (0, liftM2 BreakStmt sarbitrary sarbitrary),
          (0, liftM2 ContinueStmt sarbitrary sarbitrary),
          (1, liftM3 LabelledStmt sarbitrary sarbitrary rarbitrary),
          (3, liftM4 ForInStmt sarbitrary rarbitrary rarbitrary rarbitrary),
          (4, liftM5 ForStmt sarbitrary rarbitrary rarbitrary rarbitrary rarbitrary),
          (4, arbtry),
          (1, liftM2 ThrowStmt sarbitrary rarbitrary),
          (1, liftM2 ReturnStmt sarbitrary rarbitrary),
          (2, liftM3 WithStmt sarbitrary rarbitrary rarbitrary),
          (2, liftM2 VarDeclStmt sarbitrary (listOf1 rrarbitrary)),
          (1, liftM4 FunctionStmt sarbitrary sarbitrary sarbitrary rarbitrary)]
    where arbtry = 
            do (mCatch, mFinally) <- soneof [liftM2 (,) (return Nothing) (liftM Just rarbitrary),
                                            liftM2 (,) (liftM Just rarbitrary) (return Nothing),
                                            liftM2 (,) (liftM Just rarbitrary) (liftM Just rarbitrary)]
               a <- sarbitrary                      
               body <- rarbitrary
               return $ TryStmt a body mCatch mFinally
    
  sshrink (BlockStmt a body) = emptyStmtShrink a ++ 
                              [BlockStmt as bs | as <- sshrink a, bs <- sshrink body]
  sshrink (EmptyStmt a) = emptyStmtShrink a
  sshrink (ExprStmt a e) = emptyStmtShrink a ++ 
                          [ExprStmt as es | as <- sshrink a, es <- sshrink e]
  sshrink (IfStmt a e th el) = emptyStmtShrink a ++
                              [IfStmt as es ths els | as <- sshrink a, es <- sshrink e, ths <- sshrink th, els <- sshrink el]
  sshrink (IfSingleStmt a e th) = emptyStmtShrink a ++
                                 [IfSingleStmt as es ths | as <- sshrink a, es <- sshrink e, ths <- sshrink th]
  sshrink (SwitchStmt a e cases) = emptyStmtShrink a ++
                                  [SwitchStmt as es cs | as <- sshrink a, es <-shrink e, cs <- sshrink cases] 
  sshrink (WhileStmt a e b) = emptyStmtShrink a ++
                             [WhileStmt as es bs | as <- sshrink a, es <- sshrink e, bs <- sshrink b]
  sshrink (DoWhileStmt a b e) = emptyStmtShrink a ++  
                               [DoWhileStmt as bs es | as <- sshrink a, es <- sshrink e, bs <- sshrink b]
  sshrink (BreakStmt a l) = emptyStmtShrink a ++
                           [BreakStmt as ls | as <- sshrink a, ls <- sshrink l]
  sshrink (ContinueStmt a l) = emptyStmtShrink a ++
                              [ContinueStmt as ls | as <- sshrink a, ls <- sshrink l]
  sshrink (LabelledStmt a l s) = emptyStmtShrink a ++
                                [LabelledStmt as ls ss | as <- sshrink a, ls <- sshrink l, ss <- sshrink s]
  sshrink (ForInStmt a i o s) = emptyStmtShrink a ++
                               [ForInStmt as is os ss | as <- sshrink a, is <-shrink i, os <-shrink o, ss <- sshrink s]
  sshrink (ForStmt a i e1 e2 s) = emptyStmtShrink a ++
                                 [ForStmt as is e1s e2s ss | as <- sshrink a, is <- sshrink i, e1s <- sshrink e1, e2s <- sshrink e2, ss <- sshrink s]
  sshrink (TryStmt a b cs mf) = emptyStmtShrink a ++
                               [TryStmt as bs css mfs | as <- sshrink a, bs <- sshrink b, css <- sshrink cs, mfs <- sshrink mf]
  sshrink (ThrowStmt a e) = emptyStmtShrink a ++
                           [ThrowStmt as es | as <- sshrink a, es <- sshrink e]
  sshrink (ReturnStmt a e) = emptyStmtShrink a ++
                            [ReturnStmt as es | as <- sshrink a, es <- sshrink e]
  sshrink (WithStmt a o s) = emptyStmtShrink a ++
                            [WithStmt as os ss | as <- sshrink a, os <- sshrink o, ss <- sshrink s]
  sshrink (VarDeclStmt a vds) = emptyStmtShrink a ++
                               [VarDeclStmt as vdss | as <- sshrink a, vdss <- sshrink vds]
  sshrink (FunctionStmt a n pars b) = emptyStmtShrink a ++
                                     [FunctionStmt as ns parss bs | as <- sshrink a, ns <- sshrink n, parss <- sshrink pars, bs <- sshrink b]
    
emptyStmtShrink a = [EmptyStmt a2 | a2 <- sshrink a]    

type LabelSubst   = Map (Id ()) (Id ())
emptyConstantPool = Data.Map.empty

instance (Data a, StatefulArbitrary ArbState a) => StatefulArbitrary ArbState (JavaScript a) where
  sarbitrary = do {s <- liftM2 Script sarbitrary sarbitrary;
                  if isProgramFixable s then fixLabels s
                  else sarbitrary}
  sshrink (Script a ss) = [Script na nss | na <- sshrink a, nss <- sshrink ss]
  
-- | Fixes labels so that labeled breaks and continues refer to
-- existing labeled statements, enclosing them; also, reduces the size
-- of the label set. Assumes that the program has a proper syntactic
-- structure, i.e. 'isProgramFixable' s = True.
fixLabels :: (Data a) => JavaScript a -> Gen (JavaScript a)
fixLabels s = 
  fixBreakContinueLabels s >>= removeDuplicateLabels
          
-- | choose n elements from a list randomly
rChooseElem :: [a] -> Int -> Gen [a]
rChooseElem xs n | n > 0 && (not $ null xs) = 
  if n >= length xs then return xs
  else (vectorOf n $ choose (0, n-1)) >>=
       (\subst -> return $ foldr (\n ys -> (xs!!n):ys) [] subst)
rChooseElem _  _ = return [] 

-- | A predicate that tells us whether a program has a fixable/correct
-- label-break/continue structure.  The predicate imposes syntactic
-- restrictions on the break, continue and labeled statements as in
-- the ECMA spec
isProgramFixable :: (Data a ) => JavaScript a -> Bool
isProgramFixable (Script _ stmts) = 
  Prelude.and $ 
  Prelude.map 
             (\stmt -> isBreakContinueFixable stmt False False False) 
             stmts

-- | Imposes relaxed restrictions on break and continue per ECMAScript
-- 5 spec (page 92): any continue without a label should be nested
-- within an iteration stmt, any continue with a label should be
-- nested in a labeled statement (not necessarily with the same
-- label); any break statement without a label should be nested in an
-- iteration or switch stmt, any break statement with a label should
-- be nested in a labeled statement (not necessarily with the same
-- label).
isBreakContinueFixable :: (Data a) => Statement a -> 
                                      Bool -> 
                                      Bool -> 
                                      Bool ->
                                      Bool
isBreakContinueFixable stmt inLabeled inIter inSwitch =
  case stmt of
    ContinueStmt _ Nothing -> inIter
    ContinueStmt _ (Just label) -> inLabeled
    BreakStmt    _ Nothing -> inIter || inSwitch
    BreakStmt    _ (Just label) -> inLabeled
    LabelledStmt _ label _ -> 
      continue stmt True inIter inSwitch
    _ -> if isIterationStmt stmt then
             continue stmt inLabeled True inSwitch
         else if isSwitchStmt stmt then
                  continue stmt inLabeled inIter True 
              else True
  --  _ -> continue stmt inLabeled inIter inSwitch
  where continue stmt inLabeled inIter inSwitch =
          and $ map (\s -> isBreakContinueFixable s inLabeled inIter inSwitch) (children stmt)
                   
-- | Removes duplicate labels from nested labeled statements in order
-- to impose restrictions on labeled statements as per ECMAScript 5
-- spec (page 95): nested labeled statements cannot have duplicating
-- labels.
removeDuplicateLabels :: Data a => JavaScript a -> Gen (JavaScript a)
removeDuplicateLabels (Script x stmts) =
    return $ Script x (map (\stmt -> (evalState (transformM removeDL stmt) [])) stmts)
    where
      removeDL :: Statement a -> State [String] (Statement a)
      removeDL stmt@(LabelledStmt x lab s) = 
          do {enclosingLabels <- get;
              if Prelude.elem (unId lab) enclosingLabels then return s
              else modify ((:) $ unId lab) >> return stmt}
      removeDL s = return s        
      
-- | Selects a random element of the list
selectRandomElement :: [a] -> Gen a
selectRandomElement xs = 
  let l = length xs in
  do n <- sarbitrary
     return $ xs !! (n `mod` l - 1)
-- | Changes labels of break/continue so that they refer to one of the
-- enclosing labels
fixBreakContinueLabels :: Data a => JavaScript a -> Gen (JavaScript a)
fixBreakContinueLabels (Script x stmts) =
  do stmts2 <- mapM (\stmt -> (evalStateT (fixBCL stmt) [])) stmts
     return $ Script x stmts2
    where
      fixBCL :: Data a => Statement a -> StateT [String] Gen (Statement a)
      fixBCL stmt@(LabelledStmt _ lab s) =
        do modify ((:) $ unId lab)
           descendM fixBCL stmt
      fixBCL stmt@(BreakStmt x (Just (Id y lab))) =
          do {labels <- get;
              if lab `notElem` labels then
                  do {newLab <- lift $ selectRandomElement labels;
                      return $ BreakStmt x (Just $ Id y newLab)}
              else return stmt}
      fixBCL stmt@(ContinueStmt x (Just (Id y lab))) =
          do {labels <- get;
              if lab `notElem` labels then
                  do {newLab <- lift $ selectRandomElement labels;
                      return $ ContinueStmt x (Just $ Id y newLab)}
              else return stmt}
      fixBCL s = return s

isSwitchStmt :: Statement a    -> Bool
isSwitchStmt (SwitchStmt _ _ _) = True
isSwitchStmt _                  = False
