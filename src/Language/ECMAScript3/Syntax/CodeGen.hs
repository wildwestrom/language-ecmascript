-- | Utility combinator functions for simplifying writing programmatic
-- generation of ECMAScript code
module Language.ECMAScript3.Syntax.CodeGen where

import Language.ECMAScript3.Syntax
import Data.Default.Class

script :: Default a => [Statement a] -> JavaScript a
script = Script def

id_ :: Default a => String -> Id a
id_ = Id def

propId :: Default a => Id a -> Prop a
propId = PropId def

propId_ :: Default a => String -> Prop a
propId_ = PropId def . id_

propS :: Default a => String -> Prop a
propS = PropString def

propN :: Default a => Integer -> Prop a
propN = PropNum def

lvar :: Default a => String -> LValue a
lvar   = LVar def

ldot :: Default a => Expression a -> String -> LValue a
ldot = LDot def

lbrack :: Default a => Expression a -> Expression a -> LValue a
lbrack = LBracket def

string :: Default a => String -> Expression a
string = StringLit def

regexp :: Default a => String -> Bool -> Bool -> Expression a
regexp = RegexpLit def

number :: Default a => Double -> Expression a
number = NumLit def

int :: Default a => Int -> Expression a
int    = IntLit def

null_ :: Default a => Expression a
null_   = NullLit def

array :: Default a => [Expression a] -> Expression a
array = ArrayLit def

object :: Default a => [(Prop a, Expression a)] -> Expression a
object = ObjectLit def

this :: Default a => Expression a
this = ThisRef def

var :: Default a => Id a -> Expression a
var = VarRef def

var_ :: Default a => String -> Expression a
var_ = VarRef def . id_

dot :: Default a => Expression a -> Id a -> Expression a
dot = DotRef def

dot_ :: Default a => Expression a -> String -> Expression a
dot_ e = DotRef def e . id_

brack :: Default a => Expression a -> Expression a -> Expression a
brack = BracketRef def

new :: Default a => Expression a -> [Expression a] -> Expression a
new = NewExpr def

prefix :: Default a => PrefixOp -> Expression a -> Expression a
prefix = PrefixExpr def

uassign :: Default a => UnaryAssignOp -> LValue a -> Expression a
uassign = UnaryAssignExpr def

infix_
  :: Default a =>
     InfixOp -> Expression a -> Expression a -> Expression a
infix_ = InfixExpr def

cond
  :: Default a =>
     Expression a -> Expression a -> Expression a -> Expression a
cond = CondExpr def

assign
  :: Default a =>
     AssignOp -> LValue a -> Expression a -> Expression a
assign = AssignExpr def

list :: Default a => [Expression a] -> Expression a
list = ListExpr def

call :: Default a => Expression a -> [Expression a] -> Expression a
call = CallExpr def

func
  :: Default a => Id a -> [Id a] -> [Statement a] -> Expression a
func id = FuncExpr def (Just id)

func_
  :: Default a => String -> [Id a] -> [Statement a] -> Expression a
func_ s = func (id_ s)

lambda :: Default a => [Id a] -> [Statement a] -> Expression a
lambda = FuncExpr def Nothing

case_ :: Default a => Expression a -> [Statement a] -> CaseClause a
case_ = CaseClause def

default_ :: Default a => [Statement a] -> CaseClause a
default_ = CaseDefault def

catch :: Default a => Id a -> Statement a -> CatchClause a
catch = CatchClause def

catch_ :: Default a => String -> Statement a -> CatchClause a
catch_ s = CatchClause def (id_ s)

vardecl :: Default a => Id a -> VarDecl a
vardecl id = VarDecl def id Nothing

vardecl_ :: Default a => String -> VarDecl a
vardecl_ = vardecl . id_

varinit :: Default a => Id a -> Expression a -> VarDecl a
varinit id = VarDecl def id . Just

varinit_ :: Default a => String -> Expression a -> VarDecl a
varinit_ s = varinit (id_ s)

block :: Default a => [Statement a] -> Statement a
block = BlockStmt def

empty :: Default a => Statement a
empty = EmptyStmt def

expr :: Default a => Expression a -> Statement a
expr = ExprStmt def

ifte
  :: Default a =>
     Expression a -> Statement a -> Statement a -> Statement a
ifte = IfStmt def

ift :: Default a => Expression a -> Statement a -> Statement a
ift  = IfSingleStmt def

switch
  :: Default a => Expression a -> [CaseClause a] -> Statement a
switch = SwitchStmt def

while :: Default a => Expression a -> Statement a -> Statement a
while = WhileStmt def

dowhile :: Default a => Statement a -> Expression a -> Statement a
dowhile = DoWhileStmt def

break_ :: Default a => Maybe (Id a) -> Statement a
break_ = BreakStmt def

continue :: Default a => Maybe (Id a) -> Statement a
continue = ContinueStmt def

label :: Default a => Id a -> Statement a -> Statement a
label = LabelledStmt def

label_ :: Default a => String -> Statement a -> Statement a
label_ s = label (id_ s)

forin
  :: Default a =>
     ForInInit a -> Expression a -> Statement a -> Statement a
forin = ForInStmt def

try :: Default a => Statement a -> Statement a
try b = TryStmt def b Nothing Nothing

trycatch
  :: Default a =>
     Statement a -> CatchClause a -> Maybe (Statement a) -> Statement a
trycatch b c = TryStmt def b (Just c)

tryfinally
  :: Default a => Statement a -> Statement a -> Statement a
tryfinally b f = TryStmt def b Nothing (Just f)

trycatchfinally
  :: Default a =>
     Statement a -> CatchClause a -> Statement a -> Statement a
trycatchfinally b c f = TryStmt def b (Just c) (Just f)

throw :: Default a => Expression a -> Statement a
throw = ThrowStmt def

return_ :: Default a => Expression a -> Statement a
return_ = ReturnStmt def . Just

ret :: Default a => Statement a
ret = ReturnStmt def Nothing

with :: Default a => Expression a -> Statement a -> Statement a
with = WithStmt def

vds :: Default a => [VarDecl a] -> Statement a
vds = VarDeclStmt def

function
  :: Default a => Id a -> [Id a] -> [Statement a] -> Statement a
function = FunctionStmt def

function_
  :: Default a => String -> [Id a] -> [Statement a] -> Statement a
function_ s = function (id_ s)

id2string :: Id a -> Expression a
id2string (Id a s) = StringLit a s

-- | Helper function to convert LValues to expressions
lv2e :: LValue a -> Expression a
lv2e lval = case lval of
  LVar a vname -> VarRef a (Id a vname)
  LDot a obj fname -> DotRef a obj (Id a fname)
  LBracket a obj field -> BracketRef a obj field

-- | May fail
e2lv :: Expression a -> LValue a
e2lv e = case e of
  VarRef a (Id _ vname) -> LVar a vname
  DotRef a obj (Id _ fname) -> LDot a obj fname
  BracketRef a obj field -> LBracket a obj field
  _ -> error "expr2LVal: Can't convert an expression to an LValue"
  
forInInit2lv :: ForInInit a -> LValue a
forInInit2lv i = case i of
  ForInVar (Id a s) -> LVar a s
  ForInLVal lv      -> lv
