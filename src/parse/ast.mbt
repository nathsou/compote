
pub enum Type {
  Int; Void
} derive(Eq)

pub enum Expr {
  Constant(Int)
  Variable(String)
  Unary(~op: UnaryOp, ~expr: Expr)
  Binary(~op: BinaryOp, ~lhs: Expr, ~rhs: Expr)
  Assignment(~op: AssignmentOp, ~lhs: Expr, ~rhs: Expr)
  Conditional(~cond: Expr, ~then: Expr, ~otherwise: Expr)
}

pub enum Stmt {
  Return(Expr)
  Expression(Expr)
  If(~cond: Expr, ~then: Stmt, ~otherwise: Stmt?)
  Labeled(~label: String, ~stmt: Stmt)
  Goto(String)
  Compound(Array[BlockItem])
  Break(~label: String?)
  Continue(~label: String?)
  While(~cond: Expr, ~body: Stmt, ~label: String?)
  DoWhile(~body: Stmt, ~cond: Expr, ~label: String?)
  For(~init: ForInit, ~cond:  Expr?, ~post: Expr?, ~body: Stmt, ~label: String?)
  Null // for empty statements (just a semicolon)
}

pub enum BlockItem {
  Stmt(Stmt)
  Decl(Decl)
}

pub enum ForInit {
  InitEmpty
  InitDecl(Decl)
  InitExpr(Expr)
}

pub struct FunctionDef {
  return_type: Type
  name: String
  body: Array[BlockItem]
}

pub enum Decl {
  VariableDecl(~ty: Type, ~name: String, ~value: Expr?)
}

pub struct Program {
  functions: Array[FunctionDef]
}

pub enum UnaryOp {
  Neg; Complement; Not; Inc(~is_prefix: Bool); Dec(~is_prefix: Bool)
} derive(Eq)

pub enum BinaryOp {
  Add; Sub; Mul; Div; Rem
  BitwiseAnd; BitwiseOr; BitwiseXor;
  Shl; Shr
  Equ; Neq; Lss; Leq; Gtr; Geq
  LogicalAnd; LogicalOr
  Ternary
  Assignment(AssignmentOp)
} derive(Eq)

pub enum AssignmentOp {
  Assign; AssignAdd; AssignSub; AssignMul; AssignDiv; AssignRem
  AssignAnd; AssignOr; AssignXor; AssignShl; AssignShr
} derive(Eq)

// https://en.wikipedia.org/wiki/Operators_in_C_and_C++#Operator_precedence
pub fn precedence(self: BinaryOp) -> Int {
  match self {
    Mul | Div | Rem => 50
    Add | Sub => 45
    Shl | Shr => 40
    Lss | Leq | Gtr | Geq => 35
    Equ | Neq => 30
    BitwiseAnd => 28
    BitwiseXor => 25
    BitwiseOr => 20
    LogicalAnd => 10
    LogicalOr => 5
    Ternary => 3
    Assignment(_) => 1
  }
}
