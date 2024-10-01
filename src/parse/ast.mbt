
pub enum Type {
  Int
  Long
  FunType(~params: Array[Type], ~ret: Type)
} derive(Eq)

pub enum Constant {
  IntConst(Int)
  LongConst(Int64)
} derive (Eq, Hash)

pub enum StorageClass {
  Static; Extern
} derive(Eq)

pub enum Expr {
  Constant(~const: Constant, ~ty: Type?)
  Variable(~name: String, ~ty: Type?)
  Cast(~target_ty: Type, ~expr: Expr, ~ty: Type?)
  Unary(~op: UnaryOp, ~expr: Expr, ~ty: Type?)
  Binary(~op: BinaryOp, ~lhs: Expr, ~rhs: Expr, ~ty: Type?)
  Assignment(~op: AssignmentOp, ~lhs: Expr, ~rhs: Expr, ~ty: Type?)
  Conditional(~cond: Expr, ~then: Expr, ~otherwise: Expr, ~ty: Type?)
  Call(~name: String, ~args: Array[Expr], ~ty: Type?)
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
  Switch(~expr: Expr, ~body: Stmt, ~label: String?, ~cases_count: Int) // cases count includes default
  Case(~expr: Expr, ~body: Stmt)
  Default(~body: Stmt)
  Null // for empty statements (just a semicolon)
}

pub enum BlockItem {
  Stmt(Stmt)
  Decl(Decl)
}

pub enum ForInit {
  InitEmpty
  InitDecl(VariableDecl)
  InitExpr(Expr)
}

pub struct VariableDecl {
  ty: Type
  name: String
  value: Expr?
  storage_class: StorageClass?
}

pub struct FunctionParam {
  ty: Type
  name: String
}

pub struct FunctionDecl {
  return_ty: Type
  name: String
  params: Array[FunctionParam]
  body: Array[BlockItem]?
  storage_class: StorageClass?
}

pub enum Decl {
  VarDecl(VariableDecl)
  FunDecl(FunctionDecl)
}

pub struct Program {
  decls: Array[Decl]
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

pub fn to_type(self: FunctionDecl) -> Type {
  FunType(~params=self.params.map(fn(param) { param.ty }), ~ret=self.return_ty)
}

pub fn ty(self: Expr) -> Type {
  (match self {
    Constant(~const=_, ~ty) => ty
    Variable(~name=_, ~ty) => ty
    Cast(~target_ty=_, ~expr=_, ~ty) => ty
    Unary(~op=_, ~expr=_, ~ty) => ty
    Binary(~op=_, ~lhs=_, ~rhs=_, ~ty) => ty
    Assignment(~op=_, ~lhs=_, ~rhs=_, ~ty) => ty
    Conditional(~cond=_, ~then=_, ~otherwise=_, ~ty) => ty
    Call(~name=_, ~args=_, ~ty) => ty
  }).unwrap()
}
