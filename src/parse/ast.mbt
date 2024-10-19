
pub enum Type {
  Int
  Long
  UnsignedInt
  UnsignedLong
  FunType(~params: Array[Type], ~ret: Type)
} derive(Eq)

pub enum Constant {
  IntConst(Int)
  LongConst(Int64)
  UnsignedIntConst(UInt)
  UnsignedLongConst(UInt64)
} derive (Eq, Hash)

pub enum StorageClass {
  Static; Extern
} derive(Eq, Hash)

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
} derive(Show)

pub struct VariableDecl {
  ty: Type
  name: String
  value: Expr?
  storage_class: StorageClass?
} derive(Show)

pub struct FunctionParam {
  ty: Type
  name: String
} derive(Show)

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
} derive(Show)

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
  FunType(params=self.params.map(fn(param) { param.ty }), ret=self.return_ty)
}

pub fn ty(self: Expr) -> Type {
  (match self {
    Constant(const=_, ~ty) => ty
    Variable(name=_, ~ty) => ty
    Cast(target_ty=_, expr=_, ~ty) => ty
    Unary(op=_, expr=_, ~ty) => ty
    Binary(op=_, lhs=_, rhs=_, ~ty) => ty
    Assignment(op=_, lhs=_, rhs=_, ~ty) => ty
    Conditional(cond=_, then=_, otherwise=_, ~ty) => ty
    Call(name=_, args=_, ~ty) => ty
  }).unwrap()
}

pub fn byte_size(self: Type) -> UInt {
  match self {
    Int | UnsignedInt => 4
    Long | UnsignedLong => 8
    FunType(_) => 0
  }
}

pub fn is_signed(self: Type) -> Bool {
  match self {
    Int | Long => true
    UnsignedInt | UnsignedLong | FunType(_) => false
  }
}

pub fn fit_const_to_type(const: Constant, ty: Type) -> Constant! {
  match (const, ty) {
    (IntConst(n), Int) => IntConst(n)
    (IntConst(n), Long) => LongConst(n.to_int64())
    (IntConst(n), UnsignedInt) => UnsignedIntConst(n.reinterpret_as_uint())
    (IntConst(n), UnsignedLong) => UnsignedLongConst(n.to_uint64())
    (LongConst(n), Int) => IntConst(n.to_int())
    (LongConst(n), Long) => LongConst(n)
    (LongConst(n), UnsignedInt) => UnsignedIntConst(n.reinterpret_as_uint64().to_uint())
    (LongConst(n), UnsignedLong) => UnsignedLongConst(n.reinterpret_as_uint64())
    (UnsignedIntConst(n), UnsignedInt) => UnsignedIntConst(n)
    (UnsignedIntConst(n), UnsignedLong) => UnsignedLongConst(n.to_uint64())
    (UnsignedIntConst(n), Int) => IntConst(n.reinterpret_as_int())
    (UnsignedIntConst(n), Long) => LongConst(n.to_int64())
    (UnsignedLongConst(n), UnsignedInt) => UnsignedIntConst(n.to_uint())
    (UnsignedLongConst(n), UnsignedLong) => UnsignedLongConst(n)
    (UnsignedLongConst(n), Long) => LongConst(n.to_int64())
    (UnsignedLongConst(n), Int) => IntConst(n.to_int())
    (_, FunType(_)) => raise Failure("Invalid constant type conversion")
  }
}
