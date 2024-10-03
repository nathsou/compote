
pub enum AsmReg {
  AX; CX; DX; DI; SI; R8; R9; R10; R11; SP
} derive(Eq, Show)

pub enum AsmType {
  LongWord; QuadWord
} derive (Eq, Show)

pub enum CondCode {
  Eq; Ne; Gt; Ge; Lt; Le
} derive(Show)

pub enum RegSize {
  Byte; Long; Quad
} derive(Show)

pub enum AsmOperand {
  Imm(Int64)
  Reg(AsmReg)
  RegSize(AsmReg, RegSize)
  Pseudo(String)
  Stack(Int64)
  Data(String)
} derive(Show)

pub enum AsmUnaryOp {
  Neg; BitwiseNot
} derive(Show)

pub enum AsmBinaryOp {
  Add; Sub; Mul
  And; Or; Xor
  Shl; Sal
} derive(Show)

pub enum AsmInst {
  Mov(~src: AsmOperand, ~dst: AsmOperand, ~ty: AsmType)
  Movsx(~src: AsmOperand, ~dst: AsmOperand)
  Unary(~op: AsmUnaryOp, ~dst: AsmOperand, ~ty: AsmType)
  Binary(~op: AsmBinaryOp, ~src: AsmOperand, ~dst: AsmOperand, ~ty: AsmType)
  Cmp(~src: AsmOperand, ~dst: AsmOperand, ~ty: AsmType)
  IDiv(~src: AsmOperand, ~ty: AsmType)
  Cdq(~ty: AsmType)
  Jmp(String)
  JmpCond(~cond: CondCode, ~target: String)
  SetCond(~cond: CondCode, ~dst: AsmOperand)
  Label(String)
  Push(AsmOperand)
  Call(String)
  Ret
} derive(Show)

pub struct AsmFunctionDef {
  name: String
  global: Bool
  body: Array[AsmInst]
  stack_size: Int64
}

pub struct AsmStaticVarDef {
  name: String
  global: Bool
  alignment: Int64
  init: @validate.StaticInit
}

pub enum OperatingSystem {
  Linux; MacOS
} derive(Eq)

pub struct AsmProgram {
  functions: Array[AsmFunctionDef]
  static_vars: Array[AsmStaticVarDef]
}

pub enum BackendSymbolTableEntry {
  ObjEntry(~ty: AsmType, ~is_static: Bool)
  FunEntry(~is_defined: Bool)
}

pub fn size(self: AsmType) -> Int64 {
  match self {
    LongWord => 4
    QuadWord => 8
  }
}

pub fn alignment(self: AsmType) -> Int64 {
  match self {
    LongWord => 4
    QuadWord => 8
  }
}

pub fn suffix(self: AsmType) -> String {
  match self {
    LongWord => "l"
    QuadWord => "q"
  }
}

pub type BackendSymbolTable Map[String, BackendSymbolTableEntry]

fn get_symbol_ty(self: BackendSymbolTable, name: String) -> AsmType! {
  match self._[name] {
    Some(ObjEntry(~ty, ~is_static=_)) => ty
    _ => raise Failure("symbol not found in symbol table")
  }
}

fn is_symbol_static(self: BackendSymbolTable, name: String) -> Bool {
  match self._[name] {
    Some(ObjEntry(~ty=_, ~is_static)) => is_static
    _ => false
  }
}
