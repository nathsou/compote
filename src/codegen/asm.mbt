
pub enum AsmReg {
  AX; CX; DX; DI; SI; R8; R9; R10; R11; SP
  XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6; XMM7
  XMM8; XMM9; XMM10; XMM11; XMM12; XMM13; XMM14; XMM15
} derive(Eq, Show)

pub enum AsmType {
  LongWord; QuadWord; Double
} derive (Eq, Show)

pub enum CondCode {
  Eq; Ne; Gt; Ge; Lt; Le; Above; AboveEq; Below; BelowEq
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
  Neg; BitwiseNot; Shr
} derive(Show)

pub enum AsmBinaryOp {
  Add; Sub; Mul
  And; Or; Xor
  Shl; Shr; Sal; Sar
  DivDouble
} derive(Show)

pub enum AsmInst {
  Mov(src~: AsmOperand, dst~: AsmOperand, ty~: AsmType)
  Movsx(src~: AsmOperand, dst~: AsmOperand)
  MovZeroExtend(src~: AsmOperand, dst~: AsmOperand)
  Unary(op~: AsmUnaryOp, dst~: AsmOperand, ty~: AsmType)
  Binary(op~: AsmBinaryOp, src~: AsmOperand, dst~: AsmOperand, ty~: AsmType)
  Cmp(src~: AsmOperand, dst~: AsmOperand, ty~: AsmType)
  IDiv(src~: AsmOperand, ty~: AsmType)
  Div(src~: AsmOperand, ty~: AsmType)
  Cdq(ty~: AsmType)
  Cvttsd2si(dst_ty~: AsmType, src~: AsmOperand, dst~: AsmOperand)
  Cvtsi2sd(src_ty~: AsmType, src~: AsmOperand, dst~: AsmOperand)
  Jmp(String)
  JmpCond(cond~: CondCode, target~: String)
  SetCond(cond~: CondCode, dst~: AsmOperand)
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
} derive(Show)

pub struct AsmStaticVarDef {
  name: String
  global: Bool
  alignment: Int64
  init: @validate.StaticInit
} derive(Show)

pub struct AsmStaticConstDef {
  name: String
  alignment: Int64
  init: @validate.StaticInit
} derive(Show)

pub enum OperatingSystem {
  Linux; MacOS
} derive(Eq)

pub struct AsmProgram {
  functions: Array[AsmFunctionDef]
  static_vars: Array[AsmStaticVarDef]
  static_consts: Array[AsmStaticConstDef]
} derive(Show)

pub enum BackendSymbolTableEntry {
  ObjEntry(ty~: AsmType, is_static~: Bool, is_constant~: Bool)
  FunEntry(is_defined~: Bool)
}

pub fn AsmType::size(self: AsmType) -> Int64 {
  match self {
    LongWord => 4
    QuadWord => 8
    Double => 8
  }
}

pub fn AsmType::alignment(self: AsmType) -> Int64 {
  match self {
    LongWord => 4
    QuadWord => 8
    Double => 8
  }
}

pub fn AsmType::suffix(self: AsmType) -> String raise EmitterError {
  match self {
    LongWord => "l"
    QuadWord => "q"
    Double => raise EmitterError::Unreachable("no suffix for double type")
  }
}

pub struct BackendSymbolTable(Map[String, BackendSymbolTableEntry])

fn BackendSymbolTable::get_symbol_ty(self: BackendSymbolTable, name: String) -> AsmType raise {
  match self.0.get(name) {
    Some(ObjEntry(ty~, ..)) => ty
    _ => fail("symbol not found in symbol table")
  }
}

fn BackendSymbolTable::is_symbol_static(self: BackendSymbolTable, name: String) -> Bool {
  self.0.get(name) is Some(ObjEntry(is_static=true, ..))
}
