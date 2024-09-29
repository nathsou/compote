
pub enum AsmReg {
  AX; CX; DX; DI; SI; R8; R9; R10; R11
}

pub enum CondCode {
  Eq; Ne; Gt; Ge; Lt; Le
}

pub enum RegSize {
  Byte; Long; Quad
}

pub enum AsmOperand {
  Imm(Int)
  Reg(AsmReg)
  RegSize(AsmReg, RegSize)
  Pseudo(String)
  Stack(Int)
  Data(String)
}

pub enum AsmUnaryOp {
  Neg; BitwiseNot
}

pub enum AsmBinaryOp {
  Add; Sub; Mul
  And; Or; Xor
  Shl; Sal
}

pub enum AsmInst {
  Mov(~src: AsmOperand, ~dst: AsmOperand)
  Unary(~op: AsmUnaryOp, ~dst: AsmOperand)
  Binary(~op: AsmBinaryOp, ~src: AsmOperand, ~dst: AsmOperand)
  Cmp(~src: AsmOperand, ~dst: AsmOperand)
  IDiv(~src: AsmOperand)
  Cdq
  Jmp(String)
  JmpCond(~cond: CondCode, ~target: String)
  SetCond(~cond: CondCode, ~dst: AsmOperand)
  Label(String)
  AllocateStack(~size: Int)
  DeallocateStack(~size: Int)
  Push(AsmOperand)
  Call(String)
  Ret
}

pub struct AsmFunctionDef {
  name: String
  global: Bool
  body: Array[AsmInst]
  stack_size: Int
}

pub struct AsmStaticVarDef {
  name: String
  global: Bool
  init: Int
}

pub enum OperatingSystem {
  Linux; MacOS
} derive(Eq)

pub struct AsmProgram {
  functions: Array[AsmFunctionDef]
  static_vars: Array[AsmStaticVarDef]
}
