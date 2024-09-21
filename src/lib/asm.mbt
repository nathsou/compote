
pub enum AsmReg {
  AX; DX; R10; R11; CL
}

pub enum CondCode {
  Eq; Ne; Gt; Ge; Lt; Le
}

pub enum RegSize {
  Byte; Long
}

pub enum AsmOperand {
  Imm(Int)
  Reg(AsmReg)
  RegSize(AsmReg, RegSize)
  Pseudo(String)
  Stack(Int)
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
  Ret
}

pub struct AsmFunctionDef {
  name: String
  body: Array[AsmInst]
}

pub enum OperatingSystem {
  Linux; MacOS
} derive(Eq)

pub struct AsmProgram {
  functions: Array[AsmFunctionDef]
}
