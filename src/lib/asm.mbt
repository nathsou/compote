
pub enum AsmReg {
  AX; DX; R10; R11
}

pub enum AsmOperand {
  Imm(Int)
  Reg(AsmReg)
  Pseudo(String)
  Stack(Int)
}

pub enum AsmUnaryOp {
  Neg; Not
}

pub enum AsmBinaryOp {
  Add; Sub; Mul
}

pub enum AsmInst {
  Mov(~src: AsmOperand, ~dst: AsmOperand)
  Unary(~op: AsmUnaryOp, ~dst: AsmOperand)
  Binary(~op: AsmBinaryOp, ~src: AsmOperand, ~dst: AsmOperand)
  IDiv(~src: AsmOperand)
  Cdq
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
