
pub enum AsmReg {
  AX; R10
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

pub enum AsmInst {
  Mov(~src: AsmOperand, ~dst: AsmOperand)
  Unary(~op: AsmUnaryOp, ~dst: AsmOperand)
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
