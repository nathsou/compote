
pub type FixUpPass Unit

fn FixUpPass::rewrite_inst(inst: AsmInst) -> Array[AsmInst] {
  match inst {
     // some instructions with stack addresses as both operands are invalid
    Mov(~src=Stack(src), ~dst=Stack(dst)) => [
      Mov(~src=Stack(src), ~dst=Reg(R10)),
      Mov(~src=Reg(R10), ~dst=Stack(dst)),
    ]
    Binary(~op=Add | Sub | And | Or | Xor as op, ~src=Stack(src), ~dst=Stack(dst)) => [
      Mov(~src=Stack(src), ~dst=Reg(R10)),
      Binary(~op, ~src=Reg(R10), ~dst=Stack(dst)),
    ]
    Cmp(~src=Stack(src), ~dst=Stack(dst)) => [
      Mov(~src=Stack(src), ~dst=Reg(R10)),
      Cmp(~src=Reg(R10), ~dst=Stack(dst)),
    ]
    // if the shift amount is an immediate, it can be used directly
    Binary(~op=Shl | Sal, ~src=Imm(_), ~dst=_) => [inst]
    // otherwise, the shift amount must be stored in the cl registern 
    Binary(~op=Shl | Sal as op, ~src, ~dst) => [
      Mov(~src, ~dst=Reg(CL)),
      Binary(~op, ~src=RegSize(CL, Byte), ~dst),
    ]
    // imul can't use a memory address as destination
    Binary(~op=Mul, ~src, ~dst=Stack(dst)) => [
      Mov(~src=Stack(dst), ~dst=Reg(R11)),
      Binary(~op=Mul, ~src, ~dst=Reg(R11)),
      Mov(~src=Reg(R11), ~dst=Stack(dst)),
    ]
    // idiv, cmp instructions do not support immediate operands
    IDiv(~src=Imm(n)) => [
      Mov(~src=Imm(n), ~dst=Reg(R10)),
      IDiv(~src=Reg(R10)),
    ]
    Cmp(~src, ~dst=Imm(n)) => [
      Mov(~src=Imm(n), ~dst=Reg(R11)),
      Cmp(~src, ~dst=Reg(R11)),
    ]
    _ => [inst]
  }
}

pub fn FixUpPass::run(insts: Array[AsmInst]) -> Array[AsmInst] {
  let new_insts = []

  for inst in insts {
    for new_inst in FixUpPass::rewrite_inst(inst) {
      new_insts.push(new_inst)
    }
  }

  new_insts
}
