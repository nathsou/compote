
pub type FixUpPass Unit

fn FixUpPass::rewrite_inst(inst: AsmInst) -> Array[AsmInst] {
  match inst {
     // mov, add, sub instructions with stack addresses as both operands are invalid
    Mov(~src=Stack(src), ~dst=Stack(dst)) => [
      Mov(~src=Stack(src), ~dst=Reg(R10)),
      Mov(~src=Reg(R10), ~dst=Stack(dst)),
    ]
    Binary(~op=Add | Sub as op, ~src=Stack(src), ~dst=Stack(dst)) => [
      Mov(~src=Stack(src), ~dst=Reg(R10)),
      Binary(~op, ~src=Reg(R10), ~dst=Stack(dst)),
    ]
    // imul can't use a memory address as its destination
    Binary(~op=Mul, ~src, ~dst=Stack(dst)) => [
      Mov(~src=Stack(dst), ~dst=Reg(R11)),
      Binary(~op=Mul, ~src, ~dst=Reg(R11)),
      Mov(~src=Reg(R11), ~dst=Stack(dst)),
    ]
    // idiv instructions doe not support immediate operands
    IDiv(~src=Imm(n)) => [
      Mov(~src=Imm(n), ~dst=Reg(R10)),
      IDiv(~src=Reg(R10)),
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
