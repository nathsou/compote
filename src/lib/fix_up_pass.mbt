
pub type FixUpPass Unit

fn FixUpPass::rewrite_inst(inst: AsmInst) -> Array[AsmInst] {
  match inst {
    Mov(~src=Stack(src), ~dst=Stack(dst)) => [
      Mov(~src=Stack(src), ~dst=Reg(R10)),
      Mov(~src=Reg(R10), ~dst=Stack(dst)),
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
