
pub struct AsmBuilder {
  instructions: Array[AsmInst]
}

pub fn AsmBuilder::new() -> AsmBuilder {
  { instructions: [] }
}

fn emit(self: AsmBuilder, inst: AsmInst) -> Unit {
  self.instructions.push(inst)
}

fn asm(self: UnaryOp) -> AsmUnaryOp {
  match self {
    Neg => Neg
    Not => Not
  }
}

fn asm(self: TacValue) -> AsmOperand {
  match self {
    Constant(n) => Imm(n)
    Variable(name) => Pseudo(name)
  }
}

fn convert_inst(self: AsmBuilder, inst: TacInst) -> Unit {
  match inst {
    Return(value) => {
      let src = value.asm()
      self.emit(Mov(~src, ~dst=Reg(AX)))
      self.emit(Ret)
    }
    Unary(~op, ~src, ~dst) => {
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Mov(~src, ~dst))
      self.emit(Unary(~op=op.asm(), ~dst))
    }
  }
}

pub fn asm(self: TacFunctionDef) -> AsmFunctionDef {
  let builder = AsmBuilder::new()

  // placeholder for stack allocation
  builder.emit(AllocateStack(~size=0))

  for inst in self.body {
    builder.convert_inst(inst)
  }

  let pass1 = ReplacePseudoRegsPass::new()
  let insts1 = pass1.run(builder.instructions)

  // replace the placeholder with the actual stack allocation
  insts1[0] = AllocateStack(~size=pass1.stack_offset)

  let insts2 = FixUpPass::run(insts1)

  { name: self.name, body: insts2 }
}

pub fn asm(self: TacProgram) -> AsmProgram {
  { functions: self.functions.map(TacFunctionDef::asm) }
}
