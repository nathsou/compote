
pub struct AsmBuilder {
  instructions: Array[AsmInst]
}

pub type! AsmBuilderError {
  UneachableUnaryOpConversion(UnaryOp)
  UnreachableBinaryOpConversion(BinaryOp)
  UnreachableCondCodeConversion(BinaryOp)
} derive(Show)

pub fn AsmBuilder::new() -> AsmBuilder {
  { instructions: [] }
}

fn emit(self: AsmBuilder, inst: AsmInst) -> Unit {
  self.instructions.push(inst)
}

fn asm(self: UnaryOp) -> AsmUnaryOp!AsmBuilderError {
  match self {
    Neg => Neg
    Complement => BitwiseNot
    Not => raise UneachableUnaryOpConversion(self)
  }
}

fn asm(self: BinaryOp) -> AsmBinaryOp!AsmBuilderError {
  match self {
    Add => Add
    Sub => Sub
    Mul => Mul
    BitwiseAnd => And
    BitwiseOr => Or
    BitwiseXor => Xor
    Shl => Shl
    Shr => Sal
    Div | Rem | Equ | Neq | Lss | Leq | Gtr | Geq | LogicalAnd | LogicalOr => raise UnreachableBinaryOpConversion(self)
  }
}

fn to_cond_code(self: BinaryOp) -> CondCode!AsmBuilderError {
  match self {
    Equ => Eq
    Neq => Ne
    Lss => Lt
    Leq => Le
    Gtr => Gt
    Geq => Ge
    _ => raise UnreachableCondCodeConversion(self)
  }
}

fn asm(self: TacValue) -> AsmOperand {
  match self {
    Constant(n) => Imm(n)
    Variable(name) => Pseudo(name)
  }
}

fn convert_inst(self: AsmBuilder, inst: TacInst) -> Unit!AsmBuilderError {
  match inst {
    Return(value) => {
      let src = value.asm()
      self.emit(Mov(~src, ~dst=Reg(AX)))
      self.emit(Ret)
    }
    Unary(~op=Not, ~src, ~dst) => {
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Cmp(~src=Imm(0), ~dst=src))
      // clear the destination register before the set instruction
      self.emit(Mov(~src=Imm(0), ~dst))
      self.emit(SetCond(~cond=Eq, ~dst))
    }
    Unary(~op, ~src, ~dst) => {
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Mov(~src, ~dst))
      self.emit(Unary(~op=op.asm!(), ~dst))
    }
    Binary(~op=Lss | Leq | Gtr | Geq | Equ | Neq as op, ~lhs, ~rhs, ~dst) => {
      let lhs = lhs.asm()
      let rhs = rhs.asm()
      let dst = dst.asm()
      self.emit(Cmp(~src=rhs, ~dst=lhs))
      // clear the destination register before the set instruction
      self.emit(Mov(~src=Imm(0), ~dst))
      self.emit(SetCond(~cond=op.to_cond_code!(), ~dst))
    }
    Binary(~op, ~lhs, ~rhs, ~dst) => {
      let lhs = lhs.asm()
      let rhs = rhs.asm()
      let dst = dst.asm()
      
      match op {
        Div | Rem => {
          let output_reg = if op == Div { AX } else { DX }
          self.emit(Mov(~src=lhs, ~dst=Reg(AX)))
          self.emit(Cdq)
          self.emit(IDiv(~src=rhs))
          self.emit(Mov(~src=Reg(output_reg), ~dst))
        }
        _ => {
          self.emit(Mov(~src=lhs, ~dst))
          self.emit(Binary(~op=op.asm!(), ~src=rhs, ~dst))
        }
      }
    }
    Jump(target) => {
      self.emit(Jmp(target))
    }
    JumpIfZero(~cond, ~target) => {
      let cond = cond.asm()
      self.emit(Cmp(~src=Imm(0), ~dst=cond))
      self.emit(JmpCond(~cond=Eq, ~target))
    }
    JumpIfNotZero(~cond, ~target) => {
      let cond = cond.asm()
      self.emit(Cmp(~src=Imm(0), ~dst=cond))
      self.emit(JmpCond(~cond=Ne, ~target))
    }
    Copy(~src, ~dst)  => {
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Mov(~src, ~dst))
    }
    Label(name) => {
      self.emit(Label(name))
    }
  }
}

pub fn asm(self: TacFunctionDef) -> AsmFunctionDef!AsmBuilderError {
  let builder = AsmBuilder::new()

  // placeholder for stack allocation
  builder.emit(AllocateStack(~size=0))

  for inst in self.body {
    builder.convert_inst!(inst)
  }

  let pass1 = ReplacePseudoRegsPass::new()
  let insts1 = pass1.run(builder.instructions)

  // replace the placeholder with the actual stack allocation
  insts1[0] = AllocateStack(~size=pass1.stack_offset)

  let insts2 = FixUpPass::run(insts1)

  { name: self.name, body: insts2 }
}

pub fn asm(self: TacProgram) -> AsmProgram!AsmBuilderError {
  let functions = []

  for f in self.functions {
    functions.push(f.asm!())
  }

  { functions, }
}
