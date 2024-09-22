
pub struct AsmBuilder {
  instructions: Array[AsmInst]
}

pub type! AsmBuilderError {
  UneachableUnaryOpConversion(@parse.UnaryOp)
  UnreachableBinaryOpConversion(@parse.BinaryOp)
  UnreachableCondCodeConversion(@parse.BinaryOp)
} derive(Show)

pub fn AsmBuilder::new() -> AsmBuilder {
  { instructions: [] }
}

fn emit(self: AsmBuilder, inst: AsmInst) -> Unit {
  self.instructions.push(inst)
}

fn convert_unary_op(op: @parse.UnaryOp) -> AsmUnaryOp!AsmBuilderError {
  match op {
    Neg => Neg
    Complement => BitwiseNot
    Not | Dec(_) | Inc(_) => raise UneachableUnaryOpConversion(op)
  }
}

fn convert_binary_op(op: @parse.BinaryOp) -> AsmBinaryOp!AsmBuilderError {
  match op {
    Add => Add
    Sub => Sub
    Mul => Mul
    BitwiseAnd => And
    BitwiseOr => Or
    BitwiseXor => Xor
    Shl => Shl
    Shr => Sal
    Div | Rem | Equ | Neq | Lss | Leq | Gtr | Geq |
    LogicalAnd | LogicalOr | Assignment(_) => {
      raise UnreachableBinaryOpConversion(op)
    }
  }
}

fn convert_cond_code(op: @parse.BinaryOp) -> CondCode!AsmBuilderError {
  match op {
    Equ => Eq
    Neq => Ne
    Lss => Lt
    Leq => Le
    Gtr => Gt
    Geq => Ge
    _ => raise UnreachableCondCodeConversion(op)
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
      self.emit(Unary(~op=convert_unary_op!(op), ~dst))
    }
    Binary(~op=Lss | Leq | Gtr | Geq | Equ | Neq as op, ~lhs, ~rhs, ~dst) => {
      let lhs = lhs.asm()
      let rhs = rhs.asm()
      let dst = dst.asm()
      self.emit(Cmp(~src=rhs, ~dst=lhs))
      // clear the destination register before the set instruction
      self.emit(Mov(~src=Imm(0), ~dst))
      self.emit(SetCond(~cond=convert_cond_code!(op), ~dst))
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
          self.emit(Binary(~op=convert_binary_op!(op), ~src=rhs, ~dst))
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
