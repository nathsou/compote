
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
    LogicalAnd | LogicalOr | Assignment(_) | Ternary => {
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


let arg_regs: Array[AsmReg] = [DI, SI, DX, CX, R8, R9]

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
    Call(~name, ~args, ~dst) => {
      // split_at currently panics if the index is out of bounds
      let (reg_args, stack_args) = if args.length() > 6 {
        args.split_at(6)
      } else {
        (args, [])
      }
      
      // stack align to 16 bytes
      let stack_padding = if stack_args.length() % 2 == 1 { 8 } else { 0 }

      if stack_padding > 0 {
        self.emit(AllocateStack(~size=stack_padding))
      }

      for i, arg in reg_args {
        let asm_arg = arg.asm()
        self.emit(Mov(~src=asm_arg, ~dst=Reg(arg_regs[i])))
      }

      // push the remaining arguments onto the stack in reverse order
      for i = stack_args.length() - 1; i >= 0; i = i - 1 {
        let asm_arg = stack_args[i].asm()
        match asm_arg {
          Reg(_) | Imm(_) => {
            self.emit(Push(asm_arg))
          }
          _ => {
            // if the argument is in memory, load it into a register first before pushing it
            self.emit(Mov(~src=asm_arg, ~dst=Reg(AX)))
            self.emit(Push(Reg(AX)))
          }
        }
      }

      self.emit(Call(name))

      let bytes_to_remove = 8 * stack_args.length() + stack_padding

      if bytes_to_remove > 0 {
        self.emit(DeallocateStack(~size=bytes_to_remove))
      }

      // retrieve the return value
      let dst = dst.asm()
      self.emit(Mov(~src=Reg(AX), ~dst))
    }
  }
}

fn align_to_16_bytes(size: Int) -> Int {
  let remainder = size % 16

  if remainder == 0 {
    size
  } else {
    size + 16 - remainder
  }
}

pub fn asm(self: TacFunctionDef) -> AsmFunctionDef!AsmBuilderError {
  let builder = AsmBuilder::new()

  // placeholder for stack allocation
  builder.emit(AllocateStack(~size=0))

  // copy arguments to the stack
  for i, param in self.params {
    // the first 6 arguments are passed in registers
    if i < 6 {
      let dst = Pseudo(param)
      let src = Reg(arg_regs[i])
      builder.emit(Mov(~src, ~dst))
    } else {
      // the remaining arguments are passed on the stack
      // but we need to load them into the current stack frame
      let dst = Pseudo(param)
      // offset by 16 bytes for the return address and base pointer
      let offset = 16 + 8 * (i - 6)
      builder.emit(Mov(~src=Stack(offset), ~dst))
    }
  }

  for inst in self.body {
    builder.convert_inst!(inst)
  }

  let pass1 = ReplacePseudoRegsPass::new()
  let insts1 = pass1.run(builder.instructions)

  let aligned_stack_size = align_to_16_bytes(pass1.stack_offset.abs())

  // replace the placeholder with the actual stack allocation, aligned to 16 bytes
  insts1[0] = AllocateStack(~size=aligned_stack_size)

  let insts2 = FixUpPass::run(insts1)

  { name: self.name, body: insts2, stack_size: pass1.stack_offset }
}

pub fn asm(self: TacProgram) -> AsmProgram!AsmBuilderError {
  let functions = []

  for f in self.functions {
    functions.push(f.asm!())
  }

  { functions, }
}
