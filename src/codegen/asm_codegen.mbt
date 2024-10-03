
pub struct AsmBuilder {
  instructions: Array[AsmInst]
  symbols: Map[String, @validate.SymbolTableEntry]
}

pub type! AsmBuilderError {
  UneachableUnaryOpConversion(@parse.UnaryOp)
  UnreachableBinaryOpConversion(@parse.BinaryOp)
  UnreachableCondCodeConversion(@parse.BinaryOp)
  Unreachable(String)
} derive(Show)

pub fn AsmBuilder::new(symbols: Map[String, @validate.SymbolTableEntry]) -> AsmBuilder {
  { instructions: [], symbols }
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
    Constant(IntConst(n)) => Imm(n.to_int64())
    Constant(LongConst(n)) => Imm(n)
    Variable(name) => Pseudo(name)
  }
}

fn convert_type(ty: @parse.Type) -> AsmType!AsmBuilderError {
  match ty {
    Int => LongWord
    Long => QuadWord
    _ => raise AsmBuilderError::Unreachable("converting function type to AsmType")
  }
}

fn value_ty(self: AsmBuilder, value: TacValue) -> AsmType!AsmBuilderError {
  match value {
    Constant(IntConst(_)) => LongWord
    Constant(LongConst(_)) => QuadWord
    Variable(name) => {
      match self.symbols[name] {
        Some(entry) => convert_type!(entry.ty())
        None => raise AsmBuilderError::Unreachable("variable not found in symbol table")
      }
    }
  }
}

let arg_regs: Array[AsmReg] = [DI, SI, DX, CX, R8, R9]

fn convert_inst(self: AsmBuilder, inst: TacInst) -> Unit!AsmBuilderError {
  match inst {
    Return(value) => {
      let src = value.asm()
      self.emit(Mov(~src, ~dst=Reg(AX), ~ty=self.value_ty!(value)))
      self.emit(Ret)
    }
    SignExtend(~src, ~dst) => {
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Movsx(~src, ~dst))
    }
    Truncate(~src, ~dst) => {
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Mov(~src, ~dst, ~ty=LongWord))
    }
    Unary(~op=Not, ~src, ~dst) => {
      let src_ty = self.value_ty!(src)
      let dst_ty = self.value_ty!(dst)
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Cmp(~src=Imm(0), ~dst=src, ~ty=src_ty))
      // clear the destination register before the set instruction
      self.emit(Mov(~src=Imm(0), ~dst, ~ty=dst_ty))
      self.emit(SetCond(~cond=Eq, ~dst))
    }
    Unary(~op, ~src, ~dst) => {
      let src_ty = self.value_ty!(src)
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Mov(~src, ~dst, ~ty=src_ty))
      self.emit(Unary(~op=convert_unary_op!(op), ~dst, ~ty=src_ty))
    }
    Binary(~op=Lss | Leq | Gtr | Geq | Equ | Neq as op, ~lhs, ~rhs, ~dst) => {
      let lhs_ty = self.value_ty!(lhs)
      let dst_ty = self.value_ty!(dst)
      let lhs = lhs.asm()
      let rhs = rhs.asm()
      let dst = dst.asm()
      self.emit(Cmp(~src=rhs, ~dst=lhs, ~ty=lhs_ty))
      // clear the destination register before the set instruction
      self.emit(Mov(~src=Imm(0), ~dst, ~ty=dst_ty))
      self.emit(SetCond(~cond=convert_cond_code!(op), ~dst))
    }
    Binary(~op, ~lhs, ~rhs, ~dst) => {
      let lhs_ty = self.value_ty!(lhs)
      let dst_ty = self.value_ty!(dst)
      let lhs = lhs.asm()
      let rhs = rhs.asm()
      let dst = dst.asm()
      
      match op {
        Equ | Neq | Gtr | Geq | Lss | Leq => {
          self.emit(Cmp(~src=rhs, ~dst=lhs, ~ty=lhs_ty))
          // clear the destination register before the set instruction
          self.emit(Mov(~src=Imm(0), ~dst, ~ty=dst_ty))
          self.emit(SetCond(~cond=convert_cond_code!(op), ~dst))
        }
        Div | Rem => {
          let output_reg = if op == Div { AX } else { DX }
          self.emit(Mov(~src=lhs, ~dst=Reg(AX), ~ty=lhs_ty))
          self.emit(Cdq(~ty=lhs_ty))
          self.emit(IDiv(~src=rhs, ~ty=lhs_ty))
          self.emit(Mov(~src=Reg(output_reg), ~dst, ~ty=lhs_ty))
        }
        _ => {
          self.emit(Mov(~src=lhs, ~dst, ~ty=lhs_ty))
          self.emit(Binary(~op=convert_binary_op!(op), ~src=rhs, ~dst, ~ty=lhs_ty))
        }
      }
    }
    Jump(target) => {
      self.emit(Jmp(target))
    }
    JumpIfZero(~cond, ~target) => {
      let cond_ty = self.value_ty!(cond)
      let cond = cond.asm()
      self.emit(Cmp(~src=Imm(0), ~dst=cond, ~ty=cond_ty))
      self.emit(JmpCond(~cond=Eq, ~target))
    }
    JumpIfNotZero(~cond, ~target) => {
      let cond_ty = self.value_ty!(cond)
      let cond = cond.asm()
      self.emit(Cmp(~src=Imm(0), ~dst=cond, ~ty=cond_ty))
      self.emit(JmpCond(~cond=Ne, ~target))
    }
    Copy(~src, ~dst)  => {
      let ty = self.value_ty!(src)
      let src = src.asm()
      let dst = dst.asm()
      self.emit(Mov(~src, ~dst, ~ty))
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
      let stack_padding: Int64 = if stack_args.length() % 2 == 1 { 8 } else { 0 }

      if stack_padding > 0 {
        self.emit(Binary(~op=Sub, ~src=Imm(stack_padding), ~dst=Reg(SP), ~ty=QuadWord))
      }

      for i, arg in reg_args {
        let asm_arg = arg.asm()
        self.emit(Mov(~src=asm_arg, ~dst=Reg(arg_regs[i]), ~ty=self.value_ty!(arg)))
      }

      // push the remaining arguments onto the stack in reverse order
      for i = stack_args.length() - 1; i >= 0; i = i - 1 {
        let arg = stack_args[i]
        let asm_arg = arg.asm()
        match asm_arg {
          Reg(_) | Imm(_) => {
            self.emit(Push(asm_arg))
          }
          _ => {
            let asm_ty = self.value_ty!(arg)
            if asm_ty == QuadWord {
              self.emit(Push(asm_arg))
            } else {
              // if the argument is 4-bytes in memory, load it into a register first before pushing it
              self.emit(Mov(~src=asm_arg, ~dst=Reg(AX), ~ty=asm_ty))
              self.emit(Push(Reg(AX)))
            }
          }
        }
      }

      self.emit(Call(name))

      let bytes_to_remove = (8).to_int64() * stack_args.length().to_int64() + stack_padding

      if bytes_to_remove > 0 {
        self.emit(Binary(~op=Add, ~src=Imm(bytes_to_remove), ~dst=Reg(SP), ~ty=QuadWord))
      }

      // retrieve the return value
      let dst_ty = self.value_ty!(dst)
      let dst = dst.asm()
      self.emit(Mov(~src=Reg(AX), ~dst, ~ty=dst_ty))
    }
  }
}

fn align_to_16_bytes(size: Int64) -> Int64 {
  let remainder = size % 16

  if remainder == 0 {
    size
  } else {
    size + 16 - remainder
  }
}

pub fn asm(self: TacFunctionDef, symbols: Map[String, @validate.SymbolTableEntry]) -> AsmFunctionDef!AsmBuilderError {
  let builder = AsmBuilder::new(symbols)

  // placeholder for stack allocation
  builder.emit(Binary(~op=Sub, ~src=Imm(0), ~dst=Reg(SP), ~ty=QuadWord))

  // copy arguments to the stack
  for i, param in self.params {
    let param_ty = match symbols[param] {
      Some(entry) => convert_type!(entry.ty())
      None => raise AsmBuilderError::Unreachable("parameter not found in symbol table")
    }

    // the first 6 arguments are passed in registers
    if i < 6 {
      let dst = Pseudo(param)
      let src = Reg(arg_regs[i])
      builder.emit(Mov(~src, ~dst, ~ty=param_ty))
    } else {
      // the remaining arguments are passed on the stack
      // but we need to load them into the current stack frame
      let dst = Pseudo(param)
      // offset by 16 bytes for the return address and base pointer
      let offset = (16).to_int64() + (8).to_int64() * (i.to_int64() - 6)
      builder.emit(Mov(~src=Stack(offset), ~dst, ~ty=param_ty))
    }
  }

  for inst in self.body {
    builder.convert_inst!(inst)
  }

  let backend_symbols = {}

  // build the backend symbol table
  for name, entry in symbols {
    let backend_entry = match entry {
      Function(~ty=_, ~attributes=FunAttr(~global=_, ~defined)) => FunEntry(~is_defined=defined)
      Variable(~ty, ~attributes=StaticAttr(_)) => ObjEntry(~ty=convert_type!(ty), ~is_static=true)
      Variable(~ty, ~attributes=LocalAttr) => ObjEntry(~ty=convert_type!(ty), ~is_static=false)
      _ => raise AsmBuilderError::Unreachable("symbol table to entry not handled")
    }

    backend_symbols[name] = backend_entry
  }

  let pass1 = ReplacePseudoRegsPass::new(BackendSymbolTable(backend_symbols))
  let insts1 = match pass1.run?(builder.instructions) {
    Ok(insts) => insts
    Err(_) => raise AsmBuilderError::Unreachable("replace pseudo regs pass failed")
  }

  let aligned_stack_size = align_to_16_bytes(pass1.stack_offset.abs())

  // replace the placeholder with the actual stack allocation, aligned to 16 bytes
  insts1[0] = Binary(~op=Sub, ~src=Imm(aligned_stack_size), ~dst=Reg(SP), ~ty=QuadWord)

  let insts2 = FixUpPass::run(insts1)

  {
    name: self.name,
    global: self.global,
    body: insts2,
    stack_size: pass1.stack_offset,
  }
}

pub fn asm(self: TacProgram, symbols: Map[String, @validate.SymbolTableEntry]) -> AsmProgram!AsmBuilderError {
  let functions: Array[AsmFunctionDef] = []
  let static_vars: Array[AsmStaticVarDef] = []

  for item in self.top_level {
    match item {
      Function(function) => {
        functions.push(function.asm!(symbols))
      }
      StaticVar(~name, ~global, ~init, ~ty) => {
        let alignment = convert_type!(ty).alignment()
        static_vars.push({ name, global, init, alignment })
      } 
    }
  }

  { functions, static_vars }
}
