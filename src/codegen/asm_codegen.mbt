

pub struct AsmBuilder {
  instructions: Array[AsmInst]
  symbols: Map[String, SymbolTableEntry]
  static_consts: Array[AsmStaticConstDef]
}

pub suberror AsmBuilderError {
  UneachableUnaryOpConversion(@parse.UnaryOp)
  UnreachableBinaryOpConversion(@parse.BinaryOp)
  UnreachableCondCodeConversion(@parse.BinaryOp)
  Unreachable(String)
} derive(Show)

pub fn AsmBuilder::new(symbols: Map[String, SymbolTableEntry], static_consts: Array[AsmStaticConstDef]) -> AsmBuilder {
  { instructions: [], symbols, static_consts }
}

fn AsmBuilder::emit(self: AsmBuilder, inst: AsmInst) -> Unit {
  self.instructions.push(inst)
}

fn AsmBuilder::emit_static_const(self: AsmBuilder, init: @validate.StaticInit, alignment~: Int64) -> String {
  for const_def in self.static_consts {
    if const_def.init == init && const_def.alignment == alignment {
      return const_def.name
    }
  }

  let name = "static_const_\{self.static_consts.length()}"
  self.static_consts.push({ name, alignment, init })
  name
}

test "AsmBuilder::emit_static_const" {
  let builder = AsmBuilder::new({}, [])
  let name1 = builder.emit_static_const(DoubleInit(3.14), alignment=16)
  let name2 = builder.emit_static_const(DoubleInit(3.14), alignment=16)
  let name3 = builder.emit_static_const(DoubleInit(0.0), alignment=16)
  let name4 = builder.emit_static_const(DoubleInit(-0.0), alignment=16)
  let name5 = builder.emit_static_const(DoubleInit(3.14), alignment=8)

  assert_eq(name1, name2)
  assert_not_eq(name3, name4)
  assert_not_eq(name1, name5)
}

fn @parse.UnaryOp::to_asm(self: @parse.UnaryOp) -> AsmUnaryOp raise AsmBuilderError {
  match self {
    Neg => Neg
    Complement => BitwiseNot
    Not | Dec(_) | Inc(_) => raise UneachableUnaryOpConversion(self)
  }
}

fn convert_binary_op(op: @parse.BinaryOp, is_signed~: Bool) -> AsmBinaryOp raise AsmBuilderError {
  match op {
    Add => Add
    Sub => Sub
    Mul => Mul
    BitwiseAnd => And
    BitwiseOr => Or
    BitwiseXor => Xor
    ShiftLeft => if is_signed { Sal } else { Shl }
    ShiftRight => if is_signed { Sar } else { Shr }
    Div => DivDouble
    Rem | Equ | Neq | Lss | Leq | Gtr | Geq |
    LogicalAnd | LogicalOr | Assignment(_) | Ternary => {
      raise UnreachableBinaryOpConversion(op)
    }
  }
}

fn convert_cond_code(op: @parse.BinaryOp, is_signed~: Bool, is_double~: Bool) -> CondCode raise AsmBuilderError {
  let signed_not_double = is_signed && !is_double

  match op {
    Equ => Eq
    Neq => Ne
    Gtr => if signed_not_double { Gt } else { Above }
    Geq => if signed_not_double { Ge } else { AboveEq }
    Lss => if signed_not_double { Lt } else { Below }
    Leq => if signed_not_double { Le } else { BelowEq }
    _ => raise UnreachableCondCodeConversion(op)
  }
}

fn AsmBuilder::convert_val(self: AsmBuilder, op: TacValue) -> AsmOperand {
  match op {
    Constant(IntConst(n)) => Imm(n.to_int64())
    Constant(LongConst(n)) => Imm(n)
    Constant(UnsignedIntConst(n)) => Imm(n.to_int64())
    Constant(UnsignedLongConst(n)) => Imm(n.reinterpret_as_int64())
    Constant(DoubleConst(d)) => {
      let const_name = self.emit_static_const(DoubleInit(d), alignment=8)
      Data(const_name)
    }
    Variable(name) => Pseudo(name)
  }
}

fn convert_type(ty: Type) -> AsmType raise AsmBuilderError {
  match ty {
    Int | UnsignedInt => LongWord
    Long | UnsignedLong => QuadWord
    Double => Double
    FunType(_) => raise AsmBuilderError::Unreachable("converting function type to AsmType")
  }
}

fn AsmBuilder::tacky_ty(self: AsmBuilder, value: TacValue) -> Type raise AsmBuilderError {
  match value {
    Constant(c) => c.ty()
    Variable(name) => {
      match self.symbols.get(name) {
        Some(entry) => entry.ty()
        None => raise AsmBuilderError::Unreachable("variable not found in symbol table")
      }
    }
  }
}

fn AsmBuilder::asm_ty(self: AsmBuilder, value: TacValue) -> AsmType raise AsmBuilderError {
  convert_type(self.tacky_ty(value))
}

fn AsmBuilder::convert_inst(self: AsmBuilder, inst: TacInst) -> Unit raise AsmBuilderError {
  match inst {
    Return(value) => {
      let src = self.convert_val(value)
      let ty = self.asm_ty(value)
      let ret_reg = match ty {
        LongWord | QuadWord => AX
        Double => XMM0
      }

      self.emit(Mov(src~, dst=Reg(ret_reg), ty~))
      self.emit(Ret)
    }
    SignExtend(src~, dst~) => {
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      self.emit(Movsx(src~, dst~))
    }
    Truncate(src~, dst~) => {
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      self.emit(Mov(src~, dst~, ty=LongWord))
    }
    ZeroExtend(src~, dst~) => {
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      self.emit(MovZeroExtend(src~, dst~))
    }
    Unary(op=Not, src~, dst~) => {
      let src_ty = self.asm_ty(src)
      let dst_ty = self.asm_ty(dst)
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)

      match src_ty {
        LongWord | QuadWord => {
          self.emit(Cmp(src=Imm(0), dst=src, ty=src_ty))
          // clear the destination register before the set instruction
          self.emit(Mov(src=Imm(0), dst~, ty=dst_ty))
          self.emit(SetCond(cond=Eq, dst~))
        }
        Double => {
          self.emit(Binary(op=Xor, src=Reg(XMM0), dst=Reg(XMM0), ty=Double))
          self.emit(Cmp(src~, dst=Reg(XMM0), ty=Double))
          self.emit(Mov(src=Imm(0), dst~, ty=dst_ty))
          self.emit(SetCond(cond=Eq, dst~))
        }
      }
    }
    Unary(op=Neg, src~, dst~) if self.asm_ty(src) is Double => {
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      let neg_zero_const = self.emit_static_const(DoubleInit(-0.0), alignment=16)
      self.emit(Mov(src~, dst~, ty=Double))
      self.emit(Binary(op=Xor, src=Data(neg_zero_const), dst~, ty=Double))
    }
    Unary(op~, src~, dst~) => {
      let src_ty = self.asm_ty(src)
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      self.emit(Mov(src~, dst~, ty=src_ty))
      self.emit(Unary(op=op.to_asm(), dst~, ty=src_ty))
    }
    Binary(op~, lhs~, rhs~, dst~) => {
      let is_signed = self.tacky_ty(lhs).is_signed()
      let lhs_ty = self.asm_ty(lhs)
      let dst_ty = self.asm_ty(dst)
      let lhs = self.convert_val(lhs)
      let rhs = self.convert_val(rhs)
      let dst = self.convert_val(dst)
      
      match op {
        Equ | Neq | Gtr | Geq | Lss | Leq => {
          self.emit(Cmp(src=rhs, dst=lhs, ty=lhs_ty))
          // clear the destination register before the set instruction
          self.emit(Mov(src=Imm(0), dst~, ty=dst_ty))
          self.emit(SetCond(cond=convert_cond_code(op, is_signed~, is_double=lhs_ty is Double), dst~))
        }
        (Div | Rem) if lhs_ty != Double => {
          let output_reg = if op == Div { AX } else { DX }

          if is_signed {
            self.emit(Mov(src=lhs, dst=Reg(AX), ty=lhs_ty))
            self.emit(Cdq(ty=lhs_ty))
            self.emit(IDiv(src=rhs, ty=lhs_ty))
            self.emit(Mov(src=Reg(output_reg), dst~, ty=lhs_ty))
          } else {
            self.emit(Mov(src=lhs, dst=Reg(AX), ty=lhs_ty))
            self.emit(Mov(src=Imm(0), dst=Reg(DX), ty=lhs_ty))
            self.emit(Div(src=rhs, ty=lhs_ty))
            self.emit(Mov(src=Reg(output_reg), dst~, ty=lhs_ty))
          }
        }
        ShiftLeft | ShiftRight => {
          match rhs {
            Imm(_) => {
              self.emit(Mov(src=lhs, dst~, ty=lhs_ty))
              self.emit(Binary(op=convert_binary_op(op, is_signed~), src=rhs, dst~, ty=lhs_ty))
            }
            // if the shift amount is in memory, load it into the CL register
            _ => {
              self.emit(Mov(src=lhs, dst~, ty=lhs_ty))
              self.emit(Mov(src=rhs, dst=Reg(CX), ty=LongWord))
              self.emit(Binary(op=convert_binary_op(op, is_signed~), src=RegSize(CX, Byte), dst~, ty=lhs_ty))
            }
          }
        }
        _ => {
          self.emit(Mov(src=lhs, dst~, ty=lhs_ty))
          self.emit(Binary(op=convert_binary_op(op, is_signed~), src=rhs, dst~, ty=lhs_ty))
        }
      }
    }
    Jump(target) => {
      self.emit(Jmp(target))
    }
    JumpIfZero(cond~, target~) | JumpIfNotZero(cond~, target~) => {
      let cond_ty = self.asm_ty(cond)
      let cond = self.convert_val(cond)
      let cond_code = if inst is JumpIfZero(..) { Eq } else { Ne }

      match cond_ty {
        LongWord | QuadWord => {
          self.emit(Cmp(src=Imm(0), dst=cond, ty=cond_ty))
          self.emit(JmpCond(cond=cond_code, target~))
        }
        Double => {
          self.emit(Binary(op=Xor, src=Reg(XMM0), dst=Reg(XMM0), ty=Double))
          self.emit(Cmp(src=cond, dst=Reg(XMM0), ty=Double))
          self.emit(JmpCond(cond=cond_code, target~))
        }
      }
    }
    Copy(src~, dst~)  => {
      let ty = self.asm_ty(src)
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      self.emit(Mov(src~, dst~, ty~))
    }
    Label(name) => {
      self.emit(Label(name))
    }
    Call(name~, args~, dst~) => {
      let regs = get_arg_regs()
      let classified = self.classify_parameters(args)
      
      // stack align to 16 bytes
      let stack_padding: Int64 = if (classified.stack_args.length() & 1) == 1 { 8 } else { 0 }

      if stack_padding > 0 {
        self.emit(Binary(op=Sub, src=Imm(stack_padding), dst=Reg(SP), ty=QuadWord))
      }

      // pass first 6 int arguments in registers
      for i, ty_op in classified.int_reg_args {
        let (ty, arg) = ty_op
        self.emit(Mov(src=arg, dst=Reg(regs.int_regs[i]), ty~))
      }

      // pass first 8 double arguments in registers
      for i, ty_op in classified.double_reg_args {
        let (ty, arg) = ty_op
        self.emit(Mov(src=arg, dst=Reg(regs.double_regs[i]), ty~))
      }

      // push the remaining arguments onto the stack in reverse order
      for ty_op in classified.stack_args.rev_iter() {
        let (ty, arg) = ty_op
        match arg {
          Reg(_) | Imm(_) => {
            self.emit(Push(arg))
          }
          _ => {
            match ty {
              QuadWord | Double => {
                self.emit(Push(arg))
              }
              LongWord => {
                // if the argument is 4-bytes in memory, load it into a register first before pushing it
                self.emit(Mov(src=arg, dst=Reg(AX), ty~))
                self.emit(Push(Reg(AX)))
              }
            }
          }
        }
      }

      self.emit(Call(name))

      let bytes_to_remove = (8).to_int64() * classified.stack_args.length().to_int64() + stack_padding

      if bytes_to_remove > 0 {
        self.emit(Binary(op=Add, src=Imm(bytes_to_remove), dst=Reg(SP), ty=QuadWord))
      }

      // retrieve the return value
      let dst_ty = self.asm_ty(dst)
      let dst = self.convert_val(dst)
      let return_reg = if dst_ty == Double { XMM0 } else { AX }
      self.emit(Mov(src=Reg(return_reg), dst~, ty=dst_ty))
    }
    IntToDouble(src~, dst~) => {
      let src_ty = self.asm_ty(src)
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      self.emit(Cvtsi2sd(src~, dst~, src_ty~))
    }
    DoubleToInt(src~, dst~) => {
      let dst_ty = self.asm_ty(dst)
      let src = self.convert_val(src)
      let dst = self.convert_val(dst)
      self.emit(Cvttsd2si(src~, dst~, dst_ty~))
    }
    UIntToDouble(src=tac_src, dst~) => {
      let src = self.convert_val(tac_src)
      let dst = self.convert_val(dst)

      if self.tacky_ty(tac_src) == UnsignedInt {
        // UInt (32-bit) fits in signed int64, so zero-extend and convert
        self.emit(MovZeroExtend(src~, dst=Reg(R9)))
        self.emit(Cvtsi2sd(src=Reg(R9), dst~, src_ty=QuadWord))
      } else {
        // ULong (64-bit): need special handling for values >= 2^63
        let label1 = "_uint_to_double_pos_\{self.instructions.length()}"
        let label2 = "_uint_to_double_end_\{self.instructions.length()}"
        self.emit(Cmp(src=Imm(0), dst=src, ty=QuadWord))
        self.emit(JmpCond(cond=Lt, target=label1))
        self.emit(Cvtsi2sd(src~, dst~, src_ty=QuadWord))
        self.emit(Jmp(label2))
        self.emit(Label(label1))
        self.emit(Mov(src~, dst=Reg(R8), ty=QuadWord))
        self.emit(Mov(src=Reg(R8), dst=Reg(R9), ty=QuadWord))
        self.emit(Unary(op=Shr, dst=Reg(R9), ty=QuadWord))
        self.emit(Binary(op=And, src=Imm(1), dst=Reg(R8), ty=QuadWord))
        self.emit(Binary(op=Or, src=Reg(R8), dst=Reg(R9), ty=QuadWord))
        self.emit(Cvtsi2sd(src=Reg(R9), dst~, src_ty=QuadWord))
        self.emit(Binary(op=Add, src=dst, dst~, ty=Double))
        self.emit(Label(label2))
      }
    }
    DoubleToUInt(src~, dst=tac_dst) => {
      let src = self.convert_val(src)
      let dst = self.convert_val(tac_dst)

      if self.tacky_ty(tac_dst) == UnsignedInt {
        // UInt (32-bit): convert to signed int64, then truncate to 32-bit
        self.emit(Cvttsd2si(src~, dst=Reg(R9), dst_ty=QuadWord))
        self.emit(Mov(src=Reg(R9), dst~, ty=LongWord))
      } else {
        // ULong (64-bit): need special handling for values >= 2^63
        let r = R9
        let x = XMM7
        let upper_bound_const = self.emit_static_const(DoubleInit(9223372036854775808.0), alignment=8)
        let label1 = "_double_to_uint_pos_\{self.instructions.length()}"
        let label2 = "_double_to_uint_end_\{self.instructions.length()}"
        self.emit(Cmp(src=Data(upper_bound_const), dst=src, ty=Double))
        self.emit(JmpCond(cond=AboveEq, target=label1))
        self.emit(Cvttsd2si(src~, dst~, dst_ty=QuadWord))
        self.emit(Jmp(label2))
        self.emit(Label(label1))
        self.emit(Mov(src~, dst=Reg(x), ty=Double))
        self.emit(Binary(op=Sub, src=Data(upper_bound_const), dst=Reg(x), ty=Double))
        self.emit(Cvttsd2si(src=Reg(x), dst~, dst_ty=QuadWord))
        self.emit(Mov(src=Imm((9223372036854775808 : UInt64).reinterpret_as_int64()), dst=Reg(r), ty=QuadWord))
        self.emit(Binary(op=Add, src=Reg(r), dst~, ty=QuadWord))
        self.emit(Label(label2))
      }
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

priv struct ClassifiedParams {
  int_reg_args: Array[(AsmType, AsmOperand)]
  double_reg_args: Array[(AsmType, AsmOperand)]
  stack_args: Array[(AsmType, AsmOperand)]
}

fn AsmBuilder::classify_parameters(self: AsmBuilder, values: Array[TacValue]) -> ClassifiedParams raise AsmBuilderError {
  let int_reg_args: Array[(AsmType, AsmOperand)] = []
  let double_reg_args: Array[(AsmType, AsmOperand)] = []
  let stack_args: Array[(AsmType, AsmOperand)] = []

  for val in values {
    let operand = self.convert_val(val)
    let ty = self.asm_ty(val)

    match ty {
      Double => {
        if double_reg_args.length() < 8 {
          double_reg_args.push((ty, operand))
        } else {
          stack_args.push((ty, operand))
        }
      }
      LongWord | QuadWord => {
        if int_reg_args.length() < 6 {
          int_reg_args.push((ty, operand))
        } else {
          stack_args.push((ty, operand))
        }
      }
    }
  }

  { int_reg_args, double_reg_args, stack_args }
}

priv struct ArgRegs {
  int_regs: Array[AsmReg]
  double_regs: Array[AsmReg]
}

fn get_arg_regs() -> ArgRegs {
  let int_regs = [DI, SI, DX, CX, R8, R9]
  let double_regs = [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]
  { int_regs, double_regs }
}

fn AsmBuilder::set_up_parameters(self: AsmBuilder, values: Array[TacValue]) -> Unit raise AsmBuilderError {
  let classified = self.classify_parameters(values)
  let regs = get_arg_regs()

  // copy first 6 integer arguments from registers to local variables
  for i, ty_op in classified.int_reg_args {
    let (ty, operand) = ty_op
    self.emit(Mov(src=Reg(regs.int_regs[i]), dst=operand, ty~))
  }

  // copy first 8 double arguments from registers to local variables
  for i, ty_op in classified.double_reg_args {
    let (ty, operand) = ty_op
    self.emit(Mov(src=Reg(regs.double_regs[i]), dst=operand, ty~))
  }

  // copy remaining arguments from stack to local variables
  let mut offset: Int64 = 16

  for ty_op in classified.stack_args {
    let (ty, operand) = ty_op
    self.emit(Mov(src=Stack(offset), dst=operand, ty~))
    offset += 8
  }
}

pub fn TacFunctionDef::to_asm(
  self: TacFunctionDef,
  symbols: Map[String, SymbolTableEntry],
  static_consts: Array[AsmStaticConstDef],
) -> AsmFunctionDef raise AsmBuilderError {
  let builder = AsmBuilder::new(symbols, static_consts)

  // placeholder for stack allocation
  builder.emit(Binary(op=Sub, src=Imm(0), dst=Reg(SP), ty=QuadWord))

  // copy arguments to registers/stack
  builder.set_up_parameters(self.params.map(p => Variable(p)))

  for inst in self.body {
    builder.convert_inst(inst)
  }

  let backend_symbols = {}

  // build the backend symbol table
  for name, entry in symbols {
    let backend_entry = match entry {
      Function(ty=_, attributes=FunAttr(global=_, defined~)) => FunEntry(is_defined=defined)
      Variable(ty~, attributes=StaticAttr(_)) => ObjEntry(ty=convert_type(ty), is_static=true, is_constant=false)
      Variable(ty~, attributes=LocalAttr) => ObjEntry(ty=convert_type(ty), is_static=false, is_constant=false)
      _ => raise AsmBuilderError::Unreachable("symbol table to entry not handled")
    }

    backend_symbols[name] = backend_entry
  }

  let pass1 = ReplacePseudoRegsPass::new(BackendSymbolTable(backend_symbols))
  let insts1 = try pass1.run(builder.instructions) catch {
    _ => raise AsmBuilderError::Unreachable("replace pseudo regs pass failed")
  }

  let aligned_stack_size = align_to_16_bytes(pass1.stack_offset.abs())

  // replace the placeholder with the actual stack allocation, aligned to 16 bytes
  insts1[0] = Binary(op=Sub, src=Imm(aligned_stack_size), dst=Reg(SP), ty=QuadWord)

  let insts2 = fixup_run(insts1)

  {
    name: self.name,
    global: self.global,
    body: insts2,
    stack_size: pass1.stack_offset,
  }
}

pub fn TacProgram::to_asm(self: TacProgram, symbols: Map[String, SymbolTableEntry]) -> AsmProgram raise AsmBuilderError {
  let functions: Array[AsmFunctionDef] = []
  let static_vars: Array[AsmStaticVarDef] = []
  let static_consts: Array[AsmStaticConstDef] = []

  for item in self.top_level {
    match item {
      Function(function) => {
        functions.push(function.to_asm(symbols, static_consts))
      }
      StaticVar(name~, global~, init~, ty~) => {
        let alignment = convert_type(ty).alignment()
        static_vars.push({ name, global, init, alignment })
      }
      StaticConst(name~, init~, ty~) => {
        let alignment = convert_type(ty).alignment()
        static_consts.push({ name, init, alignment })
      }
    }
  }

  { functions, static_vars, static_consts }
}
