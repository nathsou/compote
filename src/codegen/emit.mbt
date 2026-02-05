
pub struct Emitter {
  os: OperatingSystem
  lines: Array[String]
  symbols: Map[String, SymbolTableEntry]
  constants: Set[String]
  mut indentation: Int
}

pub suberror EmitterError {
  UnexpectedPseudoReg(String)
  Unreachable(String)
} derive(Show)

pub fn Emitter::new(os: OperatingSystem, symbols: Map[String, SymbolTableEntry]) -> Emitter {
  { os, lines: [], indentation: 0, symbols, constants: @set.new() }
}

fn OperatingSystem::symbol(self: OperatingSystem, name: String) -> String {
  match self {
    Linux => name
    MacOS => "_" + name
  }
}

fn OperatingSystem::align(self: OperatingSystem, n: Int64) -> String {
  match self {
    Linux => ".balign \{n}" // could also use .align directive
    MacOS => ".balign \{n}"
  }
}

fn OperatingSystem::label(self: OperatingSystem, name: String) -> String {
  match self {
    Linux => ".L" + name
    MacOS => "L" + name
  }
}

fn repeat_string(str: String, n: Int) -> String {
  if n == 0 {
    return ""
  }

  let mut result = str
  let mut current_count = 1

  while current_count * 2 <= n {
    result += result
    current_count *= 2
  }

  while current_count < n {
    result += str
    current_count += 1
  }

  result
}

fn Emitter::emit(self: Emitter, line: String) -> Unit {
  self.lines.push(repeat_string("  ", self.indentation) + line)
}

fn Emitter::asm_reg_size(self: Emitter, op: AsmOperand, size: RegSize) -> String raise EmitterError {
  match op {
    Imm(n) => "$\{n}"
    Reg(reg) => match reg {
      AX => match size {
        Byte => "%al"
        Long => "%eax"
        Quad => "%rax"
      }
      DX => match size {
        Byte => "%dl"
        Long => "%edx"
        Quad => "%rdx"
      }
      CX => match size {
        Byte => "%cl"
        Long => "%ecx"
        Quad => "%rcx"
      }
      DI => match size {
        Byte => "%dil"
        Long => "%edi"
        Quad => "%rdi"
      }
      SI => match size {
        Byte => "%sil"
        Long => "%esi"
        Quad => "%rsi"
      }
      R8 => match size {
        Byte => "%r8b"
        Long => "%r8d"
        Quad => "%r8"
      }
      R9 => match size {
        Byte => "%r9b"
        Long => "%r9d"
        Quad => "%r9"
      }
      R10 => match size {
        Byte => "%r10b"
        Long => "%r10d"
        Quad => "%r10"
      }
      R11 => match size {
        Byte => "%r11b"
        Long => "%r11d"
        Quad => "%r11"
      }
      SP => match size {
        Byte => "%spl"
        Long => "%esp"
        Quad => "%rsp"
      }
      XMM0 => "%xmm0"
      XMM1 => "%xmm1"
      XMM2 => "%xmm2"
      XMM3 => "%xmm3"
      XMM4 => "%xmm4"
      XMM5 => "%xmm5"
      XMM6 => "%xmm6"
      XMM7 => "%xmm7"
      XMM8 => "%xmm8"
      XMM9 => "%xmm9"
      XMM10 => "%xmm10"
      XMM11 => "%xmm11"
      XMM12 => "%xmm12"
      XMM13 => "%xmm13"
      XMM14 => "%xmm14"
      XMM15 => "%xmm15"
    }
    RegSize(reg, size) => self.asm_reg_size(Reg(reg), size)
    Stack(offset) => "\{offset}(%rbp)"
    Pseudo(name) => raise UnexpectedPseudoReg(name)
    Data(name) => {
      let target = if self.constants.contains(name) {
        self.os.label(name)
      } else {
        self.os.symbol(name)
      }

      "\{target}(%rip)"
    }
  }
}

fn Emitter::show_op(self: Emitter, op: AsmOperand, ty: AsmType) -> String raise EmitterError {
  let size = match ty {
    LongWord => Long
    QuadWord | Double => Quad
  }

  self.asm_reg_size(op, size)
}

fn AsmUnaryOp::mnemonic(self: AsmUnaryOp) -> String {
  match self {
    Neg => "neg"
    BitwiseNot => "not"
    Shr => "shr"
  }
}

fn AsmBinaryOp::mnemonic(self: AsmBinaryOp) -> String {
  match self {
    Add => "add"
    Sub => "sub"
    Mul => "imul"
    Shl => "shl"
    Sal => "sal"
    Sar => "sar"
    Shr => "shr"
    And => "and"
    Or => "or"
    Xor => "xor"
    DivDouble => "div"
  }
}

fn CondCode::suffix(self: CondCode) -> String {
  match self {
    Eq => "e"
    Ne => "ne"
    Lt => "l"
    Le => "le"
    Gt => "g"
    Ge => "ge"
    Above => "a"
    AboveEq => "ae"
    Below => "b"
    BelowEq => "be"
  }
}

fn Emitter::emit_inst(self: Emitter, inst: AsmInst) -> Unit raise EmitterError {
  match inst {
    Mov(src~, dst~, ty~) => {
      self.emit("mov\{ty.suffix()} \{self.show_op(src, ty)}, \{self.show_op(dst, ty)}")
    }
    Movsx(src~, dst~) => {
      self.emit("movslq \{self.show_op(src, LongWord)}, \{self.show_op(dst, QuadWord)}")
    }
    MovZeroExtend(_) => {
      raise EmitterError::Unreachable("MovZeroExtend should have been removed by the ReplacePseudoRegs pass")
    }
    Unary(op~, dst~, ty~) => {
      self.emit("\{op.mnemonic()}\{ty.suffix()} \{self.show_op(dst, ty)}")
    }
    Binary(op=Shl | Sal as op, src~, dst~, ty~) => {
      self.emit("\{op.mnemonic()}\{ty.suffix()} \{self.show_op(src, ty)}, \{self.show_op(dst, ty)}")
    }
    Binary(op=Xor, src~, dst~, ty=Double) => {
      self.emit("xorpd \{self.show_op(src, Double)}, \{self.show_op(dst, Double)}")
    }
    Binary(op=Mul, src~, dst~, ty=Double) => {
      self.emit("mulsd \{self.show_op(src, Double)}, \{self.show_op(dst, Double)}")
    }
    Binary(op~, src~, dst~, ty~) => {
      self.emit("\{op.mnemonic()}\{ty.suffix()} \{self.show_op(src, ty)}, \{self.show_op(dst, ty)}")
    }
    Cmp(src~, dst~, ty=Double) => {
      self.emit("comisd \{self.show_op(src, Double)}, \{self.show_op(dst, Double)}")
    }
    Cmp(src~, dst~, ty~) => {
      self.emit("cmp\{ty.suffix()} \{self.show_op(src, ty)}, \{self.show_op(dst, ty)}")
    }
    IDiv(src~, ty~) => {
      self.emit("idiv\{ty.suffix()} \{self.show_op(src, ty)}")
    }
    Div(src~, ty~) => {
      self.emit("div\{ty.suffix()} \{self.show_op(src, ty)}")
    }
    Cdq(ty~) => {
      match ty {
        LongWord => self.emit("cdq")
        QuadWord => self.emit("cqo")
        Double => raise EmitterError::Unreachable("Cdq should not be used with Double type")
      }
    }
    Cvtsi2sd(src_ty~, src~, dst~) => {
      self.emit("cvtsi2sd\{src_ty.suffix()} \{self.show_op(src, src_ty)}, \{self.show_op(dst, Double)}")
    }
    Cvttsd2si(dst_ty~, src~, dst~) => {
      self.emit("cvttsd2si\{dst_ty.suffix()} \{self.show_op(src, Double)}, \{self.show_op(dst, dst_ty)}")
    }
    Jmp(target) => { 
      self.emit("jmp \{self.os.label(target)}")
    }
    JmpCond(cond~, target~) => {
      self.emit("j\{cond.suffix()} \{self.os.label(target)}")
    }
    SetCond(cond~, dst~) => {
      self.emit("set\{cond.suffix()} \{self.asm_reg_size(dst, Byte)}")
    }
    Label(name) => {
      self.emit(self.os.label(name) + ":")
    }
    Push(operand) => {
      self.emit("pushq \{self.show_op(operand, QuadWord)}")
    }
    Call(label) => {
      let suffix = match self.os {
        Linux => {
          // add Procedure Linkage Table suffix for external functions on Linux
          match self.symbols.get(label) {
            Some(_) => ""
            _ => "@PLT"
          }
        }
        MacOS => ""
      }

      self.emit("call \{self.os.symbol(label)}\{suffix}")
    }
    Ret => {
      // function epilogue
      self.emit("movq %rbp, %rsp")
      self.emit("popq %rbp")
      self.emit("ret")
    }
  }
}

fn Emitter::emit_function_def(self: Emitter, f: AsmFunctionDef) -> Unit raise EmitterError {
  let name = self.os.symbol(f.name)

  if f.global {
    self.emit(".globl \{name}")
  }

  self.emit(".text")
  self.emit("\{name}:")
  self.indentation += 1

  // function prologue
  self.emit("pushq %rbp")
  self.emit("movq %rsp, %rbp")

  for inst in f.body {
    self.emit_inst(inst)
  }

  self.indentation -= 1
}

fn show_initializer(init: StaticInit) -> String {
  match init {
    IntInit(n) => ".long \{n}"
    LongInit(n) => ".quad \{n}"
    UIntInit(n) => ".long \{n}"
    ULongInit(n) => ".quad \{n}"
    DoubleInit(x) => ".quad \{x.reinterpret_as_uint64()}"
  }
}

fn Emitter::emit_static_variable_def(self: Emitter, defn: AsmStaticVarDef) -> Unit {
  let { name, global, alignment, init } = defn
  let name = self.os.symbol(name)

  if global {
    self.emit(".globl \{name}")
  }

  match init {
    IntInit(0) | LongInit(0) | UIntInit(0) | ULongInit(0) | DoubleInit(0.0) => {
      self.emit(".bss")
      self.emit(self.os.align(alignment))
      self.emit("\{name}:")
      
      let size = match init {
        IntInit(_) | UIntInit(_) => 4
        LongInit(_) | ULongInit(_) | DoubleInit(_) => 8
      }

      self.emit(".zero \{size}")
    }
    IntInit(_) | LongInit(_) | UIntInit(_) | ULongInit(_) | DoubleInit(_) => {
      self.emit(".data")
      self.emit(self.os.align(alignment))
      self.emit("\{name}:")
      self.emit(show_initializer(init))
    }
  }
}

fn Emitter::emit_static_const_def(self: Emitter, defn: AsmStaticConstDef) -> Unit {
  self.constants.add(defn.name)

  match self.os {
    Linux => {
      self.emit(".section .rodata")
      self.emit(self.os.align(defn.alignment))
      self.emit("\{self.os.label(defn.name)}:")
      self.emit(show_initializer(defn.init))
    }
    MacOS => {
      match defn.alignment {
        16 => {
          self.emit(".literal16")
          self.emit(self.os.align(16))
          self.emit("\{self.os.label(defn.name)}:")
          self.emit(show_initializer(defn.init))
          self.emit(".quad 0")
        }
        _ => {
          self.emit(".literal8")
          self.emit(self.os.align(8))
          self.emit("\{self.os.label(defn.name)}:")
          self.emit(show_initializer(defn.init))
        }
      }
    }
  }
}

pub fn Emitter::emit_program(self: Emitter, program: AsmProgram) -> Unit raise EmitterError {
  for v in program.static_vars {
    self.emit_static_variable_def(v)
  }

  for c in program.static_consts {
    self.emit_static_const_def(c)
  }

  for f in program.functions {
    self.emit_function_def(f)
  }

  if self.os == Linux {
    self.emit(".section .note.GNU-stack, \"\",@progbits")
  }
}

pub impl Show for Emitter with output(self, logger: &Logger) {
  for line in self.lines {
    logger.write_string(line)
    logger.write_char('\n')
  }
}
