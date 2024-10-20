
pub struct Emitter {
  os: OperatingSystem
  lines: Array[String]
  symbols: Map[String, @validate.SymbolTableEntry]
  mut indentation: Int
}

pub type! EmitterError {
  UnexpectedPseudoReg(String)
  Unreachable(String)
} derive(Show)

pub fn Emitter::new(os: OperatingSystem, symbols: Map[String, @validate.SymbolTableEntry]) -> Emitter {
  { os, lines: [], indentation: 0, symbols }
}

fn symbol(self: OperatingSystem, name: String) -> String {
  match self {
    Linux => name
    MacOS => "_" + name
  }
}

fn align(self: OperatingSystem, n: Int64) -> String {
  match self {
    Linux => ".balign \{n}" // could also use .align directive
    MacOS => ".balign \{n}"
  }
}

fn label(self: OperatingSystem, name: String) -> String {
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

fn emit(self: Emitter, line: String) -> Unit {
  self.lines.push(repeat_string("  ", self.indentation) + line)
}

fn asm_reg_size(self: AsmOperand, size: RegSize, os: OperatingSystem) -> String!EmitterError {
  match self {
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
    }
    RegSize(reg, size) => Reg(reg).asm_reg_size!(size, os)
    Stack(offset) => "\{offset}(%rbp)"
    Pseudo(name) => raise UnexpectedPseudoReg(name)
    Data(name) => os.symbol(name) + "(%rip)"
  }
}

fn asm(self: AsmOperand, ty: AsmType, os: OperatingSystem) -> String!EmitterError {
  let size = match ty {
    LongWord => Long
    QuadWord => Quad
  }

  self.asm_reg_size!(size, os)
}

fn asm(self: AsmUnaryOp) -> String {
  match self {
    Neg => "neg"
    BitwiseNot => "not"
  }
}

fn asm(self: AsmBinaryOp) -> String {
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
  }
}

fn suffix(self: CondCode) -> String {
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

fn emit_inst(self: Emitter, inst: AsmInst) -> Unit!EmitterError {
  match inst {
    Mov(~src, ~dst, ~ty) => {
      self.emit("mov\{ty.suffix()} \{src.asm!(ty, self.os)}, \{dst.asm!(ty, self.os)}")
    }
    Movsx(~src, ~dst) => {
      self.emit("movslq \{src.asm!(LongWord, self.os)}, \{dst.asm!(QuadWord, self.os)}")
    }
    MovZeroExtend(_) => {
      raise EmitterError::Unreachable("MovZeroExtend should have been removed by the ReplacePseudoRegs pass")
    }
    Unary(~op, ~dst, ~ty) => {
      self.emit("\{op.asm()}\{ty.suffix()} \{dst.asm!(ty, self.os)}")
    }
    Binary(op=Shl | Sal as op, ~src, ~dst, ~ty) => {
      self.emit("\{op.asm()}\{ty.suffix()} \{src.asm!(ty, self.os)}, \{dst.asm!(ty, self.os)}")
    }
    Binary(~op, ~src, ~dst, ~ty) => {
      self.emit("\{op.asm()}\{ty.suffix()} \{src.asm!(ty, self.os)}, \{dst.asm!(ty, self.os)}")
    }
    Cmp(~src, ~dst, ~ty) => {
      self.emit("cmp\{ty.suffix()} \{src.asm!(ty, self.os)}, \{dst.asm!(ty, self.os)}")
    }
    IDiv(~src, ~ty) => {
      self.emit("idiv\{ty.suffix()} \{src.asm!(ty, self.os)}")
    }
    Div(~src, ~ty) => {
      self.emit("div\{ty.suffix()} \{src.asm!(ty, self.os)}")
    }
    Cdq(~ty) => {
      match ty {
        LongWord => self.emit("cdq")
        QuadWord => self.emit("cqo")
      }
    }
    Jmp(target) => {
      self.emit("jmp \{self.os.label(target)}")
    }
    JmpCond(~cond, ~target) => {
      self.emit("j\{cond.suffix()} \{self.os.label(target)}")
    }
    SetCond(~cond, ~dst) => {
      self.emit("set\{cond.suffix()} \{dst.asm_reg_size!(Byte, self.os)}")
    }
    Label(name) => {
      self.emit(self.os.label(name) + ":")
    }
    Push(operand) => {
      self.emit("pushq \{operand.asm!(QuadWord, self.os)}")
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

fn emit_function_def(self: Emitter, f: AsmFunctionDef) -> Unit!EmitterError {
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
    self.emit_inst!(inst)
  }

  self.indentation -= 1
}

fn show_initializer(init: @validate.StaticInit) -> String {
  match init {
    IntInit(n) => ".long \{n}"
    LongInit(n) => ".quad \{n}"
    UIntInit(n) => ".long \{n}"
    ULongInit(n) => ".quad \{n}"
  }
}

fn emit_static_variable_def(self: Emitter, var: AsmStaticVarDef) -> Unit {
  let { name, global, alignment, init } = var
  let name = self.os.symbol(name)

  if global {
    self.emit(".globl \{name}")
  }

  match init {
    IntInit(0) | LongInit(0) | UIntInit(0) | ULongInit(0) => {
      self.emit(".bss")
      self.emit(self.os.align(alignment))
      self.emit("\{name}:")
      
      let size = match init {
        IntInit(_) | UIntInit(_) => 4
        LongInit(_) | ULongInit(_) => 8
      }

      self.emit(".zero \{size}")
    }
    IntInit(_) | LongInit(_) | UIntInit(_) | ULongInit(_) => {
      self.emit(".data")
      self.emit(self.os.align(alignment))
      self.emit("\{name}:")
      self.emit(show_initializer(init))
    }
  }
}

pub fn emit_program(self: Emitter, program: AsmProgram) -> Unit!EmitterError {
  for v in program.static_vars {
    self.emit_static_variable_def(v)
  }

  for f in program.functions {
    self.emit_function_def!(f)
  }

  if self.os == Linux {
    self.emit(".section .note.GNU-stack, \"\",@progbits")
  }
}

pub impl Show for Emitter with output(self, logger: Logger) {
  for line in self.lines {
    logger.write_string(line)
    logger.write_char('\n')
  }
}
