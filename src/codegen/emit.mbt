
pub struct Emitter {
  os: OperatingSystem
  lines: Array[String]
  mut indentation: Int
}

pub type! EmitterError {
  UnexpectedPseudoReg(String)
} derive(Show)

pub fn Emitter::new(os: OperatingSystem) -> Emitter {
  { os, lines: [], indentation: 0 }
}

fn symbol(self: OperatingSystem, name: String) -> String {
  match self {
    Linux => name
    MacOS => "_" + name
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

fn asm(self: AsmOperand, size: RegSize) -> String!EmitterError {
  match self {
    Imm(n) => "$\{n}"
    Reg(reg) => match reg {
      AX => match size {
        Byte => "%al"
        Long => "%eax"
      }
      DX => match size {
        Byte => "%dl"
        Long => "%edx"
      }
      R10 => match size {
        Byte => "%r10b"
        Long => "%r10d"
      }
      R11 => match size {
        Byte => "%r11b"
        Long => "%r11d"
      }
      CL => match size {
        Byte => "%cl"
        Long => "%ecx"
      }
    }
    RegSize(reg, size) => Reg(reg).asm!(size)
    Stack(offset) => "\{offset}(%rbp)"
    Pseudo(name) => raise UnexpectedPseudoReg(name)
  }
}

fn asm(self: AsmUnaryOp) -> String {
  match self {
    Neg => "negl"
    BitwiseNot => "notl"
  }
}

fn asm(self: AsmBinaryOp) -> String {
  match self {
    Add => "addl"
    Sub => "subl"
    Mul => "imul"
    Shl => "shll"
    Sal => "sarl"
    And => "andl"
    Or => "orl"
    Xor => "xorl"
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
  }
}

fn emit_inst(self: Emitter, inst: AsmInst) -> Unit!EmitterError {
  match inst {
    Mov(~src, ~dst) => {
      self.emit("movl \{src.asm!(Long)}, \{dst.asm!(Long)}")
    }
    Unary(~op, ~dst) => {
      self.emit("\{op.asm()} \{dst.asm!(Long)}")
    }
    Binary(~op=Shl | Sal as op, ~src, ~dst) => {
      self.emit("\{op.asm()} \{src.asm!(Long)}, \{dst.asm!(Byte)}")
    }
    Binary(~op, ~src, ~dst) => {
      self.emit("\{op.asm()} \{src.asm!(Long)}, \{dst.asm!(Long)}")
    }
    Cmp(~src, ~dst) => {
      self.emit("cmpl \{src.asm!(Long)}, \{dst.asm!(Long)}")
    }
    IDiv(~src) => {
      self.emit("idivl \{src.asm!(Long)}")
    }
    Cdq => {
      self.emit("cdq")
    }
    Jmp(target) => {
      self.emit("jmp \{self.os.label(target)}")
    }
    JmpCond(~cond, ~target) => {
      self.emit("j\{cond.suffix()} \{self.os.label(target)}")
    }
    SetCond(~cond, ~dst) => {
      self.emit("set\{cond.suffix()} \{dst.asm!(Byte)}")
    }
    Label(name) => {
      self.emit(self.os.label(name) + ":")
    }
    AllocateStack(~size) => {
      self.emit("subq $\{size.abs()}, %rsp")
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
  self.emit(".globl \{name}")
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

pub fn emit_program(self: Emitter, program: AsmProgram) -> Unit!EmitterError {
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
