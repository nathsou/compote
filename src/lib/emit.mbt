
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

fn asm(self: AsmOperand) -> String!EmitterError {
  match self {
    Imm(n) => "$\{n}"
    Reg(reg) => match reg {
      AX => "%eax"
      DX => "%edx"
      R10 => "%r10d"
      R11 => "%r11d"
      ECX => "%ecx"
      CL => "%cl"
    }
    Stack(offset) => "\{offset}(%rbp)"
    Pseudo(name) => raise UnexpectedPseudoReg(name)
  }
}

fn asm(self: AsmUnaryOp) -> String {
  match self {
    Neg => "negl"
    Not => "notl"
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

fn emit_inst(self: Emitter, inst: AsmInst) -> Unit!EmitterError {
  match inst {
    Mov(~src, ~dst) => {
      self.emit("movl \{src.asm!()}, \{dst.asm!()}")
    }
    Unary(~op, ~dst) => {
      self.emit("\{op.asm()} \{dst.asm!()}")
    }
    Binary(~op, ~src, ~dst) => {
      self.emit("\{op.asm()} \{src.asm!()}, \{dst.asm!()}")
    }
    IDiv(~src) => {
      self.emit("idivl \{src.asm!()}")
    }
    Cdq => {
      self.emit("cdq")
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
