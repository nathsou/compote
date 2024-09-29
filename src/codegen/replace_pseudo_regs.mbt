pub struct ReplacePseudoRegsPass {
  pseudo_reg_offsets: Map[String, Int]
  symbols: Map[String, @validate.SymbolTableEntry]
  mut stack_offset: Int
}

pub fn ReplacePseudoRegsPass::new(symbols: Map[String, @validate.SymbolTableEntry]) -> ReplacePseudoRegsPass {
  { pseudo_reg_offsets: {}, symbols, stack_offset: 0 }
}

fn rewrite_operand(self: ReplacePseudoRegsPass, operand: AsmOperand) -> AsmOperand {
  match operand {
    Pseudo(name) => {
      let is_static = match self.symbols.get(name) {
        Some(Variable(~ty=_, ~attributes=StaticAttr(_))) => true
        _ => false
      }

      if is_static {
        Data(name)
      } else {
        let offset = match self.pseudo_reg_offsets.get(name) {
          Some(offset) => offset
          None => {
            self.stack_offset -= 4
            self.pseudo_reg_offsets.set(name, self.stack_offset)
            self.stack_offset
          }
        }
        
        Stack(offset)
      }

    }
    _ => operand
  }
}

fn rewrite_inst(self: ReplacePseudoRegsPass, inst: AsmInst) -> AsmInst {
  match inst {
    Mov(~src, ~dst) => {
      let src = self.rewrite_operand(src)
      let dst = self.rewrite_operand(dst)
      Mov(~src, ~dst)
    }
    Unary(~op, ~dst) => {
      let dst = self.rewrite_operand(dst)
      Unary(~op, ~dst)
    }
    Binary(~op, ~src, ~dst) => {
      let src = self.rewrite_operand(src)
      let dst = self.rewrite_operand(dst)
      Binary(~op, ~src, ~dst)
    }
    Cmp(~src, ~dst) => {
      let src = self.rewrite_operand(src)
      let dst = self.rewrite_operand(dst)
      Cmp(~src, ~dst)
    }
    IDiv(~src) => {
      let src = self.rewrite_operand(src)
      IDiv(~src)
    }
    Cdq => Cdq
    Jmp(target) => Jmp(target)
    JmpCond(~cond, ~target) => JmpCond(~cond, ~target)
    SetCond(~cond, ~dst) => {
      let dst = self.rewrite_operand(dst)
      SetCond(~cond, ~dst)
    }
    Label(name) => Label(name)
    AllocateStack(~size) => AllocateStack(~size)
    DeallocateStack(~size) => DeallocateStack(~size)
    Push(operand) => {
      let operand = self.rewrite_operand(operand)
      Push(operand)
    }
    Call(name) => Call(name)
    Ret => Ret
  }
}

pub fn run(self: ReplacePseudoRegsPass, insts: Array[AsmInst]) -> Array[AsmInst] {
  insts.map(fn(inst) { self.rewrite_inst(inst) })
}
