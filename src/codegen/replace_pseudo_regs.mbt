pub struct ReplacePseudoRegsPass {
  pseudo_reg_offsets: Map[String, Int64]
  symbols: BackendSymbolTable
  mut stack_offset: Int64
}

pub fn ReplacePseudoRegsPass::new(symbols: BackendSymbolTable) -> ReplacePseudoRegsPass {
  { pseudo_reg_offsets: {}, symbols, stack_offset: 0 }
}

fn round_aligned(alignment: Int64, n: Int64) -> Int64 {
  if n % alignment == 0 {
    n
  } else if n < 0 {
    n - alignment - (n % alignment)
  } else {
    n + alignment - (n % alignment)
  }
}

fn rewrite_operand(self: ReplacePseudoRegsPass, operand: AsmOperand) -> AsmOperand! {
  match operand {
    Pseudo(name) => {
      if self.symbols.is_symbol_static(name) {
        Data(name)
      } else {
        let offset = match self.pseudo_reg_offsets[name] {
          Some(offset) => offset
          None => {
            let ty = self.symbols.get_symbol_ty!(name)
            let size = ty.size()
            let alignment = ty.alignment()
            self.stack_offset = round_aligned(alignment, self.stack_offset - size)
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

fn rewrite_inst(self: ReplacePseudoRegsPass, inst: AsmInst) -> AsmInst! {
  match inst {
    Mov(~src, ~dst, ~ty) => {
      let src = self.rewrite_operand!(src)
      let dst = self.rewrite_operand!(dst)
      Mov(~src, ~dst, ~ty)
    }
    Movsx(~src, ~dst) => {
      let src = self.rewrite_operand!(src)
      let dst = self.rewrite_operand!(dst)
      Movsx(~src, ~dst)
    }
    Unary(~op, ~dst, ~ty) => {
      let dst = self.rewrite_operand!(dst)
      Unary(~op, ~dst, ~ty)
    }
    Binary(~op, ~src, ~dst, ~ty) => {
      let src = self.rewrite_operand!(src)
      let dst = self.rewrite_operand!(dst)
      Binary(~op, ~src, ~dst, ~ty)
    }
    Cmp(~src, ~dst, ~ty) => {
      let src = self.rewrite_operand!(src)
      let dst = self.rewrite_operand!(dst)
      Cmp(~src, ~dst, ~ty)
    }
    IDiv(~src, ~ty) => {
      let src = self.rewrite_operand!(src)
      IDiv(~src, ~ty)
    }
    Cdq(~ty) => Cdq(~ty)
    Jmp(target) => Jmp(target)
    JmpCond(~cond, ~target) => JmpCond(~cond, ~target)
    SetCond(~cond, ~dst) => {
      let dst = self.rewrite_operand!(dst)
      SetCond(~cond, ~dst)
    }
    Label(name) => Label(name)
    Push(operand) => {
      let operand = self.rewrite_operand!(operand)
      Push(operand)
    }
    Call(name) => Call(name)
    Ret => Ret
  }
}

pub fn run(self: ReplacePseudoRegsPass, insts: Array[AsmInst]) -> Array[AsmInst]! {
  let new_insts = []

  for inst in insts {
    new_insts.push(self.rewrite_inst!(inst))
  }

  new_insts
}
