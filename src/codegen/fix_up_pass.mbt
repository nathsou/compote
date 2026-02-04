
let max_i32: Int64 = @int.max_value.to_int64()
let min_i32: Int64 = @int.min_value.to_int64()
let max_u32: Int64 = 4294967295 // 2^32 - 1

fn fits_in_i32(val: Int64) -> Bool {
  val <= max_i32 && val >= min_i32
}

fn is_larger_than_u32(n: Int64) -> Bool {
  n > max_u32 || n < min_i32
}

fn fixup_rewrite_inst(inst: AsmInst) -> Array[AsmInst] {
  match inst {
     // some instructions with stack addresses as both operands are invalid
    Mov(src=Stack(_) | Data(_) as src, dst=Stack(_) | Data(_) as dst, ty~) => {
      let tmp_reg = if ty is Double { XMM14 } else { R10 }
      [
        Mov(src~, dst=Reg(tmp_reg), ty~),
        Mov(src=Reg(tmp_reg), dst~, ty~),
      ]
    }
    // Mov cannot move a large immediate to a memory location
    Mov(src=Imm(n) as src, dst=Stack(_) | Data(_) as dst, ty=QuadWord) => {
      if not(fits_in_i32(n)) {
        [
          Mov(src~, dst=Reg(R10), ty=QuadWord),
          Mov(src=Reg(R10), dst~, ty=QuadWord),
        ]
      } else {
        [inst]
      }
    }
    // Prevent GCC warning when truncating using movl
    Mov(src=Imm(n), dst~, ty=LongWord) => {
      if is_larger_than_u32(n) {
        [Mov(src=Imm(n & 0xffffffff), dst~, ty=LongWord)]
      } else {
        [inst]
      }
    }
    // Movsx can't handle an immediate source or a memory destination
    Movsx(src=Imm(_) as src, dst=Stack(_) | Data(_) as dst) => {
      [
        Mov(src~, dst=Reg(R10), ty=LongWord),
        Movsx(src=Reg(R10), dst=Reg(R11)),
        Mov(src=Reg(R11), dst~, ty=QuadWord),
      ]
    }
    Movsx(src=Imm(_) as src, dst~) => {
      [
        Mov(src~, dst=Reg(R10), ty=LongWord),
        Movsx(src=Reg(R10), dst~),
      ]
    }
    Movsx(src~, dst=Stack(_) | Data(_) as dst) => {
      [
        Movsx(src~, dst=Reg(R11)),
        Mov(src=Reg(R11), dst~, ty=QuadWord),
      ]
    }
    MovZeroExtend(src~, dst=Reg(_) as dst) => {
      [
        Mov(src~, dst~, ty=LongWord),
      ]
    }
    MovZeroExtend(src~, dst=Stack(_) | Data(_) as dst) => {
      [
        Mov(src~, dst=Reg(R11), ty=LongWord),
        Mov(src=Reg(R11), dst~, ty=QuadWord),
      ]
    }
    Binary(op=Mul, src~, dst~, ty~) => {
      let is_large_src_imm = match src {
        Imm(n) => !fits_in_i32(n)
        _ => false
      }

      match (ty, is_large_src_imm, dst) {
        // imul can't use a memory address as destination and src can't be a large immediate
        (QuadWord, true, Stack(_) | Data(_)) => {
          // rewrite both operands
          [
            Mov(src~, dst=Reg(R10), ty=QuadWord),
            Mov(src=dst, dst=Reg(R11), ty=QuadWord),
            Binary(op=Mul, src=Reg(R10), dst=Reg(R11), ty=QuadWord),
            Mov(src=Reg(R11), dst~, ty=QuadWord),
          ]
        }
        (QuadWord, true, _) => {
          [
            Mov(src~, dst=Reg(R10), ty=QuadWord),
            Binary(op=Mul, src=Reg(R10), dst~, ty=QuadWord),
          ]
        }
        (_, _, Stack(_) | Data(_)) => {
          [
            Mov(src=dst, dst=Reg(R11), ty~),
            Binary(op=Mul, src~, dst=Reg(R11), ty~),
            Mov(src=Reg(R11), dst~, ty~)
          ]
        }
        _ => [inst]
      }
    }
    Binary(op~, src~, dst~, ty~) => {
      let is_large_src_imm = match src {
        Imm(n) => !fits_in_i32(n)
        _ => false
      }

      match (ty, is_large_src_imm, op, src, dst) {
        // can't take large immediates as source operands
        (QuadWord, true, Add | Sub | And | Or | Xor, _, _) => {
          [
            Mov(src~, dst=Reg(R10), ty=QuadWord),
            Binary(op~, src=Reg(R10), dst~, ty=QuadWord),
          ]
        }
        (Double, _, _, _, _) => {
          [
            Mov(src~, dst=Reg(XMM15), ty=Double),
            Binary(op~, src=Reg(XMM15), dst=Reg(XMM15), ty=Double),
            Mov(src=Reg(XMM15), dst~, ty=Double),
          ]
        }
        // can't use memory addresses for both operands
        (_, _, Add | Sub | And | Or | Xor, Stack(_) | Data(_), Stack(_) | Data(_)) => {
          [
            Mov(src~, dst=Reg(R10), ty~),
            Binary(op~, src=Reg(R10), dst~, ty~),
          ]
        }
        _ => [inst]
      }
    }
    // div, idiv, cmp instructions do not support immediate operands
    IDiv(src=Imm(n), ty~) => [
      Mov(src=Imm(n), dst=Reg(R10), ty~),
      IDiv(src=Reg(R10), ty~),
    ]
    Div(src=Imm(n), ty~) => [
      Mov(src=Imm(n), dst=Reg(R10), ty~),
      Div(src=Reg(R10), ty~),
    ]
    Cmp(src=_, dst=Reg(_), ty=Double) => [inst]
    Cmp(src~, dst~, ty=Double) => [
      Mov(src~, dst=Reg(XMM15), ty=Double),
      Cmp(src=Reg(XMM15), dst~, ty=Double),
    ]
    Cmp(src~, dst~, ty~) => {
      let is_large_src_imm = match src {
        Imm(n) => !fits_in_i32(n)
        _ => false
      }

      // Cmp(src=Imm(2147483663), dst=Stack(-24), ty=QuadWord)
      match (ty, is_large_src_imm, src, dst) {
        // both operands can't be in memory
        (_, _, Stack(_) | Data(_), Stack(_) | Data(_)) => {
          [
            Mov(src~, dst=Reg(R10), ty~),
            Cmp(src=Reg(R10), dst~, ty~),
          ]
        }
        // first operand can't be a large immediate, second one can't be an immediate at all
        (QuadWord, true, _, Imm(_)) => {
          [
            Mov(src~, dst=Reg(R10), ty=QuadWord),
            Mov(src=dst, dst=Reg(R11), ty=QuadWord),
            Cmp(src=Reg(R10), dst=Reg(R11), ty=QuadWord),
          ]
        }
        (QuadWord, true, _, _) => {
          [
            Mov(src~, dst=Reg(R10), ty=QuadWord),
            Cmp(src=Reg(R10), dst~, ty=QuadWord),
          ]
        }
        (_, _, _, Imm(_)) => {
          [
            Mov(src=dst, dst=Reg(R11), ty~),
            Cmp(src~, dst=Reg(R11), ty~),
          ]
        }
        _ => [inst]
      }
    }
    Push(Imm(n) as src) => {
      if !fits_in_i32(n) {
        [
          Mov(src~, dst=Reg(R10), ty=QuadWord),
          Push(Reg(R10)),
        ]
      } else {
        [inst]
      }
    }
    // cvttsd2si's dst operand must be a register
    Cvttsd2si(src~, dst=(Stack(_) | Data(_)) as dst, dst_ty~) => [
      Cvttsd2si(src~, dst=Reg(R11), dst_ty~),
      Mov(src=Reg(R11), dst~, ty=dst_ty),
    ]
    Cvtsi2sd(src~, dst=(Stack(_) | Data(_)) as dst, src_ty~) => {
      let is_src_const = src is Imm(_)
      let is_dst_mem = dst is (Stack(_) | Data(_))

      if is_src_const && is_dst_mem {
        [
          Mov(src~, dst=Reg(R10), ty=src_ty),
          Cvtsi2sd(src_ty~, src=Reg(R10), dst=Reg(XMM15)),
          Mov(src=Reg(XMM15), dst~, ty=Double),
        ]
      } else if is_src_const {
        [
          Mov(src~, dst=Reg(R10), ty=src_ty),
          Cvtsi2sd(src_ty~, src=Reg(R10), dst~),
        ]
      } else if is_dst_mem {
        [
          Cvtsi2sd(src_ty~, src~, dst=Reg(XMM15)),
          Mov(src=Reg(XMM15), dst~, ty=Double),
        ]
      } else {
        [inst]
      }
    }  
    _ => [inst]
  }
}

pub fn fixup_run(insts: Array[AsmInst]) -> Array[AsmInst] {
  let new_insts = []

  for inst in insts {
    for new_inst in fixup_rewrite_inst(inst) {
      new_insts.push(new_inst)
    }
  }

  new_insts
}
