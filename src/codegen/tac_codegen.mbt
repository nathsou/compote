
struct TacContext {
  mut next_var_id: Int
  mut next_label_id: Int
}

let ctx: TacContext = { next_var_id: 0, next_label_id: 0 }

pub struct TacBuilder {
  instructions: Array[TacInst]
  switch_stack: Array[TacSwitchStackEntry]
  symbols: Map[String, @validate.SymbolTableEntry]
}

pub type! TacBuilderError {
  InvalidAssignmentTarget(@parse.Expr)
  BreakWithoutLabel
  ContinueWithoutLabel
  LoopWithoutLabel
  SwitchWithoutLabel
  CaseWithoutLabel
  CaseOutsideSwitch
  DefaultOutsideSwitch
  Unreachable(String)
} derive(Show)

pub fn TacBuilder::new(symbols: Map[String, @validate.SymbolTableEntry]) -> TacBuilder {
  {
    instructions: [],
    switch_stack: [],
    symbols,
  }
}

struct TacSwitchStackEntry {
  label: String
  subject: TacValue
  cases_count: Int
  mut current_case_index: Int
}

fn next_temp_var(self: TacBuilder, ty: @parse.Type) -> String {
  let var_name = "tmp." + ctx.next_var_id.to_string()
  ctx.next_var_id += 1
  self.symbols[var_name] = Variable(ty=ty, attributes=LocalAttr)

  var_name
}

fn next_label(prefix: String) -> String {
  let label_name = prefix + ctx.next_label_id.to_string()
  ctx.next_label_id += 1
  label_name
}

fn emit(self: TacBuilder, inst: TacInst) -> Unit {
  self.instructions.push(inst)
}

fn get_break_label(loop_label: String) -> String {
  loop_label + ".break"
}

fn get_continue_label(loop_label: String) -> String {
  loop_label + ".continue"
}

fn get_case_label(switch_label: String, case_index: Int) -> String {
  "case_" + switch_label + "." + case_index.to_string()
}

fn get_case_fallthrough_label(switch_label: String, case_index: Int) -> String {
  "case_fallthrough_" + switch_label + "." + case_index.to_string()
}

fn cast_expr(src: TacValue, dst: TacValue, src_ty: @parse.Type, dst_ty: @parse.Type) -> TacInst {
    // assuming src.ty != dst.ty

    let src_ty_size = src_ty.byte_size()
    let dst_ty_size = dst_ty.byte_size()

    if dst_ty_size == src_ty_size {
      return Copy(~src, ~dst)
    } else if dst_ty_size < src_ty_size {
      return Truncate(~src, ~dst)
    } else if src_ty.is_signed() {
      return SignExtend(~src, ~dst)
    } else {
      return ZeroExtend(~src, ~dst)
    }
}

fn convert_expr(self: TacBuilder, expr: @parse.Expr) -> TacValue!TacBuilderError {
  match expr {
    Constant(~const, ty=_) => Constant(const)
    Variable(~name, ty=_) => Variable(name)
    Cast(~target_ty, ~expr, ty=_) => {
      let res = self.convert_expr!(expr)
      let expr_ty = expr.ty()

      if target_ty == expr_ty {
        return res
      }

      let dst_name = self.next_temp_var(target_ty)
      let dst = TacValue::Variable(dst_name)
      self.emit(cast_expr(res, dst, expr_ty, target_ty))

      dst
      
    }
    Unary(~op, ~expr, ~ty) => {
      let src = self.convert_expr!(expr)

      match op {
        Inc(~is_prefix) | Dec(~is_prefix) as op => {
          let op: @parse.BinaryOp = match op {
            Inc(_) => Add
            _ => Sub
          }

          if is_prefix {
            self.emit(Binary(~op, lhs=src, rhs=Constant(IntConst(1)), dst=src))
            src
          } else {
            // introduce a temporary variable to store the original value
            let tmp_name = self.next_temp_var(ty.unwrap())
            let tmp = TacValue::Variable(tmp_name)
            self.emit(Copy(~src, dst=tmp))
            self.emit(Binary(~op, lhs=src, rhs=Constant(IntConst(1)), dst=src))
            tmp
          }
        }
        _ => {
          let dst = TacValue::Variable(self.next_temp_var(ty.unwrap())) 
          self.emit(Unary(~op, ~src, ~dst))
          dst
        }
      }
    }
    Binary(op=LogicalAnd, ~lhs, ~rhs, ty=_) => {
      let result_name = self.next_temp_var(expr.ty())
      let result = TacValue::Variable(result_name)
      let false_label = next_label("and_false")
      let end_label = next_label("and_end")
      let v1 = self.convert_expr!(lhs)
      self.emit(JumpIfZero(cond=v1, target=false_label))
      let v2 = self.convert_expr!(rhs)
      self.emit(JumpIfZero(cond=v2, target=false_label))
      self.emit(Copy(src=Constant(IntConst(1)), dst=result))
      self.emit(Jump(end_label))
      self.emit(Label(false_label))
      self.emit(Copy(src=Constant(IntConst(0)), dst=result))
      self.emit(Label(end_label))
      result
    }
    Binary(op=LogicalOr, ~lhs, ~rhs, ~ty) => {
      let result_name = self.next_temp_var(ty.unwrap())
      let result = TacValue::Variable(result_name)
      let true_label = next_label("or_true")
      let end_label = next_label("or_end")
      let v1 = self.convert_expr!(lhs)
      self.emit(JumpIfNotZero(cond=v1, target=true_label))
      let v2 = self.convert_expr!(rhs)
      self.emit(JumpIfNotZero(cond=v2, target=true_label))
      self.emit(Copy(src=Constant(IntConst(0)), dst=result))
      self.emit(Jump(end_label))
      self.emit(Label(true_label))
      self.emit(Copy(src=Constant(IntConst(1)), dst=result))
      self.emit(Label(end_label))
      result
    }
    Binary(~op, ~lhs, ~rhs, ~ty) => {
      let v1 = self.convert_expr!(lhs)
      let v2 = self.convert_expr!(rhs)
      let dst_name = self.next_temp_var(ty.unwrap())
      let dst = TacValue::Variable(dst_name)
      self.emit(Binary(~op, lhs=v1, rhs=v2, ~dst))
      dst
    }
    Assignment(~op, lhs=Variable(~name, ty=Some(lhs_ty)), ~rhs, ty=Some(res_ty)) => {
      let compound_op: Option[@parse.BinaryOp] = match op {
        AssignAdd => Some(Add)
        AssignSub => Some(Sub)
        AssignMul => Some(Mul)
        AssignDiv => Some(Div)
        AssignRem => Some(Rem)
        AssignAnd => Some(BitwiseAnd)
        AssignOr => Some(BitwiseOr)
        AssignXor => Some(BitwiseXor)
        AssignShl => Some(Shl)
        AssignShr => Some(Shr)
        Assign => None
      }

      let rhs = self.convert_expr!(rhs)
      let dst = TacValue::Variable(name)

      match compound_op {
        Some(op) => {
          if lhs_ty == res_ty {
            self.emit(Binary(~op, lhs=dst, ~rhs, ~dst))
          } else {
            let tmp = TacValue::Variable(self.next_temp_var(res_ty))
            self.emit(cast_expr(dst, tmp, lhs_ty, res_ty))
            self.emit(Binary(~op, lhs=tmp, ~rhs, dst=tmp))
            self.emit(cast_expr(tmp, dst, lhs_ty, res_ty))
          }

          dst
        }
        None => {
          self.emit(Copy(src=rhs, ~dst))
          dst
        }
      }
    }
    Assignment(op=_, ~lhs, rhs=_, ty=_) => raise InvalidAssignmentTarget(lhs)
    Conditional(~cond, ~then, ~otherwise, ~ty) => {
      let cond = self.convert_expr!(cond)
      let else_label = next_label("ternary_else")
      let end_label = next_label("ternary_end")
      let result = TacValue::Variable(self.next_temp_var(ty.unwrap()))
      self.emit(JumpIfZero(~cond, target=else_label))
      let then_value = self.convert_expr!(then)
      self.emit(Copy(src=then_value, dst=result))
      self.emit(Jump(end_label))
      self.emit(Label(else_label))
      let otherwise_value = self.convert_expr!(otherwise)
      self.emit(Copy(src=otherwise_value, dst=result))
      self.emit(Label(end_label))
      result
    }
    Call(~name, ~args, ~ty) => {
      let arg_values = []

      for arg in args {
        arg_values.push(self.convert_expr!(arg))
      }

      let result = TacValue::Variable(self.next_temp_var(ty.unwrap()))
      self.emit(Call(~name, args=arg_values, dst=result))
      result
    }
  }
}

fn convert_stmt(self: TacBuilder, stmt: @parse.Stmt) -> Unit!TacBuilderError {
  match stmt {
    Return(expr) => {
      let src = self.convert_expr!(expr)
      self.emit(Return(src))
    }
    Expression(expr) => {
      let _ = self.convert_expr!(expr)
    }
    If(~cond, ~then, ~otherwise) => {
      let cond = self.convert_expr!(cond)
      
      match otherwise {
        None => {
          let end_label = next_label("if_end")
          self.emit(JumpIfZero(~cond, target=end_label))
          self.convert_stmt!(then)
          self.emit(Label(end_label))
        }
        Some(otherwise) => {
          let else_label = next_label("if_else")
          let end_label = next_label("if_end")
          self.emit(JumpIfZero(~cond, target=else_label))
          self.convert_stmt!(then)
          self.emit(Jump(end_label))
          self.emit(Label(else_label))
          self.convert_stmt!(otherwise)
          self.emit(Label(end_label))
        }
      }
    }
    Labeled(~label, ~stmt) => {
      self.emit(Label(label))
      self.convert_stmt!(stmt)
    }
    Goto(label) => {
      self.emit(Jump(label))
    }
    Compound(items) => {
      for item in items {
        match item {
          Decl(decl) => self.convert_decl!(decl)
          Stmt(stmt) => self.convert_stmt!(stmt)
        }
      }
    }
    Break(~label) => {
      match label {
        Some(label) => self.emit(Jump(get_break_label(label)))
        None => raise BreakWithoutLabel
      }
    }
    Continue(~label) => {
      match label {
        Some(label) => self.emit(Jump(get_continue_label(label)))
        None => raise ContinueWithoutLabel
      }
    }
    While(~cond, ~body, ~label) => {
      let loop_label = match label {
        Some(label) => label
        None => raise LoopWithoutLabel
      }

      let start_label = get_continue_label(loop_label)
      let end_label = get_break_label(loop_label)
      self.emit(Label(start_label))
      let cond = self.convert_expr!(cond)
      self.emit(JumpIfZero(~cond, target=end_label))
      self.convert_stmt!(body)
      self.emit(Jump(start_label))
      self.emit(Label(end_label))
    }
    DoWhile(~body, ~cond, ~label) => {
      let loop_label = match label {
        Some(label) => label
        None => raise LoopWithoutLabel
      }

      let start_label = next_label("do_start")
      self.emit(Label(start_label))
      self.convert_stmt!(body)
      self.emit(Label(get_continue_label(loop_label)))
      let cond = self.convert_expr!(cond)
      self.emit(JumpIfNotZero(~cond, target=start_label))
      self.emit(Label(get_break_label(loop_label)))
    }
    For(~init, ~cond, ~post, ~body, ~label) => {
      let loop_label = match label {
        Some(label) => label
        None => raise LoopWithoutLabel
      }

      let start_label = next_label("for_start")
      let continue_label = get_continue_label(loop_label)
      let end_label = get_break_label(loop_label)

      match init {
        InitEmpty => ()
        InitDecl(decl) => self.convert_decl!(@parse.VarDecl(decl))
        InitExpr(expr) => { let _ = self.convert_expr!(expr) }
      }

      self.emit(Label(start_label))
      
      match cond {
        Some(cond) => {
          let cond = self.convert_expr!(cond)
          self.emit(JumpIfZero(~cond, target=end_label))
        }
        None => ()
      }

      self.convert_stmt!(body)
      
      self.emit(Label(continue_label))

      match post {
        Some(post) => { let _ = self.convert_expr!(post) }
        None => ()
      }

      self.emit(Jump(start_label))
      self.emit(Label(end_label))
    }
    Switch(~expr, ~body, ~label, ~cases_count) => {
      let switch_label = match label {
        Some(label) => label
        None => raise SwitchWithoutLabel
      }

      let subject = self.convert_expr!(expr)

      if cases_count > 0 {
        self.switch_stack.push({
          label: switch_label,
          subject,
          cases_count,
          current_case_index: 0,
        })
        self.emit(Jump(get_case_label(switch_label, 0)))
        self.convert_stmt!(body)
        self.emit(Label(get_break_label(switch_label)))
        let _ = self.switch_stack.pop()
      }
    }
    Case(~expr, ~body) => {
      let switch_entry = match self.switch_stack.last() {
        Some(entry) => entry
        None => raise CaseOutsideSwitch
      }

      let case_index = switch_entry.current_case_index
      let next_case_label = if case_index != switch_entry.cases_count - 1 {
        switch_entry.current_case_index += 1
        // if this is not the last case, jump to the next case label (skipping the condition)
        let next_case_label = get_case_label(switch_entry.label, case_index + 1)
        next_case_label
      } else {
        // if this is the last case, jump to the break label
        get_break_label(switch_entry.label)
      }

      let fallthrough_label = get_case_fallthrough_label(switch_entry.label, case_index)

      if case_index > 0 {
        self.emit(Jump(fallthrough_label))
      }

      self.emit(Label(get_case_label(switch_entry.label, case_index)))
      let enter_case = TacValue::Variable(self.next_temp_var(Int))
      self.emit(Binary(op=Equ, lhs=switch_entry.subject, rhs=self.convert_expr!(expr), dst=enter_case))
      self.emit(JumpIfZero(cond=enter_case, target=next_case_label))
      if case_index != 0 {
        self.emit(Label(fallthrough_label))
      }
      self.convert_stmt!(body)
    }
    Default(~body) => {
      let switch_entry = match self.switch_stack.last() {
        Some(entry) => entry
        None => raise DefaultOutsideSwitch
      }

      self.emit(Label(get_case_label(switch_entry.label, switch_entry.current_case_index)))
      self.emit(Label(get_case_fallthrough_label(switch_entry.label, switch_entry.current_case_index)))

      // the default case is not necessarily the last case in the switch statement
      let next_fallthrough_label = if switch_entry.current_case_index != switch_entry.cases_count - 1 {
        switch_entry.current_case_index += 1
        Some(get_case_fallthrough_label(switch_entry.label, switch_entry.current_case_index))
      } else {
        None
      }

      self.convert_stmt!(body)

      match next_fallthrough_label {
        Some(label) => self.emit(Jump(label))
        None => ()
      }
    }
    Null => ()
  }
}

fn convert_decl(self: TacBuilder, decl: @parse.Decl) -> Unit!TacBuilderError {
  match decl {
    // extern and static variables are handled at the top level
    VarDecl({ storage_class: Some(_), ty: _, name: _, value: _}) => ()
    VarDecl({ storage_class: _, ty: _, name, value }) => {
      match value {
        Some(expr) => {
          let src = self.convert_expr!(expr)
          let dst = TacValue::Variable(name)
          self.emit(Copy(~src, ~dst))
        }
        None => ()
      }
    }
    FunDecl(_) => ()
  }
}

fn convert_function_def(func: @parse.FunctionDecl, symbols: Map[String, @validate.SymbolTableEntry]) -> TacFunctionDef?!TacBuilderError {
  let builder = TacBuilder::new(symbols)

  match func.body {
    Some(body) => {
      for item in body {
        match item {
          Stmt(stmt) => builder.convert_stmt!(stmt)
          Decl(decl) => builder.convert_decl!(decl)
        }
      }

      // for now always add a "return 0;" at the end of the function
      // this handles the case where the function does not return a value
      builder.convert_stmt!(Return(Constant(const=IntConst(0), ty=Some(Int))))

      let global = match symbols.get(func.name) {
        Some(Function(ty=_, attributes=FunAttr(~global, defined=_))) => global
        Some(Function(_)) => raise TacBuilderError::Unreachable("function symbol with invalid attributes")
        _ => raise TacBuilderError::Unreachable("function symbol not found")
      }

      Some({
        name: func.name,
        global,
        params: func.params.map(fn(param) { param.name }),
        body: builder.instructions,
      })
    }
    // ignore function declarations
    None => None
  }
}

pub fn convert_prog(prog: @parse.Program, symbols: Map[String, @validate.SymbolTableEntry]) -> TacProgram!TacBuilderError {
  let top_level: Array[TacTopLevel] = []

  for decl in prog.decls {
    match decl {
      FunDecl(func) => {
        match convert_function_def!(func, symbols) {
          Some(def) => top_level.push(Function(def))
          None => ()
        }
      }
      VarDecl(_) => ()
    }
  }

  // collect static variables
  for name, entry in symbols {
    match entry {
      Variable(~ty, ~attributes) => {
        match attributes {
          StaticAttr(~init, ~global) => {
            match init {
              Initial(init) => {
                top_level.push(StaticVar(~name, ~global, ~init, ~ty))
              }
              Tentative => {
                let init: @validate.StaticInit = match ty {
                  Int => IntInit(0)
                  Long => LongInit(0)
                  UnsignedInt => UIntInit(0)
                  UnsignedLong => ULongInit(0)
                  _ => raise TacBuilderError::Unreachable("unsupported static variable type")
                }

                top_level.push(StaticVar(~name, ~global, ~init, ~ty))
              }
              NoInitializer => ()
            }
          }
          _ => ()
        }
      }
      _ => ()
    }
  }

  { top_level, }
}
