
pub struct TacBuilder {
  instructions: Array[TacInst]
  mut next_var_id: Int
  mut next_label_id: Int
}

pub type! TacBuilderError {
  InvalidAssignmentTarget(@parse.Expr)
} derive(Show)

pub fn TacBuilder::new() -> TacBuilder {
  { instructions: [], next_var_id: 0, next_label_id: 0 }
}

fn next_temp_var(self: TacBuilder) -> String {
  let var_name = "tmp." + self.next_var_id.to_string()
  self.next_var_id += 1
  var_name
}

fn next_label(self: TacBuilder, prefix: String) -> String {
  let label_name = prefix + self.next_label_id.to_string()
  self.next_label_id += 1
  label_name
}

fn emit(self: TacBuilder, inst: TacInst) -> Unit {
  self.instructions.push(inst)
}

fn convert_expr(self: TacBuilder, expr: @parse.Expr) -> TacValue!TacBuilderError {
  match expr {
    Constant(n) => Constant(n)
    Variable(name) => Variable(name)
    Unary(~op, ~expr) => {
      let src = self.convert_expr!(expr)

      match op {
        Inc(~is_prefix) | Dec(~is_prefix) as op => {
          let op: @parse.BinaryOp = match op {
            Inc(_) => Add
            _ => Sub
          }

          if is_prefix {
            self.emit(Binary(~op, ~lhs=src, ~rhs=Constant(1), ~dst=src))
            src
          } else {
            // introduce a temporary variable to store the original value
            let tmp_name = self.next_temp_var()
            let tmp = TacValue::Variable(tmp_name)
            self.emit(Copy(~src, ~dst=tmp))
            self.emit(Binary(~op, ~lhs=src, ~rhs=Constant(1), ~dst=src))
            tmp
          }
        }
        _ => {
          let dst = TacValue::Variable(self.next_temp_var()) 
          self.emit(Unary(~op, ~src, ~dst))
          dst
        }
      }
    }
    Binary(~op=LogicalAnd, ~lhs, ~rhs) => {
      let result_name = self.next_temp_var()
      let result = TacValue::Variable(result_name)
      let false_label = self.next_label("and_false")
      let end_label = self.next_label("and_end")
      let v1 = self.convert_expr!(lhs)
      self.emit(JumpIfZero(~cond=v1, ~target=false_label))
      let v2 = self.convert_expr!(rhs)
      self.emit(JumpIfZero(~cond=v2, ~target=false_label))
      self.emit(Copy(~src=Constant(1), ~dst=result))
      self.emit(Jump(end_label))
      self.emit(Label(false_label))
      self.emit(Copy(~src=Constant(0), ~dst=result))
      self.emit(Label(end_label))
      result
    }
    Binary(~op=LogicalOr, ~lhs, ~rhs) => {
      let result_name = self.next_temp_var()
      let result = TacValue::Variable(result_name)
      let true_label = self.next_label("or_true")
      let end_label = self.next_label("or_end")
      let v1 = self.convert_expr!(lhs)
      self.emit(JumpIfNotZero(~cond=v1, ~target=true_label))
      let v2 = self.convert_expr!(rhs)
      self.emit(JumpIfNotZero(~cond=v2, ~target=true_label))
      self.emit(Copy(~src=Constant(0), ~dst=result))
      self.emit(Jump(end_label))
      self.emit(Label(true_label))
      self.emit(Copy(~src=Constant(1), ~dst=result))
      self.emit(Label(end_label))
      result
    }
    Binary(~op, ~lhs, ~rhs) => {
      let v1 = self.convert_expr!(lhs)
      let v2 = self.convert_expr!(rhs)
      let dst_name = self.next_temp_var()
      let dst = TacValue::Variable(dst_name)
      self.emit(Binary(~op, ~lhs=v1, ~rhs=v2, ~dst))
      dst
    }
    Assignment(~op, ~lhs=Variable(name), ~rhs) => {
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

      let src = self.convert_expr!(match compound_op {
        Some(op) => Binary(~op, ~lhs=Variable(name), ~rhs)
        None => rhs
      })

      let dst = TacValue::Variable(name)
      self.emit(Copy(~src, ~dst))
      dst
    }
    Assignment(~op=_, ~lhs, ~rhs=_) => raise InvalidAssignmentTarget(lhs)
    Conditional(~cond, ~then, ~otherwise) => {
      let cond = self.convert_expr!(cond)
      let else_label = self.next_label("ternary_else")
      let end_label = self.next_label("ternary_end")
      let result = TacValue::Variable(self.next_temp_var())
      self.emit(JumpIfZero(~cond, ~target=else_label))
      let then_value = self.convert_expr!(then)
      self.emit(Copy(~src=then_value, ~dst=result))
      self.emit(Jump(end_label))
      self.emit(Label(else_label))
      let otherwise_value = self.convert_expr!(otherwise)
      self.emit(Copy(~src=otherwise_value, ~dst=result))
      self.emit(Label(end_label))
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
          let end_label = self.next_label("if_end")
          self.emit(JumpIfZero(~cond, ~target=end_label))
          self.convert_stmt!(then)
          self.emit(Label(end_label))
        }
        Some(otherwise) => {
          let else_label = self.next_label("if_else")
          let end_label = self.next_label("if_end")
          self.emit(JumpIfZero(~cond, ~target=else_label))
          self.convert_stmt!(then)
          self.emit(Jump(end_label))
          self.emit(Label(else_label))
          self.convert_stmt!(otherwise)
          self.emit(Label(end_label))
        }
      }
    }
    Null => ()
  }
}

fn convert_decl(self: TacBuilder, decl: @parse.Decl) -> Unit!TacBuilderError {
  match decl {
    VariableDecl(~ty=_, ~name, ~value) => {
      match value {
        Some(expr) => {
          let src = self.convert_expr!(expr)
          let dst = TacValue::Variable(name)
          self.emit(Copy(~src, ~dst))
        }
        None => ()
      }
    }
  }
}

fn convert_function_def(func: @parse.FunctionDef) -> TacFunctionDef!TacBuilderError {
  let builder = TacBuilder::new()
  
  for item in func.body {
    match item {
      Stmt(stmt) => builder.convert_stmt!(stmt)
      Decl(decl) => builder.convert_decl!(decl)
    }
  }

  // for now always add a "return 0;" at the end of the function
  // this handles the case where the function does not return a value
  builder.convert_stmt!(Return(Constant(0)))

  { name: func.name, body: builder.instructions }
}

pub fn convert_prog(prog: @parse.Program) -> TacProgram!TacBuilderError {
  let functions = []

  for func in prog.functions {
    functions.push(convert_function_def!(func))
  }

  { functions, }
}
