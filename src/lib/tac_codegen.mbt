
pub struct TacBuilder {
  instructions: Array[TacInst]
  mut next_var_id: Int
  mut next_label_id: Int
}

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

fn convert_expr(self: TacBuilder, expr: Expr) -> TacValue {
  match expr {
    Constant(n) => Constant(n)
    Unary(~op, ~expr) => {
      let src = self.convert_expr(expr)
      let dst_name = self.next_temp_var()
      let dst = TacValue::Variable(dst_name)
      self.emit(Unary(~op, ~src, ~dst))
      dst
    }
    Binary(~op=LogicalAnd, ~lhs, ~rhs) => {
      let result_name = self.next_temp_var()
      let result = TacValue::Variable(result_name)
      let false_label = self.next_label("and_false")
      let end_label = self.next_label("and_end")
      let v1 = self.convert_expr(lhs)
      self.emit(JumpIfZero(~cond=v1, ~target=false_label))
      let v2 = self.convert_expr(rhs)
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
      let v1 = self.convert_expr(lhs)
      self.emit(JumpIfNotZero(~cond=v1, ~target=true_label))
      let v2 = self.convert_expr(rhs)
      self.emit(JumpIfNotZero(~cond=v2, ~target=true_label))
      self.emit(Copy(~src=Constant(0), ~dst=result))
      self.emit(Jump(end_label))
      self.emit(Label(true_label))
      self.emit(Copy(~src=Constant(1), ~dst=result))
      self.emit(Label(end_label))
      result
    }
    Binary(~op, ~lhs, ~rhs) => {
      let v1 = self.convert_expr(lhs)
      let v2 = self.convert_expr(rhs)
      let dst_name = self.next_temp_var()
      let dst = TacValue::Variable(dst_name)
      self.emit(Binary(~op, ~lhs=v1, ~rhs=v2, ~dst))
      dst
    }
  }
}

fn convert_stmt(self: TacBuilder, stmt: Stmt) -> Unit {
  match stmt {
    Return(expr) => {
      let src = self.convert_expr(expr)
      self.emit(Return(src))
    }
  }
}

fn convert_decl(decl: Decl) -> TacFunctionDef {
  match decl {
    Function(~returnType=_, ~name, ~body) => {
      let builder = TacBuilder::new()
      builder.convert_stmt(body)
      { name, body: builder.instructions }
    }
  }
}

pub fn convert_prog(prog: Program) -> TacProgram {
  { functions: prog.decls.map(convert_decl) }
}
