
pub struct TacBuilder {
  instructions: Array[TacInst]
  mut next_var_id: Int
}

pub fn TacBuilder::new() -> TacBuilder {
  { instructions: [], next_var_id: 0 }
}

fn new_var(self: TacBuilder) -> String {
  let var_name = "tmp." + self.next_var_id.to_string()
  self.next_var_id += 1
  var_name
}

fn emit(self: TacBuilder, inst: TacInst) -> Unit {
  self.instructions.push(inst)
}

fn convert_expr(self: TacBuilder, expr: Expr) -> TacValue {
  match expr {
    Constant(n) => Constant(n)
    Unary(~op, ~expr) => {
      let src = self.convert_expr(expr)
      let dst_name = self.new_var()
      let dst = TacValue::Variable(dst_name)
      self.emit(Unary(~op, ~src, ~dst))
      dst
    }
    Binary(~op, ~lhs, ~rhs) => {
      let v1 = self.convert_expr(lhs)
      let v2 = self.convert_expr(rhs)
      let dst_name = self.new_var()
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
