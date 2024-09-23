
pub struct VariableResolutionPass {
  mut next_var_id: Int
  mut next_goto_label_id: Int
}

pub fn VariableResolutionPass::new() -> VariableResolutionPass {
  { next_var_id: 0, next_goto_label_id: 0 }
}

pub type! VariableResolutionError {
  VariableAlreadyDeclared(String)
  UndeclaredVariable(String)
  InvalidLvalue(String)
  LabelAlreadyDeclared(String)
  UndeclaredLabel(String)
} derive(Show)

fn is_lvalue(expr: @parse.Expr) -> Bool {
  match expr {
    Variable(_) => true
    _ => false
  }
}

struct Scope {
  variables: Map[String, String]
  goto_labels: Map[String, String]
}

fn Scope::new() -> Scope {
  { variables: {}, goto_labels: {} }
}

fn resolve_expr(self: VariableResolutionPass, expr: @parse.Expr, scope: Scope) -> @parse.Expr!VariableResolutionError {
  match expr {
    Variable(name) => {
      match scope.variables.get(name) {
        Some(new_name) => @parse.Variable(new_name)
        None => raise UndeclaredVariable(name)
      }
    }
    Constant(n) => @parse.Constant(n)
    Unary(~op, ~expr) => {
      match op {
        Inc(_) | Dec(_) => {
          if not(is_lvalue(expr)) {
            raise InvalidLvalue(Show::to_string(expr))
          }
        }
        _ => ()
      }

      @parse.Unary(~op, ~expr=self.resolve_expr!(expr, scope))
    }
    Binary(~op, ~lhs, ~rhs) => @parse.Binary(~op, ~lhs=self.resolve_expr!(lhs, scope), ~rhs=self.resolve_expr!(rhs, scope))
    Assignment(~op, ~lhs, ~rhs) => {
      if not(is_lvalue(lhs)) {
        raise InvalidLvalue(Show::to_string(lhs))
      }

      @parse.Expr::Assignment(~op, ~lhs=self.resolve_expr!(lhs, scope), ~rhs=self.resolve_expr!(rhs, scope))
    }
    Conditional(~cond, ~then, ~otherwise) => {
      @parse.Conditional(
        ~cond=self.resolve_expr!(cond, scope),
        ~then=self.resolve_expr!(then, scope),
        ~otherwise=self.resolve_expr!(otherwise, scope),
      )
    }
  }
}

fn resolve_stmt(self: VariableResolutionPass, stmt: @parse.Stmt, scope: Scope) -> @parse.Stmt!VariableResolutionError {
  match stmt {
    Return(expr) => @parse.Stmt::Return(self.resolve_expr!(expr, scope))
    Expression(expr) => @parse.Stmt::Expression(self.resolve_expr!(expr, scope))
    If(~cond, ~then, ~otherwise) => {
      @parse.Stmt::If(
        ~cond=self.resolve_expr!(cond, scope),
        ~then=self.resolve_stmt!(then, scope),
        ~otherwise=match otherwise {
          Some(stmt) => Some(self.resolve_stmt!(stmt, scope))
          None => None
        },
      )
    }
    Labeled(~label, ~stmt) => {
      match scope.goto_labels.get(label) {
        Some(unique_label) => @parse.Labeled(~label=unique_label, ~stmt=self.resolve_stmt!(stmt, scope))
        None => raise UndeclaredLabel(label)
      }
    }
    Goto(label) => {
      match scope.goto_labels.get(label) {
        Some(unique_label) => @parse.Stmt::Goto(unique_label)
        None => raise UndeclaredLabel(label)
      }
    }
    Null => @parse.Null
  }
}

fn resolve_decl(self: VariableResolutionPass, decl: @parse.Decl, scope: Scope) -> @parse.Decl!VariableResolutionError {
  match decl {
    VariableDecl(~ty, ~name, ~value) => {
      if scope.variables.contains(name) {
        raise VariableAlreadyDeclared(name)
      }

      let new_name = "var." + name + "." + self.next_var_id.to_string()
      scope.variables[name] = new_name
      self.next_var_id += 1

      match value {
        Some(expr) => {
          let new_expr = self.resolve_expr!(expr, scope)
          @parse.VariableDecl(~ty, ~name=new_name, ~value=Some(new_expr))
        }
        None => @parse.VariableDecl(~ty, ~name=new_name, ~value=None)
      }
    }
  }
}

fn collect_expr_labels(expr: @parse.Expr, scope: Scope) -> Unit {
  match expr {
    Variable(_) => ()
    Constant(_) => ()
    Unary(~op=_, ~expr) => collect_expr_labels(expr, scope)
    Binary(~op=_, ~lhs, ~rhs) => {
      collect_expr_labels(lhs, scope)
      collect_expr_labels(rhs, scope)
    }
    Assignment(~op=_, ~lhs, ~rhs) => {
      collect_expr_labels(lhs, scope)
      collect_expr_labels(rhs, scope)
    }
    Conditional(~cond, ~then, ~otherwise) => {
      collect_expr_labels(cond, scope)
      collect_expr_labels(then, scope)
      collect_expr_labels(otherwise, scope)
    }
  }
}

fn collect_stmt_labels(self: VariableResolutionPass, stmt: @parse.Stmt, scope: Scope) -> Unit!VariableResolutionError {
  match stmt {
    Return(_) => ()
    Expression(_) => ()
    If(~cond, ~then, ~otherwise) => {
      collect_expr_labels(cond, scope)
      self.collect_stmt_labels!(then, scope)
      match otherwise {
        Some(stmt) => self.collect_stmt_labels!(stmt, scope)
        None => ()
      }
    }
    Labeled(~label, ~stmt) => {
      if scope.goto_labels.contains(label) {
        raise LabelAlreadyDeclared(label)
      }

      scope.goto_labels[label] = "goto." + label + "." + self.next_goto_label_id.to_string()
      self.collect_stmt_labels!(stmt, scope)
    }
    Goto(_) => ()
    Null => ()
  }
}

fn collect_function_def_labels(self: VariableResolutionPass, func: @parse.FunctionDef, scope: Scope) -> Unit!VariableResolutionError {
  for item in func.body {
    match item {
      Decl(_) => ()
      Stmt(stmt) => self.collect_stmt_labels!(stmt, scope)
    }
  }
}

// Collect all the labels declared in this function (a goto can reference a label declared later in the function)
fn resolve_function_def(self: VariableResolutionPass, func: @parse.FunctionDef) -> @parse.FunctionDef!VariableResolutionError {
  let scope = Scope::new()
  let new_body = []
  self.collect_function_def_labels!(func, scope)

  for item in func.body {
    match item {
      Decl(decl) => {
        let new_decl = self.resolve_decl!(decl, scope)
        new_body.push(@parse.Decl(new_decl))
      }
      Stmt(stmt) => {
        let new_stmt = self.resolve_stmt!(stmt, scope)
        new_body.push(@parse.Stmt(new_stmt))
      }
    }
  }

  { return_type: func.return_type, name: func.name, body: new_body }
}

pub fn resolved(self: VariableResolutionPass, program: @parse.Program) -> @parse.Program!VariableResolutionError {
  let functions = []

  for func in program.functions {
    let new_func = self.resolve_function_def!(func)
    functions.push(new_func)
  }

  { functions, }
}
