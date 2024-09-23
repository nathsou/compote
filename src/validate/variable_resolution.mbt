
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

struct Env {
  variables: Scope[String]
  goto_labels: Map[String, String]
}

fn Env::new() -> Env {
  { variables: Scope::new(), goto_labels: {} }
}

fn child(self: Env) -> Env {
  { variables: self.variables.child(), goto_labels: self.goto_labels }
}

fn resolve_expr(self: VariableResolutionPass, expr: @parse.Expr, env: Env) -> @parse.Expr!VariableResolutionError {
  match expr {
    Variable(name) => {
      match env.variables.resolve(name) {
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

      @parse.Unary(~op, ~expr=self.resolve_expr!(expr, env))
    }
    Binary(~op, ~lhs, ~rhs) => @parse.Binary(~op, ~lhs=self.resolve_expr!(lhs, env), ~rhs=self.resolve_expr!(rhs, env))
    Assignment(~op, ~lhs, ~rhs) => {
      if not(is_lvalue(lhs)) {
        raise InvalidLvalue(Show::to_string(lhs))
      }

      @parse.Expr::Assignment(~op, ~lhs=self.resolve_expr!(lhs, env), ~rhs=self.resolve_expr!(rhs, env))
    }
    Conditional(~cond, ~then, ~otherwise) => {
      @parse.Conditional(
        ~cond=self.resolve_expr!(cond, env),
        ~then=self.resolve_expr!(then, env),
        ~otherwise=self.resolve_expr!(otherwise, env),
      )
    }
  }
}

fn resolve_stmt(self: VariableResolutionPass, stmt: @parse.Stmt, env: Env) -> @parse.Stmt!VariableResolutionError {
  match stmt {
    Return(expr) => @parse.Stmt::Return(self.resolve_expr!(expr, env))
    Expression(expr) => @parse.Stmt::Expression(self.resolve_expr!(expr, env))
    If(~cond, ~then, ~otherwise) => {
      @parse.Stmt::If(
        ~cond=self.resolve_expr!(cond, env),
        ~then=self.resolve_stmt!(then, env),
        ~otherwise=match otherwise {
          Some(stmt) => Some(self.resolve_stmt!(stmt, env))
          None => None
        },
      )
    }
    Labeled(~label, ~stmt) => {
      match env.goto_labels.get(label) {
        Some(unique_label) => @parse.Labeled(~label=unique_label, ~stmt=self.resolve_stmt!(stmt, env))
        None => raise UndeclaredLabel(label)
      }
    }
    Goto(label) => {
      match env.goto_labels.get(label) {
        Some(unique_label) => @parse.Stmt::Goto(unique_label)
        None => raise UndeclaredLabel(label)
      }
    }
    Compound(items) => {
      let block_env = env.child()
      @parse.Compound(self.resolve_block!(items, block_env))
    }
    Null => @parse.Null
  }
}

fn resolve_decl(self: VariableResolutionPass, decl: @parse.Decl, env: Env) -> @parse.Decl!VariableResolutionError {
  match decl {
    VariableDecl(~ty, ~name, ~value) => {
      let new_name = "var." + name + "." + self.next_var_id.to_string()

      match env.variables.declare?(name, new_name) {
        Ok(_) => ()
        Err(MemberAlreadyDeclared) => raise VariableAlreadyDeclared(name)
      }

      self.next_var_id += 1

      match value {
        Some(expr) => {
          let new_expr = self.resolve_expr!(expr, env)
          @parse.VariableDecl(~ty, ~name=new_name, ~value=Some(new_expr))
        }
        None => @parse.VariableDecl(~ty, ~name=new_name, ~value=None)
      }
    }
  }
}

fn resolve_block(self: VariableResolutionPass, block: Array[@parse.BlockItem], env: Env) -> Array[@parse.BlockItem]!VariableResolutionError {
  let new_block = []

  for item in block {
    match item {
      Decl(decl) => {
        let new_decl = self.resolve_decl!(decl, env)
        new_block.push(@parse.BlockItem::Decl(new_decl))
      }
      Stmt(stmt) => {
        let new_stmt = self.resolve_stmt!(stmt, env)
        new_block.push(@parse.BlockItem::Stmt(new_stmt))
      }
    }
  }

  new_block
}

fn collect_expr_labels(expr: @parse.Expr, labels: Map[String, String]) -> Unit {
  match expr {
    Variable(_) => ()
    Constant(_) => ()
    Unary(~op=_, ~expr) => collect_expr_labels(expr, labels)
    Binary(~op=_, ~lhs, ~rhs) => {
      collect_expr_labels(lhs, labels)
      collect_expr_labels(rhs, labels)
    }
    Assignment(~op=_, ~lhs, ~rhs) => {
      collect_expr_labels(lhs, labels)
      collect_expr_labels(rhs, labels)
    }
    Conditional(~cond, ~then, ~otherwise) => {
      collect_expr_labels(cond, labels)
      collect_expr_labels(then, labels)
      collect_expr_labels(otherwise, labels)
    }
  }
}

fn collect_block_labels(self: VariableResolutionPass, block: Array[@parse.BlockItem], labels: Map[String, String]) -> Unit!VariableResolutionError {
  for item in block {
    match item {
      Decl(_) => ()
      Stmt(stmt) => self.collect_stmt_labels!(stmt, labels)
    }
  }
}

fn collect_stmt_labels(self: VariableResolutionPass, stmt: @parse.Stmt, labels: Map[String, String]) -> Unit!VariableResolutionError {
  match stmt {
    Return(_) => ()
    Expression(_) => ()
    If(~cond, ~then, ~otherwise) => {
      collect_expr_labels(cond, labels)
      self.collect_stmt_labels!(then, labels)
      match otherwise {
        Some(stmt) => self.collect_stmt_labels!(stmt, labels)
        None => ()
      }
    }
    Labeled(~label, ~stmt) => {
      if labels.contains(label) {
        raise LabelAlreadyDeclared(label)
      }

      labels[label] = "goto." + label + "." + self.next_goto_label_id.to_string()
      self.collect_stmt_labels!(stmt, labels)
    }
    Goto(_) => ()
    Compound(items) => {
      self.collect_block_labels!(items, labels)
    }
    Null => ()
  }
}

fn collect_function_def_labels(self: VariableResolutionPass, func: @parse.FunctionDef, labels: Map[String, String]) -> Unit!VariableResolutionError {
  for item in func.body {
    match item {
      Decl(_) => ()
      Stmt(stmt) => self.collect_stmt_labels!(stmt, labels)
    }
  }
}

// Collect all the labels declared in this function (a goto can reference a label declared later in the function)
fn resolve_function_def(self: VariableResolutionPass, func: @parse.FunctionDef) -> @parse.FunctionDef!VariableResolutionError {
  let env = Env::new()
  self.collect_function_def_labels!(func, env.goto_labels)
  let new_body = self.resolve_block!(func.body, env)

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
