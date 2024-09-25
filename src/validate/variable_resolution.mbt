
pub struct VariableResolutionPass {
  mut next_var_id: Int
  mut next_goto_label_id: Int
  mut next_loop_label_id: Int
  mut next_switch_label_id: Int
}

pub fn VariableResolutionPass::new() -> VariableResolutionPass {
  { next_var_id: 0, next_goto_label_id: 0, next_loop_label_id: 0, next_switch_label_id: 0 }
}

pub type! VariableResolutionError {
  VariableAlreadyDeclared(String)
  UndeclaredVariable(String)
  InvalidLvalue(String)
  LabelAlreadyDeclared(String)
  UndeclaredLabel(String)
  BreakOutsideLoopOrSwitch
  ContinueOutsideLoop
  CaseOutsideSwitch
  DefaultOutsideSwitch
  CaseExprMustBeConstant
  DuplicateCase(@parse.Constant)
  MultipleDefaultInSwitch
  Unreachable(String)
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
  stack: ResolutionStack
}

fn Env::new() -> Env {
  { variables: Scope::new(), goto_labels: {}, stack: ResolutionStack::new() }
}

fn child(self: Env) -> Env {
  { variables: self.variables.child(), goto_labels: self.goto_labels, stack: self.stack }
}

fn push(self: Env, entry: ResolutionStackEntry) -> Unit {
  self.stack.push(entry)
}

fn pop(self: Env) -> Unit {
  let _ = self.stack.pop()
}

fn next_loop_label(self: VariableResolutionPass, prefix: String) -> String {
  let label_name = prefix + "." + self.next_loop_label_id.to_string()
  self.next_loop_label_id += 1
  label_name
}

fn next_switch_entry(self: VariableResolutionPass, prefix: String) -> String {
  let label_name = prefix + "." + self.next_switch_label_id.to_string()
  self.next_switch_label_id += 1
  label_name
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
    Return(expr) => Return(self.resolve_expr!(expr, env))
    Expression(expr) => Expression(self.resolve_expr!(expr, env))
    If(~cond, ~then, ~otherwise) => {
      If(
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
        Some(unique_label) => Labeled(~label=unique_label, ~stmt=self.resolve_stmt!(stmt, env))
        None => raise UndeclaredLabel(label)
      }
    }
    Goto(label) => {
      match env.goto_labels.get(label) {
        Some(unique_label) => Goto(unique_label)
        None => raise UndeclaredLabel(label)
      }
    }
    Compound(items) => {
      let block_env = env.child()
      Compound(self.resolve_block!(items, block_env))
    }
    Break(~label) => {
      match label {
        Some(_) => raise Unreachable("break with label before resolution")
        None => {
          match env.stack.peek() {
            Some(Loop(label) | Switch({ label, cases: _, has_default: _ })) => Break(~label=Some(label))
            None => raise BreakOutsideLoopOrSwitch
          }
        }
      }
    }
    Continue(~label) => {
      match label {
        Some(_) => raise Unreachable("continue with label before resolution")
        None => {
          match env.stack.get_innermost_loop_label() {
            Some(loop_label) => Continue(~label=Some(loop_label))
            None => raise ContinueOutsideLoop
          }
        }
      }
    }
    While(~cond, ~body, ~label=_) => {
      let loop_label = self.next_loop_label("while")
      env.push(Loop(loop_label))
      let cond = self.resolve_expr!(cond, env)
      let body = self.resolve_stmt!(body, env)
      env.pop()
      While(~cond, ~body, ~label=Some(loop_label))
    }
    DoWhile(~body, ~cond, ~label=_) => {
      let loop_label = self.next_loop_label("do_while")
      env.push(Loop(loop_label))
      let body = self.resolve_stmt!(body, env)
      let cond = self.resolve_expr!(cond, env)
      env.pop()
      DoWhile(~body, ~cond, ~label=Some(loop_label))
    }
    For(~init, ~cond, ~post, ~body, ~label=_) => {
      let for_env = env.child()
      let loop_label = self.next_loop_label("for")
      for_env.push(Loop(loop_label))

      let new_init: @parse.ForInit = match init {
        InitEmpty => InitEmpty
        InitDecl(decl) => {
          let new_decl = self.resolve_decl!(decl, for_env)
          InitDecl(new_decl)
        }
        InitExpr(expr) => {
          let new_expr = self.resolve_expr!(expr, for_env)
          InitExpr(new_expr)
        }
      }

      let new_cond = match cond {
        Some(expr) => Some(self.resolve_expr!(expr, for_env))
        None => None
      }

      let new_post = match post {
        Some(expr) => Some(self.resolve_expr!(expr, for_env))
        None => None
      }

      let body = self.resolve_stmt!(body, for_env)
      for_env.pop()

      For(~init=new_init, ~cond=new_cond, ~post=new_post, ~body, ~label=Some(loop_label))
    }
    Switch(~expr, ~body, ~label=_, ~cases_count=_) => {
      let switch_label = self.next_switch_entry("switch")
      let switch_entry = ResolutionStackSwitchEntry::new(switch_label)
      env.push(Switch(switch_entry))
      let new_expr = self.resolve_expr!(expr, env)
      let new_body = self.resolve_stmt!(body, env)
      env.pop()

      let cases_count = switch_entry.cases.size() + if switch_entry.has_default { 1 } else { 0 }

      Switch(~expr=new_expr, ~body=new_body, ~label=Some(switch_label), ~cases_count)
    }
    Case(~expr, ~body) => {
      let const = match expr {
        Constant(n) => n
        _ => raise CaseExprMustBeConstant
      }

      let switch_entry = match env.stack.get_innermost_switch_entry() {
        Some(entry) => entry
        None => raise CaseOutsideSwitch
      }

      if switch_entry.cases.contains(const) {
        raise DuplicateCase(const)
      }

      switch_entry.cases.insert(const)

      let new_expr = self.resolve_expr!(expr, env)
      let new_body = self.resolve_stmt!(body, env)

      Case(~expr=new_expr, ~body=new_body)
    }
    Default(~body) => {
      let switch_entry = match env.stack.get_innermost_switch_entry() {
        Some(entry) => entry
        None => raise DefaultOutsideSwitch
      }

      if switch_entry.has_default {
        raise MultipleDefaultInSwitch
      }

      switch_entry.has_default = true

      let new_body = self.resolve_stmt!(body, env)

      Default(~body=new_body)
    }
    Null => Null
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
    If(~cond=_, ~then, ~otherwise) => {
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
    Break(_) => ()
    Continue(_) => ()
    While(~cond=_, ~body, ~label=_) => {
      self.collect_stmt_labels!(body, labels)
    }
    DoWhile(~body, ~cond=_, ~label=_) => {
      self.collect_stmt_labels!(body, labels)
    }
    For(~init=_, ~cond=_, ~post=_, ~body, ~label=_) => {
      self.collect_stmt_labels!(body, labels)
    }
    Switch(~expr=_, ~body, ~label=_, ~cases_count=_) => {
      self.collect_stmt_labels!(body, labels)
    }
    Case(~expr=_, ~body) => {
      self.collect_stmt_labels!(body, labels)
    }
    Default(~body) => {
      self.collect_stmt_labels!(body, labels)
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
