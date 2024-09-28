
// A pass that resolves all the names and labels in the program
pub struct NameResolutionPass {
  mut next_var_id: Int
  mut next_goto_label_id: Int
  mut next_loop_label_id: Int
  mut next_switch_label_id: Int
}

pub fn NameResolutionPass::new() -> NameResolutionPass {
  { next_var_id: 0, next_goto_label_id: 0, next_loop_label_id: 0, next_switch_label_id: 0 }
}

pub type! NameResolutionError {
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
  UndeclaredFunction(String)
  DuplicateFunctionDeclaration(String)
  FunctionDefinitionShouldBeTopLevel(String)
  Unreachable(String)
} derive(Show)

fn is_lvalue(expr: @parse.Expr) -> Bool {
  match expr {
    Variable(_) => true
    _ => false
  }
}

enum IdentifierLinkage {
  // Internal
  External
}

struct ResolvedIdentifier {
  new_name: String
  linkage: IdentifierLinkage?
}

struct Env {
  identifiers: Scope[ResolvedIdentifier]
  goto_labels: Map[String, String]
  stack: ResolutionStack
}

fn Env::new() -> Env {
  { identifiers: Scope::new(), goto_labels: {}, stack: ResolutionStack::new() }
}

fn child(self: Env) -> Env {
  { identifiers: self.identifiers.child(), goto_labels: self.goto_labels, stack: self.stack }
}

fn push(self: Env, entry: ResolutionStackEntry) -> Unit {
  self.stack.push(entry)
}

fn pop(self: Env) -> Unit {
  let _ = self.stack.pop()
}

fn is_top_level(self: Env) -> Bool {
  self.identifiers.parent.is_empty()
}

fn next_loop_label(self: NameResolutionPass, prefix: String) -> String {
  let label_name = prefix + "." + self.next_loop_label_id.to_string()
  self.next_loop_label_id += 1
  label_name
}

fn next_goto_label(self: NameResolutionPass, prefix: String) -> String {
  let label_name = "goto." + prefix + "." + self.next_goto_label_id.to_string()
  self.next_goto_label_id += 1
  label_name
}

fn next_switch_entry(self: NameResolutionPass, prefix: String) -> String {
  let label_name = prefix + "." + self.next_switch_label_id.to_string()
  self.next_switch_label_id += 1
  label_name
}

fn resolve_expr(self: NameResolutionPass, expr: @parse.Expr, env: Env) -> @parse.Expr!NameResolutionError {
  match expr {
    Variable(name) => {
      match env.identifiers.resolve(name) {
        Some({ new_name, linkage: _ }) => @parse.Variable(new_name)
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
    Call(~name, ~args) => {
      match env.identifiers.resolve(name) {
        Some({ new_name, linkage: _ }) => {
          let new_args = []

          for arg in args {
            new_args.push(self.resolve_expr!(arg, env))
          }

          @parse.Call(~name=new_name, ~args=new_args)
        }
        None => raise UndeclaredFunction(name)
      }
    }
  }
}

fn resolve_stmt(self: NameResolutionPass, stmt: @parse.Stmt, env: Env) -> @parse.Stmt!NameResolutionError {
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
        Some(_) => raise NameResolutionError::Unreachable("break with label before resolution")
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
        Some(_) => raise NameResolutionError::Unreachable("continue with label before resolution")
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
          InitDecl(self.resolve_variable_decl!(decl, for_env))
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

fn resolve_variable_decl(self: NameResolutionPass, decl: @parse.VariableDecl, env: Env) -> @parse.VariableDecl!NameResolutionError {
  let { ty, name, value } = decl
  let new_name = "var." + name + "." + self.next_var_id.to_string()

  match env.identifiers.declare?(name, { new_name, linkage: None }) {
    Ok(_) => ()
    Err(MemberAlreadyDeclared) => raise VariableAlreadyDeclared(name)
  }

  self.next_var_id += 1

  match value {
    Some(expr) => {
      let new_expr = self.resolve_expr!(expr, env)
      { ty, name: new_name, value: Some(new_expr) }
    }
    None => { ty, name: new_name, value: None }
  }
}

fn resolve_decl(self: NameResolutionPass, decl: @parse.Decl, env: Env) -> @parse.Decl!NameResolutionError {
  match decl {
    VarDecl({ ty, name, value }) => {
      VarDecl(self.resolve_variable_decl!({ ty, name, value }, env))
    }
    FunDecl(decl) => {
      if not(decl.body.is_empty()) && not(env.is_top_level()) {
        raise FunctionDefinitionShouldBeTopLevel(decl.name)
      }

      FunDecl(self.resolve_function_decl!(decl, env))
    }
  }
}

fn resolved_param(self: NameResolutionPass, param: @parse.FunctionParam, env: Env) -> @parse.FunctionParam!NameResolutionError {
  let new_name = "param." + param.name + "." + self.next_var_id.to_string()

  match env.identifiers.declare?(param.name, { new_name, linkage: None }) {
    Ok(_) => ()
    Err(MemberAlreadyDeclared) => raise VariableAlreadyDeclared(param.name)
  }

  { ty: param.ty, name: new_name }
}

fn resolve_block(self: NameResolutionPass, block: Array[@parse.BlockItem], env: Env) -> Array[@parse.BlockItem]!NameResolutionError {
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

fn collect_block_labels(self: NameResolutionPass, block: Array[@parse.BlockItem], labels: Map[String, String]) -> Unit!NameResolutionError {
  for item in block {
    match item {
      Decl(_) => ()
      Stmt(stmt) => self.collect_stmt_labels!(stmt, labels)
    }
  }
}

fn collect_stmt_labels(self: NameResolutionPass, stmt: @parse.Stmt, labels: Map[String, String]) -> Unit!NameResolutionError {
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

      labels[label] = self.next_goto_label(label)
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

fn collect_function_def_labels(self: NameResolutionPass, func: @parse.FunctionDecl, labels: Map[String, String]) -> Unit!NameResolutionError {
  match func.body {
    None => ()
    Some(body) => self.collect_block_labels!(body, labels)
  }
}

// Collect all the labels declared in this function (a goto can reference a label declared later in the function)
fn resolve_function_decl(self: NameResolutionPass, func: @parse.FunctionDecl, env: Env) -> @parse.FunctionDecl!NameResolutionError {
  let { return_type, name, params, body } = func

  env.goto_labels.clear() // Reset the goto labels for this function
  self.collect_function_def_labels!(func, env.goto_labels)

  if env.identifiers.contains_local(name) {
    match env.identifiers.resolve(name) {
      Some({ new_name: _, linkage: None }) => raise DuplicateFunctionDeclaration(name)
      _ => ()
    }
  }

  env.identifiers.declare_shadowing(name, { new_name: name, linkage: Some(External) })

  let body_env = env.child()
  let new_params = []

  for param in params {
    new_params.push(self.resolved_param!(param, body_env))
  }

  let new_body = match body {
    Some(body) => {
      Some(self.resolve_block!(body, body_env))
    }
    None => None
  }

  { return_type, name, params: new_params, body: new_body }
}

pub fn resolved(self: NameResolutionPass, program: @parse.Program) -> @parse.Program!NameResolutionError {
  let functions = []
  let env = Env::new()

  for func in program.functions {
    let new_func = self.resolve_function_decl!(func, env)
    functions.push(new_func)
  }

  { functions, }
}
