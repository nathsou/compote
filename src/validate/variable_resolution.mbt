
pub struct VariableResolutionPass {
  mut next_var_id: Int
}

pub fn VariableResolutionPass::new() -> VariableResolutionPass {
  { next_var_id: 0 }
}

pub type! VariableResolutionError {
  VariableAlreadyDeclared(String)
  UndeclaredVariable(String)
  InvalidLvalue(String)
} derive(Show)

fn is_lvalue(expr: @parse.Expr) -> Bool {
  match expr {
    Variable(_) => true
    _ => false
  }
}

fn resolve_expr(self: VariableResolutionPass, decl: @parse.Expr, scope: Map[String, String]) -> @parse.Expr!VariableResolutionError {
  match decl {
    Variable(name) => {
      match scope.get(name) {
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
  }
}

fn resolve_stmt(self: VariableResolutionPass, stmt: @parse.Stmt, scope: Map[String, String]) -> @parse.Stmt!VariableResolutionError {
  match stmt {
    Return(expr) => @parse.Stmt::Return(self.resolve_expr!(expr, scope))
    Expression(expr) => @parse.Stmt::Expression(self.resolve_expr!(expr, scope))
    Null => @parse.Null
  }
}

fn resolve_decl(self: VariableResolutionPass, decl: @parse.Decl, scope: Map[String, String]) -> @parse.Decl!VariableResolutionError {
  match decl {
    VariableDecl(~ty, ~name, ~value) => {
      if scope.contains(name) {
        raise VariableAlreadyDeclared(name)
      }

      let new_name = "var." + name + "." + self.next_var_id.to_string()
      scope[name] = new_name
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

fn resolve_function_dev(self: VariableResolutionPass, func: @parse.FunctionDef) -> @parse.FunctionDef!VariableResolutionError {
  let scope = {}
  let new_body = []

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
    let new_func = self.resolve_function_dev!(func)
    functions.push(new_func)
  }

  { functions, }
}
