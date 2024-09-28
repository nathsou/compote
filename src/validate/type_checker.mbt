
pub enum SymbolTableEntry {
  Variable(@parse.Type)
  Function(~ty: @parse.Type, ~defined: Bool)
}

pub struct TypeChecker {
  symbols: Map[String, SymbolTableEntry]
}

pub type! TypeCheckerError {
  IncompatibleFunctionDeclarations(~name: String, ~old_decl: @parse.Type, ~new_decl: @parse.Type)
  FunctionDefinedMoreThanOnce(String)
  FunctionNameUsedAsVariable(String)
  IncorrectArgumentCount(~name: String, ~expected: Int, ~actual: Int)
  VariableUsedAsFunctionName(String)
  UndefinedSymbol(String)
  Unreachable(String)
} derive(Show)

pub fn TypeChecker::new() -> TypeChecker {
  { symbols: {} }
}

pub fn check_variable_declaration(self: TypeChecker, decl: @parse.VariableDecl) -> Unit!TypeCheckerError {
  self.symbols[decl.name] = Variable(decl.ty)

  match decl.value {
    Some(value) => check_expr!(self, value)
    None => ()
  }
}

pub fn check_function_declaration(self: TypeChecker, decl: @parse.FunctionDecl) -> Unit!TypeCheckerError {
  let fun_type = @parse.FunType(~param_count=decl.params.length())
  let has_body = not(decl.body.is_empty())
  let mut already_defined = false

  match self.symbols.get(decl.name) {
    Some(Function(~ty, ~defined)) => {
      if ty != fun_type {
        raise IncompatibleFunctionDeclarations(~name=decl.name, ~old_decl=ty, ~new_decl=fun_type)
      }

      already_defined = defined

      if already_defined && has_body {
        raise FunctionDefinedMoreThanOnce(decl.name)
      }
    }
    _ => ()
  }

  self.symbols[decl.name] = Function(~ty=fun_type, ~defined=already_defined || has_body)

  match decl.body {
    Some(body) => {
      for param in decl.params {
        self.symbols[param.name] = Variable(param.ty)
      }

      self.check_block!(body)
    }
    None => ()
  }
}

pub fn check_decl(self: TypeChecker, decl: @parse.Decl) -> Unit!TypeCheckerError {
  match decl {
    VarDecl(decl) => check_variable_declaration!(self, decl)
    FunDecl(decl) => check_function_declaration!(self, decl)
  }
}

pub fn check_block(self: TypeChecker, block: Array[@parse.BlockItem]) -> Unit!TypeCheckerError {
  for item in block {
    match item {
      Stmt(stmt) => check_stmt!(self, stmt)
      Decl(decl) => check_decl!(self, decl)
    }
  }
}

pub fn check_expr(self: TypeChecker, expr: @parse.Expr) -> Unit!TypeCheckerError {
  match expr {
    Constant(_) => ()
    Variable(name) => {
      match self.symbols.get(name) {
        Some(Variable(_)) => ()
        Some(Function(_)) => raise FunctionNameUsedAsVariable(name)
        None => raise UndefinedSymbol(name)
      }
    }
    Unary(~op=_, ~expr) => {
      check_expr!(self, expr)
    }
    Binary(~op=_, ~lhs, ~rhs) => {
      check_expr!(self, lhs)
      check_expr!(self, rhs)
    }
    Assignment(~op=_, ~lhs, ~rhs) => {
      check_expr!(self, lhs)
      check_expr!(self, rhs)
    }
    Conditional(~cond, ~then, ~otherwise) => {
      check_expr!(self, cond)
      check_expr!(self, then)
      check_expr!(self, otherwise)
    }
    Call(~name, ~args) => {
      match self.symbols.get(name) {
      Some(Function(~ty=@parse.FunType(~param_count), ~defined=_)) => {
          if args.length() != param_count {
            raise IncorrectArgumentCount(~name=name, ~expected=param_count, ~actual=args.length())
          }

          for arg in args {
            self.check_expr!(arg)
          }
        }
        Some(Function(_)) => raise TypeCheckerError::Unreachable("Function with incorrect type")
        Some(Variable(_)) => raise VariableUsedAsFunctionName(name)
        None => raise UndefinedSymbol(name)
      }
    }
  }
}

pub fn check_stmt(self: TypeChecker, stmt: @parse.Stmt) -> Unit!TypeCheckerError {
  match stmt {
    Return(expr) => {
      self.check_expr!(expr)
    }
    Expression(expr) => {
      self.check_expr!(expr)
    }
    If(~cond, ~then, ~otherwise) => {
      self.check_expr!(cond)
      self.check_stmt!(then)

      match otherwise {
        Some(stmt) => self.check_stmt!(stmt)
        None => ()
      }
    }
    Labeled(~label=_, ~stmt) => {
      self.check_stmt!(stmt)
    }
    Compound(block) => {
      self.check_block!(block)
    }
    DoWhile(~body, ~cond, ~label=_) => {
      self.check_stmt!(body)
      self.check_expr!(cond)
    }
    While(~cond, ~body, ~label=_) => {
      self.check_expr!(cond)
      self.check_stmt!(body)
    }
    For(~init, ~cond, ~post, ~body, ~label=_) => {
      match init {
        InitDecl(decl) => self.check_variable_declaration!(decl)
        InitExpr(expr) => self.check_expr!(expr)
        _ => ()
      }

      match cond {
        Some(expr) => self.check_expr!(expr)
        None => ()
      }

      match post {
        Some(expr) => self.check_expr!(expr)
        None => ()
      }

      self.check_stmt!(body)
    }
    Break(~label=_) => ()
    Continue(~label=_) => ()
    Goto(_) => ()
    Switch(~expr, ~body, ~label=_, ~cases_count=_) => {
      self.check_expr!(expr)
      self.check_stmt!(body)
    }
    Case(~expr, ~body) => {
      self.check_expr!(expr)
      self.check_stmt!(body)
    }
    Default(~body) => {
      self.check_stmt!(body)
    }
    Null => ()
  }
}

pub fn check_program(self: TypeChecker, program: @parse.Program) -> Unit!TypeCheckerError {
  for decl in program.functions {
    self.check_function_declaration!(decl)
  }
}
