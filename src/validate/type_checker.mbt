
pub enum InitialValue {
  Tentative; Initial(Int); NoInitializer
}

pub enum IdentifierAttributes {
  FunAttr(~defined: Bool, ~global: Bool)
  StaticAttr(~init: InitialValue, ~global: Bool)
  LocalAttr
}

pub enum SymbolTableEntry {
  Variable(~ty: @parse.Type, ~attributes: IdentifierAttributes)
  Function(~ty: @parse.Type, ~attributes: IdentifierAttributes)
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
  StaticFunctionDeclFollowsNonStatic(String)
  FileScopeVarNonConstantInitializer(~variable: String, ~init: @parse.Expr)
  FunctionRedeclaredAsVariable(String)
  ConflictingVariableLinkage(String)
  ConflictingGlobalVariableDefinition(String)
  InitializerOnLocalExternVarDecl(String)
  NonConstantInitializerOnLocalStaticVar(~variable: String, ~init: @parse.Expr)
  StorageClassNotPermittedInForLoopInitializer
  Unreachable(String)
} derive(Show)

pub fn TypeChecker::new() -> TypeChecker {
  { symbols: {} }
}

fn check_file_scope_variable_decl(self: TypeChecker, decl: @parse.VariableDecl) -> Unit!TypeCheckerError {
  let mut initial_value = match decl.value {
    Some(Constant(IntConst(n))) => Initial(n)
    None => match decl.storage_class {
      Some(Extern) => NoInitializer
      _ => Tentative
    }
    Some(init) => raise FileScopeVarNonConstantInitializer(~variable=decl.name, ~init)
  }

  let mut is_global = decl.storage_class != Some(Static)

  match self.symbols.get(decl.name) {
    Some(Variable(~ty, ~attributes) | Function(~ty, ~attributes)) => {
      if ty != Int {
        raise FunctionRedeclaredAsVariable(decl.name)
      }

      match attributes {
        StaticAttr(~init=prev_init, ~global=prev_global) => {
          if decl.storage_class == Some(Extern) {
            is_global = prev_global
          } else if prev_global != is_global {
            raise ConflictingVariableLinkage(decl.name)
          }

          match (prev_init, initial_value) {
            (Initial(_), Initial(_)) => raise ConflictingGlobalVariableDefinition(decl.name)
            (Initial(_), _) => {
              initial_value = prev_init
            }
            (Tentative, Tentative | NoInitializer) => {
              initial_value = Tentative
            }
            (_, Initial(_)) | (NoInitializer, _) => ()
          }
        }
        _ => raise TypeCheckerError::Unreachable("file scope variable previously declared as local")
      }
    }
    None => ()
  }

  self.symbols[decl.name] = Variable(~ty=Int, ~attributes=StaticAttr(~init=initial_value, ~global=is_global))
}

fn check_local_variable_decl(self: TypeChecker, decl: @parse.VariableDecl) -> Unit!TypeCheckerError {
  match decl.storage_class {
    Some(Extern) => {
      if not(decl.value.is_empty()) {
        raise InitializerOnLocalExternVarDecl(decl.name)
      }

      match self.symbols.get(decl.name) {
        Some(Variable(~ty, ~attributes=_) | Function(~ty, ~attributes=_)) => {
          if ty != Int {
            raise FunctionRedeclaredAsVariable(decl.name)
          }
        }
        None => {
          self.symbols[decl.name] = Variable(~ty=Int, ~attributes=StaticAttr(~init=NoInitializer, ~global=true))
        }
      }
    }
    Some(Static) => {
      let initial_value = match decl.value {
        Some(Constant(IntConst(n))) => Initial(n)
        None => Initial(0)
        Some(init) => raise NonConstantInitializerOnLocalStaticVar(~variable=decl.name, ~init)
      }

      self.symbols[decl.name] = Variable(~ty=Int, ~attributes=StaticAttr(~init=initial_value, ~global=false))
    }
    None => {
      self.symbols[decl.name] = Variable(~ty=Int, ~attributes=LocalAttr)

      match decl.value {
        Some(value) => self.check_expr!(value)
        None => ()
      }
    }
  }
}

fn check_variable_declaration(self: TypeChecker, decl: @parse.VariableDecl, ~scope: DeclarationScope) -> Unit!TypeCheckerError {
  match scope {
    FileScope => self.check_file_scope_variable_decl!(decl)
    BlockScope => self.check_local_variable_decl!(decl)
  }
}

fn check_function_declaration(self: TypeChecker, decl: @parse.FunctionDecl) -> Unit!TypeCheckerError {
  let fun_type = @parse.FunType(~param_count=decl.params.length())
  let has_body = not(decl.body.is_empty())
  let mut already_defined = false
  let mut is_global = decl.storage_class != Some(Static)

  match self.symbols.get(decl.name) {
    Some(Variable(~ty=prev_ty, ~attributes) | Function(~ty=prev_ty, ~attributes)) => {
      if prev_ty != fun_type {
        raise IncompatibleFunctionDeclarations(~name=decl.name, ~old_decl=prev_ty, ~new_decl=fun_type)
      }

      match attributes {
        FunAttr(~defined=prev_defined, ~global=prev_global) => {
          already_defined = prev_defined

          if already_defined && has_body {
            raise FunctionDefinedMoreThanOnce(decl.name)
          }

          if prev_global && decl.storage_class == Some(Static) {
            raise StaticFunctionDeclFollowsNonStatic(decl.name)
          }

          is_global = prev_global
        }
        _ => raise TypeCheckerError::Unreachable("function without function attributes")
      }
    }
    None => ()
  }

  self.symbols[decl.name] = Function(
    ~ty=fun_type,
    attributes=FunAttr(~defined=already_defined || has_body, ~global=is_global),
  )

  match decl.body {
    Some(body) => {
      for param in decl.params {
        self.symbols[param.name] = Variable(~ty=param.ty, ~attributes=LocalAttr)
      }

      self.check_block!(body)
    }
    None => ()
  }
}

pub fn check_decl(self: TypeChecker, decl: @parse.Decl, ~scope: DeclarationScope) -> Unit!TypeCheckerError {
  match decl {
    VarDecl(decl) => self.check_variable_declaration!(decl, ~scope)
    FunDecl(decl) => self.check_function_declaration!(decl)
  }
}

pub fn check_block(self: TypeChecker, block: Array[@parse.BlockItem]) -> Unit!TypeCheckerError {
  for item in block {
    match item {
      Stmt(stmt) => self.check_stmt!(stmt)
      Decl(decl) => self.check_decl!(decl, ~scope=BlockScope)
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
      Some(Function(~ty=@parse.FunType(~param_count), attributes=_)) => {
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
        InitDecl(decl) => match decl.storage_class {
          Some(_) => raise StorageClassNotPermittedInForLoopInitializer
          None => self.check_variable_declaration!(decl, ~scope=BlockScope)
        }
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
  for decl in program.decls {
    self.check_decl!(decl, ~scope=FileScope)
  }
}
