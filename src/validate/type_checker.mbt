using @parse { 
  type UnaryOp, type BinaryOp, type Type, type Constant, type Expr, type Program,
  type VariableDecl, type FunctionDecl, type Decl, type BlockItem, type Stmt,
  type ForInit, type FunctionParam
}

pub(all) enum StaticInit {
  IntInit(Int); LongInit(Int64)
  UIntInit(UInt); ULongInit(UInt64)
  DoubleInit(Double)
} derive(Show, Eq)

pub enum InitialValue {
  Tentative; Initial(StaticInit); NoInitializer
}

fn convert_to(val: Constant, ty: Type) -> StaticInit raise TypeCheckerError {
  match ty {
    Int => match val {
      IntConst(n) => IntInit(n)
      LongConst(n) => IntInit(n.to_int())
      UnsignedIntConst(n) => IntInit(n.reinterpret_as_int())
      UnsignedLongConst(n) => IntInit(n.to_int())
      DoubleConst(x) => IntInit(x.to_int())
    }
    Long => match val {
      IntConst(n) => LongInit(n.to_int64())
      LongConst(n) => LongInit(n)
      UnsignedIntConst(n) => LongInit(n.to_int64())
      UnsignedLongConst(n) => LongInit(n.reinterpret_as_int64())
      DoubleConst(x) => LongInit(x.reinterpret_as_int64())
    }
    UnsignedInt => match val {
      IntConst(n) => UIntInit(n.reinterpret_as_uint())
      LongConst(n) => UIntInit(n.reinterpret_as_uint64().to_uint())
      UnsignedIntConst(n) => UIntInit(n)
      UnsignedLongConst(n) => UIntInit(n.to_uint())
      DoubleConst(x) => UIntInit(x.reinterpret_as_uint64().to_uint())
    }
    UnsignedLong => match val {
      IntConst(n) => ULongInit(n.to_uint64())
      LongConst(n) => ULongInit(n.reinterpret_as_uint64())
      UnsignedIntConst(n) => ULongInit(n.to_uint64())
      UnsignedLongConst(n) => ULongInit(n)
      DoubleConst(x) => ULongInit(x.reinterpret_as_uint64())
    }
    Double => match val {
      IntConst(n) => DoubleInit(n.to_double())
      LongConst(n) => DoubleInit(n.to_double())
      UnsignedIntConst(n) => DoubleInit(n.to_int64().to_double())
      UnsignedLongConst(n) => DoubleInit(n.to_double())
      DoubleConst(x) => DoubleInit(x)
    }
    FunType(_) => raise TypeCheckerError::Unreachable("non-numerical static variable initializer")
  }
}

pub(all) enum IdentifierAttributes {
  FunAttr(defined~: Bool, global~: Bool)
  StaticAttr(init~: InitialValue, global~: Bool)
  LocalAttr
}

pub(all) enum SymbolTableEntry {
  Variable(ty~: Type, attributes~: IdentifierAttributes)
  Function(ty~: Type, attributes~: IdentifierAttributes)
}

pub fn SymbolTableEntry::ty(self: SymbolTableEntry) -> Type {
  match self {
    Variable(ty~, attributes=_) | Function(ty~, attributes=_) => ty
  }
}

pub struct FunctionStackEntry {
  name: String
  return_ty: Type
}

pub struct TypeChecker {
  symbols: Map[String, SymbolTableEntry]
  function_stack: Array[FunctionStackEntry]
}

pub suberror TypeCheckerError {
  IncompatibleFunctionDeclarations(name~: String, old_decl~: Type, new_decl~: Type)
  FunctionDefinedMoreThanOnce(String)
  FunctionNameUsedAsVariable(String)
  IncorrectArgumentCount(name~: String, expected~: Int, actual~: Int)
  VariableUsedAsFunctionName(String)
  UndefinedSymbol(String)
  StaticFunctionDeclFollowsNonStatic(String)
  FileScopeVarNonConstantInitializer(variable~: String, init~: Expr)
  VariableRedeclaredWithDifferentType(String)
  ConflictingVariableLinkage(String)
  ConflictingGlobalVariableDefinition(String)
  InitializerOnLocalExternVarDecl(String)
  NonConstantInitializerOnLocalStaticVar(variable~: String, init~: Expr)
  StorageClassNotPermittedInForLoopInitializer
  IllegalUnaryOperatorForDouble(UnaryOp)
  IllegalBinaryOperatorForDouble(BinaryOp)
  Unreachable(String)
} derive(Show)

pub fn TypeChecker::new() -> TypeChecker {
  { symbols: {}, function_stack: [] }
}

fn TypeChecker::typed_file_scope_variable_decl(self: TypeChecker, decl: VariableDecl) -> VariableDecl raise TypeCheckerError {
  let mut initial_value = match decl.value {
    Some(Constant(val~, ty=_)) => Initial(convert_to(val, decl.ty))
    None => match decl.storage_class {
      Some(Extern) => NoInitializer
      _ => Tentative
    }
    Some(init) => raise FileScopeVarNonConstantInitializer(variable=decl.name, init~)
  }

  let mut is_global = decl.storage_class != Some(Static)

  match self.symbols.get(decl.name) {
    Some(Variable(ty~, attributes~) | Function(ty~, attributes~)) => {
      if decl.ty != ty {
        raise VariableRedeclaredWithDifferentType(decl.name)
      }

      match attributes {
        StaticAttr(init=prev_init, global=prev_global) => {
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

  self.symbols[decl.name] = Variable(ty=decl.ty, attributes=StaticAttr(init=initial_value, global=is_global))

  decl
}

fn TypeChecker::typed_local_variable_decl(self: TypeChecker, decl: VariableDecl) -> VariableDecl raise TypeCheckerError {
  match decl.storage_class {
    Some(Extern) => {
      if not(decl.value is None) {
        raise InitializerOnLocalExternVarDecl(decl.name)
      }

      match self.symbols.get(decl.name) {
        Some(Variable(ty~, attributes=_) | Function(ty~, attributes=_)) => {
          if ty != decl.ty {
            raise VariableRedeclaredWithDifferentType(decl.name)
          }
        }
        None => {
          self.symbols[decl.name] = Variable(ty=decl.ty, attributes=StaticAttr(init=NoInitializer, global=true))
        }
      }

      decl
    }
    Some(Static) => {
      let initial_value = match decl.value {
        Some(Constant(val~, ty=_)) => Initial(convert_to(val, decl.ty))
        None => Initial(match decl.ty {
          Int => IntInit(0)
          Long => LongInit(0)
          _ => raise TypeCheckerError::Unreachable("non-integer static variable initializer")
        })
        Some(init) => raise NonConstantInitializerOnLocalStaticVar(variable=decl.name, init~)
      }

      self.symbols[decl.name] = Variable(ty=decl.ty, attributes=StaticAttr(init=initial_value, global=false))

      decl
    }
    None => {
      self.symbols[decl.name] = Variable(ty=decl.ty, attributes=LocalAttr)

      let value = match decl.value {
        Some(value) => Some(cast(self.typed_expr(value), decl.ty))
        None => None
      }

      { ty: decl.ty, name: decl.name, value, storage_class: decl.storage_class }
    }
  }
}

fn TypeChecker::typed_variable_declaration(self: TypeChecker, decl: VariableDecl, scope~: DeclarationScope) -> VariableDecl raise TypeCheckerError {
  match scope {
    FileScope => self.typed_file_scope_variable_decl(decl)
    BlockScope => self.typed_local_variable_decl(decl)
  }
}

fn TypeChecker::check_function_declaration(self: TypeChecker, decl: FunctionDecl) -> FunctionDecl raise TypeCheckerError {
  let fun_type = decl.to_type()
  let has_body = not(decl.body is None)
  let mut already_defined = false
  let mut is_global = decl.storage_class != Some(Static)

  match self.symbols.get(decl.name) {
    Some(Variable(ty=prev_ty, attributes~) | Function(ty=prev_ty, attributes~)) => {
      if prev_ty != fun_type {
        raise IncompatibleFunctionDeclarations(name=decl.name, old_decl=prev_ty, new_decl=fun_type)
      }

      match attributes {
        FunAttr(defined=prev_defined, global=prev_global) => {
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
    ty=fun_type,
    attributes=FunAttr(defined=already_defined || has_body, global=is_global),
  )

  match decl.body {
    Some(body) => {
      self.function_stack.push({ name: decl.name, return_ty: decl.return_ty })

      for param in decl.params {
        self.symbols[param.name] = Variable(ty=param.ty, attributes=LocalAttr)
      }

      let body = self.typed_block(body)

      let _ = self.function_stack.pop()

      {
        return_ty: decl.return_ty,
        name: decl.name,
        params: decl.params,
        body: Some(body),
        storage_class: decl.storage_class,
      }
    }
    None => decl
  }
}

pub fn TypeChecker::typed_decl(self: TypeChecker, decl: Decl, scope~: DeclarationScope) -> Decl raise TypeCheckerError {
  match decl {
    VarDecl(decl) => VarDecl(self.typed_variable_declaration(decl, scope~))
    FunDecl(decl) => FunDecl(self.check_function_declaration(decl))
  }
}

pub fn TypeChecker::typed_block(self: TypeChecker, block: Array[BlockItem]) -> Array[BlockItem] raise TypeCheckerError {
  let typed_block = []

  for item in block {
    let typed_item: BlockItem = match item {
      Stmt(stmt) => Stmt(self.typed_stmt(stmt))
      Decl(decl) => Decl(self.typed_decl(decl, scope=BlockScope))
    }

    typed_block.push(typed_item)
  }

  typed_block
}

fn get_common_type(ty1: Type, ty2: Type) -> Type {
  if ty1 == ty2 {
    return ty1
  }

  if ty1 == Double || ty2 == Double {
    return Double
  }

  let ty1_size = ty1.byte_size()
  let ty2_size = ty2.byte_size()

  if ty1_size == ty2_size {
    if ty1.is_signed() {
      return ty2
    } else {
      return ty1
    }
  }
  
  if ty1_size > ty2_size { ty1 } else { ty2 }
}

fn is_illegal_binop_for_double(op: BinaryOp) -> Bool {
  match op {
    Rem | BitwiseAnd | BitwiseOr | BitwiseXor | ShiftLeft | ShiftRight => true
    _ => false
  }
}

fn cast(expr: Expr, ty: Type) -> Expr {
  if expr.ty() == ty {
    expr
  } else {
    Cast(target_ty=ty, expr~, ty=Some(ty))
  }
}

pub fn TypeChecker::typed_expr(self: TypeChecker, expr: Expr) -> Expr raise TypeCheckerError {
  match expr {
    Constant(val~, ty=_) => match val {
      IntConst(_) => Constant(val~, ty=Some(Int))
      LongConst(_) => Constant(val~, ty=Some(Long))
      UnsignedIntConst(_) => Constant(val~, ty=Some(UnsignedInt))
      UnsignedLongConst(_) => Constant(val~, ty=Some(UnsignedLong))
      DoubleConst(_) => Constant(val~, ty=Some(Double))
    }
    Variable(name~, ty=_) => {
      match self.symbols.get(name) {
        Some(Variable(ty=var_ty, attributes=_)) => Variable(name=name, ty=Some(var_ty))
        Some(Function(_)) => raise FunctionNameUsedAsVariable(name)
        None => raise UndefinedSymbol(name)
      }
    }
    Cast(target_ty~, expr~, ty=_) => {
      let expr = self.typed_expr(expr)
      Cast(target_ty~, expr~, ty=Some(target_ty))
    }
    Unary(op~, expr~, ty=_) => {
      let expr = self.typed_expr(expr)
      let expr_ty = expr.ty()

      let ty: Type = match op {
        Not => Int
        _ => expr_ty
      }

      if op == Complement && expr_ty == Double {
        raise IllegalUnaryOperatorForDouble(op)
      }

      Unary(op~, expr~, ty=Some(ty))
    }
    Binary(op~, lhs~, rhs~, ty=_) => {
      let lhs = self.typed_expr(lhs)
      let rhs = self.typed_expr(rhs)
      let common_ty = get_common_type(lhs.ty(), rhs.ty())

      if common_ty == Double && is_illegal_binop_for_double(op) {
        raise IllegalBinaryOperatorForDouble(op)
      }

      match op {
        ShiftLeft | ShiftRight => Binary(op~, lhs~, rhs~, ty=Some(lhs.ty()))
        LogicalAnd | LogicalOr => Binary(op~, lhs~, rhs~, ty=Some(Int))
        _ => {
          let lhs = cast(lhs, common_ty)
          let rhs = cast(rhs, common_ty)

          match op {
            Add | Sub | Mul | Div | Rem | BitwiseAnd | BitwiseOr | BitwiseXor => {
              Binary(op~, lhs~, rhs~, ty=Some(common_ty))
            }
            Lss | Leq | Gtr | Geq | Equ | Neq | LogicalAnd | LogicalOr | Ternary => {
              Binary(op~, lhs~, rhs~, ty=Some(Int))
            }
            _ => raise TypeCheckerError::Unreachable("unhandled binary operator")
          }
        }
      }
    }
    Assignment(op~, lhs~, rhs~, ty=_) => {
      let lhs = self.typed_expr(lhs)
      let rhs = self.typed_expr(rhs)
      let common_ty = get_common_type(lhs.ty(), rhs.ty())

      if common_ty == Double {
        match op.binop() {
          Some(op) => {
            if is_illegal_binop_for_double(op) {
              raise IllegalBinaryOperatorForDouble(op)
            }
          }
          None => ()
        }
      }

      match op {
        AssignShiftLeft | AssignShiftRight => {
          Assignment(op~, lhs~, rhs~, ty=Some(lhs.ty()))
        }
        _ => {
          Assignment(op~, lhs~, rhs=cast(rhs, common_ty), ty=Some(common_ty))
        }
      }
    }
    Conditional(cond~, then~, otherwise~, ty=_) => {
      let cond = self.typed_expr(cond)
      let then = self.typed_expr(then)
      let otherwise = self.typed_expr(otherwise)
      let common_ty = get_common_type(then.ty(), otherwise.ty())
      let then = cast(then, common_ty)
      let otherwise = cast(otherwise, common_ty)
      Conditional(cond~, then~, otherwise~, ty=Some(common_ty))
    }
    Call(name~, args~, ty=_) => {
      match self.symbols.get(name) {
        Some(Function(ty=FunType(params~, ret~), attributes=_)) => {
          if args.length() != params.length() {
            raise IncorrectArgumentCount(name=name, expected=params.length(), actual=args.length())
          }
          
          let converted_args = []

          for i in 0..<args.length() {
            let arg = self.typed_expr(args[i])
            let param_ty = params[i]
            let arg_ty = arg.ty()
            let common_ty = get_common_type(param_ty, arg_ty)
            converted_args.push(cast(arg, common_ty))
          }

          Call(name=name, args=converted_args, ty=Some(ret))
        }
        Some(Function(_)) => raise TypeCheckerError::Unreachable("Function with incorrect type")
        Some(Variable(_)) => raise VariableUsedAsFunctionName(name)
        None => raise UndefinedSymbol(name)
      }
    }
  }
}

pub fn TypeChecker::typed_stmt(self: TypeChecker, stmt: Stmt) -> Stmt raise TypeCheckerError {
  match stmt {
    Return(expr) => {
      match self.function_stack.last() {
        Some({ name: _, return_ty }) => {
          let expr = self.typed_expr(expr)
          Return(cast(expr, return_ty))
        }
        None => raise TypeCheckerError::Unreachable("return statement outside of function")
      }
    }
    Expression(expr) => Expression(self.typed_expr(expr))
    If(cond~, then~, otherwise~) => {
      let cond = self.typed_expr(cond)
      let then = self.typed_stmt(then)

      let otherwise = match otherwise {
        Some(stmt) => Some(self.typed_stmt(stmt))
        None => None
      }

      If(cond~, then~, otherwise~)
    }
    Labeled(label~, stmt~) => {
      Labeled(label~, stmt=self.typed_stmt(stmt))
    }
    Compound(block) => {
      Compound(self.typed_block(block))
    }
    DoWhile(body~, cond~, label~) => {
      let body = self.typed_stmt(body)
      let cond = self.typed_expr(cond)
      DoWhile(body~, cond~, label~)
    }
    While(cond~, body~, label~) => {
      let cond = self.typed_expr(cond)
      let body = self.typed_stmt(body)
      While(cond~, body~, label~)
    }
    For(init~, cond~, post~, body~, label~) => {
      let init: ForInit = match init {
        InitDecl(decl) => match decl.storage_class {
          Some(_) => raise StorageClassNotPermittedInForLoopInitializer
          None => InitDecl(self.typed_variable_declaration(decl, scope=BlockScope))
        }
        InitExpr(expr) => InitExpr(self.typed_expr(expr))
        InitEmpty => InitEmpty
      }

      let cond = match cond {
        Some(expr) => Some(self.typed_expr(expr))
        None => None
      }

      let post = match post {
        Some(expr) => Some(self.typed_expr(expr))
        None => None
      }

      let body = self.typed_stmt(body)
      
      For(init~, cond~, post~, body~, label~)
    }
    Break(label~) => Break(label~)
    Continue(label~) => Continue(label~) 
    Goto(label) => Goto(label)
    Switch(expr~, body~, label~, cases_count~) => {
      let expr = self.typed_expr(expr)
      let body = self.typed_stmt(body)
      Switch(expr~, body~, label~, cases_count~)
    }
    Case(expr~, body~) => {
      let expr = self.typed_expr(expr)
      let body = self.typed_stmt(body)
      Case(expr~, body~)
    }
    Default(body~) => Default(body=self.typed_stmt(body))
    Null => Null
  }
}

pub fn TypeChecker::typed_program(self: TypeChecker, program: Program) -> Program raise TypeCheckerError {
  let typed_decls = []

  for decl in program.decls {
    let typed = self.typed_decl(decl, scope=FileScope)
    typed_decls.push(typed)
  }

  { decls: typed_decls }
}
