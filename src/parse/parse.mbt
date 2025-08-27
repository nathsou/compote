pub struct Parser {
  tokens: Array[Token]
  mut index: Int
}

pub suberror ParserError {
  UnexpectedToken(expected~: Token, received~: Token)
  ExpectedIdentifier(Token)
  ExpectedExpr(Token)
  EmptyParamListNotSupportedUseVoidInstead
  TrailingComma
  InvalidTypeSpecifier(Array[Specifier])
  InvalidStorageClassSpecifier
} derive(Show)

pub fn Parser::new(tokens: Array[Token]) -> Parser {
  { tokens, index: 0}
}

fn peek(self: Parser) -> Token {
  self.tokens[self.index]
}

fn lookahead(self: Parser, n: Int) -> Token {
  self.tokens[self.index + n]
}

fn advance(self: Parser) -> Unit {
  self.index += 1
}

fn expect(self: Parser, expected: Token) -> Unit raise ParserError {
  let token = self.peek()

  if token == expected {
    self.advance()
  } else {
    raise UnexpectedToken(expected~, received=token)
  }
}

fn matches(self: Parser, token: Token) -> Bool {
  self.peek() == token
}

fn matches_predicate(self: Parser, predicate: (Token) -> Bool) -> Bool {
  predicate(self.peek())
}

fn consumes(self: Parser, token: Token) -> Bool {
  if self.matches(token) {
    self.advance()
    true
  } else {
    false
  }
}

fn parse_identifier(self: Parser) -> String raise ParserError {
  let token = self.peek()

  match token {
    Identifier(name) => {
      self.advance()
      name
    }
    _ => raise ExpectedIdentifier(token)
  }
}

fn has_duplicate_specifiers(specifiers: Array[Specifier]) -> Bool {
  let specifiers_counts = {}

  for specifier in specifiers {
    let count = specifiers_counts.get_or_default(specifier, 0);
    specifiers_counts[specifier] = count + 1
  }

  specifiers_counts.values().any(fn(count) { count > 1 })
}

fn parse_type_specifiers(specifiers: Array[Specifier], allow_storage_classes~: Bool) -> Type raise ParserError {
  if
    specifiers.is_empty() ||
    ((specifiers.contains(TypeSpecifier(Unsigned)) && specifiers.contains(TypeSpecifier(Signed))) ||
    has_duplicate_specifiers(specifiers)
  ) {
    raise InvalidTypeSpecifier(specifiers)
  }

  let contains_illegal_storage_class = not(allow_storage_classes) && specifiers.iter().any(fn(specifier) {
    match specifier {
      StorageClassSpecifier(_) => true
      _ => false
    }
  })

  if contains_illegal_storage_class {
    raise InvalidTypeSpecifier(specifiers)
  }

  if specifiers.contains(TypeSpecifier(Unsigned)) && specifiers.contains(TypeSpecifier(Long)) {
    return UnsignedLong
  }

  if specifiers.contains(TypeSpecifier(Unsigned)) {
    return UnsignedInt
  }

  if specifiers.contains(TypeSpecifier(Long)) {
    return Long
  }

  Int
}

fn parse_type(self: Parser, allow_storage_classes~: Bool) -> Type raise ParserError {
  parse_type_specifiers(self.parse_specifiers(), allow_storage_classes~)
}

fn precedence_climbing(self: Parser, min_precedence: Int) -> Expr raise ParserError {
  let mut lhs = self.parse_factor()

  while true {
    match self.peek().to_binop() {
      Some(op) => {
        let precedence = op.precedence()

        if precedence >= min_precedence {
          self.advance() // consume the operator

          match op {
            Assignment(assignment_op) => { // right-associative
              let rhs = self.precedence_climbing(precedence)
              lhs = Assignment(op=assignment_op, lhs~, rhs~, ty=None)
            }
            Ternary => {
              let then = self.parse_expr()
              self.expect(Colon)
              let otherwise = self.precedence_climbing(precedence)
              lhs = Conditional(cond=lhs, then~, otherwise~, ty=None)
            }
            _ => { // left-associative
              let rhs = self.precedence_climbing(precedence + 1)
              lhs = Binary(op=op, lhs~, rhs~, ty=None)
            }
          }
        } else {
          break
        }
      }
      None => break
    }
  }

  lhs
}

fn parse_primary_expr(self: Parser) -> Expr raise ParserError {
  let token = self.peek()

  match token {
    IntLiteral(n) => {
      self.advance()
      Constant(val=IntConst(n), ty=None)
    }
    LongLiteral(n) => {
      self.advance()
      Constant(val=LongConst(n), ty=None)
    }
    Identifier(name) => {
      self.advance()

      if self.matches(Lparen) {
        let args = self.parse_argument_list()
        Call(name~, args~, ty=None)
      } else {
        Variable(name~, ty=None)
      }
    }
    UIntLiteral(n) => {
      self.advance()
      Constant(val=UnsignedIntConst(n), ty=None)
    }
    ULongLiteral(n) => {
      self.advance()
      Constant(val=UnsignedLongConst(n), ty=None)
    }
    Lparen => {
      self.advance()

      if self.peek().is_type_specifier() {
        self.parse_cast_expr()
      } else {
        let expr = self.parse_expr()
        self.expect(Rparen)
        expr
      }
    }
    _ => raise ExpectedExpr(token)
  }
}

fn parse_cast_expr(self: Parser) -> Expr raise ParserError {
  let specifiers = self.parse_specifiers()
  let target_ty = parse_type_specifiers(specifiers, allow_storage_classes=false)
  self.expect(Rparen)
  let expr = self.parse_factor()

  Cast(target_ty~, expr~, ty=None)
}

fn parse_argument_list(self: Parser) -> Array[Expr] raise ParserError {
  let args = []

  self.expect(Lparen)

  while not(self.matches(Rparen)) {
    args.push(self.parse_expr())

    if not(self.consumes(Comma)) {
      break
    } else if self.matches(Rparen) {
      raise TrailingComma
    }
  }

  self.expect(Rparen)

  args
}

fn parse_postfix_expr(self: Parser) -> Expr raise ParserError {
  let mut lhs = self.parse_primary_expr()

  while true {
    match self.peek() {
      PlusPlus => {
        self.advance()
        lhs = Unary(op=Inc(is_prefix=false), expr=lhs, ty=None)
      }
      MinusMinus => {
        self.advance()
        lhs = Unary(op=Dec(is_prefix=false), expr=lhs, ty=None)
      }
      _ => break
    }
  }

  lhs
}

fn parse_factor(self: Parser) -> Expr raise ParserError {
  let token = self.peek()

  match token {
    Minus => {
      self.advance()
      let expr = self.parse_factor()
      Unary(op=Neg, expr~, ty=None)
    }
    Tilde => {
      self.advance()
      let expr = self.parse_factor()
      Unary(op=Complement, expr~, ty=None)
    }
    Bang => {
      self.advance()
      let expr = self.parse_factor()
      Unary(op=Not, expr~, ty=None)
    }
    PlusPlus => {
      self.advance()
      let expr = self.parse_factor()
      Unary(op=Inc(is_prefix=true), expr~, ty=None)
    }
    MinusMinus => {
      self.advance()
      let expr = self.parse_factor()
      Unary(op=Dec(is_prefix=true), expr~, ty=None)
    }
    _ => self.parse_postfix_expr()
  }
}

fn parse_expr(self: Parser) -> Expr raise ParserError {
  self.precedence_climbing(0)
}

fn parse_stmt(self: Parser) -> Stmt raise ParserError {
  let token = self.peek()

  match token {
    Return => {
      self.advance()
      let expr = self.parse_expr()
      self.expect(Semicolon)
      Return(expr)
    }
    If => {
      self.advance()
      self.expect(Lparen)
      let cond = self.parse_expr()
      self.expect(Rparen)
      let then = self.parse_stmt();
      let otherwise = if self.consumes(Else) {
        Some(self.parse_stmt())
      } else {
        None
      }

      If(cond~, then~, otherwise~)
    }
    Semicolon => {
      self.advance()
      Null
    }
    Goto => {
      self.advance()
      let label = self.parse_identifier()
      self.expect(Semicolon)
      Goto(label)
    }
    Lbrace => self.parse_compound_stmt()
    Break => {
      self.advance()
      self.expect(Semicolon)
      Break(label=None)
    }
    Continue => {
      self.advance()
      self.expect(Semicolon)
      Continue(label=None)
    }
    While => self.parse_while_stmt()
    Do => self.parse_do_while_stmt()
    For => self.parse_for_stmt()
    Switch => self.parse_switch_stmt()
    Case => self.parse_case_stmt()
    Default => self.parse_default_stmt()
    Identifier(ident) => {
      if self.lookahead(1) == Colon {
        self.index += 2
        let stmt = self.parse_stmt()
        Labeled(label=ident, stmt~)
      } else {
        self.parse_expr_stmt()
      }
    }
    _ => self.parse_expr_stmt()
  }
}

fn parse_expr_stmt(self: Parser) -> Stmt raise ParserError {
  let expr = self.parse_expr()
  self.expect(Semicolon)
  Expression(expr)
}

fn parse_compound_stmt(self: Parser) -> Stmt raise ParserError {
  let items = []

  self.expect(Lbrace)

  while not(self.matches(Rbrace)) {
    items.push(self.parse_block_item())
  }

  self.expect(Rbrace)

  Compound(items)
}

fn parse_while_stmt(self: Parser) -> Stmt raise ParserError {
  self.expect(While)
  self.expect(Lparen)
  let cond = self.parse_expr()
  self.expect(Rparen)
  let body = self.parse_stmt()

  While(cond~, body~, label=None)
}

fn parse_do_while_stmt(self: Parser) -> Stmt raise ParserError {
  self.expect(Do)
  let body = self.parse_stmt()
  self.expect(While)
  self.expect(Lparen)
  let cond = self.parse_expr()
  self.expect(Rparen)
  self.expect(Semicolon)

  DoWhile(body~, cond~, label=None)
}

fn parse_for_stmt(self: Parser) -> Stmt raise ParserError {
  self.expect(For)
  self.expect(Lparen)

  let init = if self.matches(Semicolon) {
    self.advance()
    InitEmpty
  } else if self.matches_predicate(Token::is_specifier) {
    InitDecl(self.parse_variable_decl())
  } else {
    let expr = self.parse_expr()
    self.expect(Semicolon)
    InitExpr(expr)
  }

  let cond = if self.matches(Semicolon) {
    None
  } else {
    Some(self.parse_expr())
  }

  self.expect(Semicolon)

  let post = if self.matches(Rparen) {
    None
  } else {
    Some(self.parse_expr())
  }

  self.expect(Rparen)

  let body = self.parse_stmt()

  For(init~, cond~, post~, body~, label=None)
}

fn parse_case_stmt(self: Parser) -> Stmt raise ParserError {
  self.expect(Case)
  let expr = self.parse_expr()
  self.expect(Colon)
  let body = self.parse_stmt()

  Case(expr~, body~)
}

fn parse_default_stmt(self: Parser) -> Stmt raise ParserError {
  self.expect(Default)
  self.expect(Colon)
  let body = self.parse_stmt()

  Default(body~)
}

fn parse_switch_stmt(self: Parser) -> Stmt raise ParserError {
  self.expect(Switch)
  self.expect(Lparen)
  let expr = self.parse_expr()
  self.expect(Rparen)
  let body = self.parse_stmt()

  Switch(expr~, body~, label=None, cases_count=0)
}

fn parse_block_item(self: Parser) -> BlockItem raise ParserError {
  if self.matches_predicate(Token::is_specifier) {
    Decl(self.parse_decl())
  } else {
    Stmt(self.parse_stmt())
  }
}

priv enum TypeSpecifier {
  Int; Long; Unsigned; Signed
} derive(Eq, Hash, Show)

enum Specifier {
  TypeSpecifier(TypeSpecifier)
  StorageClassSpecifier(StorageClass)
} derive(Eq, Hash, Show)

fn parse_specifiers(self: Parser) -> Array[Specifier] {
  let specifiers = []

  while true {
    match self.peek() {
      Int => {
        self.advance()
        specifiers.push(TypeSpecifier(Int))
      }
      Long => {
        self.advance()
        specifiers.push(TypeSpecifier(Long))
      }
      Unsigned => {
        self.advance()
        specifiers.push(TypeSpecifier(Unsigned))
      }
      Signed => {
        self.advance()
        specifiers.push(TypeSpecifier(Signed))
      }
      Extern => {
        self.advance()
        specifiers.push(StorageClassSpecifier(Extern))
      }
      Static => {
        self.advance()
        specifiers.push(StorageClassSpecifier(Static))
      }
      _ => break
    }
  }

  specifiers
}

fn parse_type_and_storage_class(specifiers: Array[Specifier]) -> (Type, StorageClass?) raise ParserError {
  let type_specifiers = []
  let storage_classes = []

  for specifier in specifiers {
    match specifier {
      TypeSpecifier(_) => type_specifiers.push(specifier)
      StorageClassSpecifier(storage_class) => storage_classes.push(storage_class)
    }
  }

  if storage_classes.length() > 1 {
    raise InvalidStorageClassSpecifier
  }

  let ty = parse_type_specifiers(type_specifiers, allow_storage_classes=true)
  let storage_class = if storage_classes.length() == 1 {
    Some(storage_classes[0])
  } else {
    None
  }

  (ty, storage_class)
}

fn parse_decl_lhs(self: Parser) -> (StorageClass?, Type, String) raise ParserError {
  let specifiers = self.parse_specifiers()
  let (ty, storage_class) = parse_type_and_storage_class(specifiers)
  let name = self.parse_identifier()

  (storage_class, ty, name)
}

fn parse_variable_decl(self: Parser) -> VariableDecl raise ParserError {
  let (storage_class, ty, name) = self.parse_decl_lhs()
  self.parse_variable_decl_rhs(storage_class, ty, name)
}

fn parse_variable_decl_rhs(self: Parser, storage_class: StorageClass?, ty: Type, name: String) -> VariableDecl raise ParserError {
  let value = if self.consumes(Equal) {
    Some(self.parse_expr())
  } else {
    None
  }

  self.expect(Semicolon)

  { ty, name, value, storage_class }
}

fn parse_param_list(self: Parser) -> Array[FunctionParam] raise ParserError {
  if self.consumes(Void) {
    []
  } else {
    let params: Array[FunctionParam] = []
    
    while true {
      let ty = self.parse_type(allow_storage_classes=false)
      let name = self.parse_identifier()
      params.push({ ty, name })

      if not(self.consumes(Comma)) {
        break
      }
    }

    if params.is_empty() {
      raise EmptyParamListNotSupportedUseVoidInstead
    }

    params
  }
}

fn parse_function_decl_rhs(self: Parser, storage_class: StorageClass?, return_ty: Type, name: String) -> FunctionDecl raise ParserError {
  self.expect(Lparen)
  let params = self.parse_param_list()
  self.expect(Rparen)

  let body = if self.consumes(Semicolon) {
    None
  } else {
    Some(self.parse_block())
  }

  { return_ty, name, params, body, storage_class }
}

fn parse_decl(self: Parser) -> Decl raise ParserError {
  let (storage_class, ty, name) = self.parse_decl_lhs()

  if self.matches(Lparen) {
    FunDecl(self.parse_function_decl_rhs(storage_class, ty, name))
  } else {
    VarDecl(self.parse_variable_decl_rhs(storage_class, ty, name))
  }
}

fn parse_block(self: Parser) -> Array[BlockItem] raise ParserError {
  let items = []

  self.expect(Lbrace)

  while not(self.matches(Rbrace)) {
    items.push(self.parse_block_item())
  }

  self.expect(Rbrace)

  items
}

pub fn parse(self: Parser) -> Program raise ParserError {
  let decls = []

  while self.index < self.tokens.length() {
    let decl = self.parse_decl()
    decls.push(decl)
  }

  { decls, }
}
