pub struct Parser {
  tokens: Array[Token]
  mut index: Int
}

pub type! ParserError {
  UnexpectedToken(~expected: Token, ~received: Token)
  ExpectedIdentifier(Token)
  ExpectedType(Token)
  ExpectedExpr(Token)
  EmptyParamListNotSupportedUseVoidInstead
  TrailingComma
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

fn expect(self: Parser, expected: Token) -> Unit!ParserError {
  let token = self.peek()

  if token == expected {
    self.advance()
  } else {
    raise UnexpectedToken(~expected, ~received=token)
  }
}

fn matches(self: Parser, token: Token) -> Bool {
  self.peek() == token
}

fn consumes(self: Parser, token: Token) -> Bool {
  if self.matches(token) {
    self.advance()
    true
  } else {
    false
  }
}

fn consumeIfPresent(self: Parser, token: Token) -> Unit {
    let _ = self.consumes(token)
}

fn parse_identifier(self: Parser) -> String!ParserError {
  let token = self.peek()

  match token {
    Identifier(name) => {
      self.advance()
      name
    }
    _ => raise ExpectedIdentifier(token)
  }
}

fn parse_type(self: Parser) -> Type!ParserError {
  let token = self.peek()

  match token {
    Int => {
      self.advance()
      Int
    }
    Void => {
      self.advance()
      Void
    }
    _ => raise ExpectedType(token)
  }
}

fn precedence_climbing(self: Parser, min_precedence: Int) -> Expr!ParserError {
  let mut lhs = self.parse_factor!()

  while true {
    match self.peek().to_binop() {
      Some(op) => {
        let precedence = op.precedence()

        if precedence >= min_precedence {
          self.advance() // consume the operator

          match op {
            Assignment(assignment_op) => { // right-associative
              let rhs = self.precedence_climbing!(precedence)
              lhs = Assignment(~op=assignment_op, ~lhs, ~rhs)
            }
            Ternary => {
              let then = self.parse_expr!()
              self.expect!(Colon)
              let otherwise = self.precedence_climbing!(precedence)
              lhs = Conditional(~cond=lhs, ~then, ~otherwise)
            }
            _ => { // left-associative
              let rhs = self.precedence_climbing!(precedence + 1)
              lhs = Binary(~op=op, ~lhs, ~rhs)
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

fn parse_primary_expr(self: Parser) -> Expr!ParserError {
  let token = self.peek()

  match token {
    IntLiteral(n) => {
      self.advance()
      Constant(IntConst(n))
    }
    Identifier(name) => {
      self.advance()

      if self.matches(Lparen) {
        let args = self.parse_argument_list!()
        Call(~name, ~args)
      } else {
        Variable(name)
      }
    }
    Lparen => {
      self.advance()
      let expr = self.parse_expr!()
      self.expect!(Rparen)
      expr
    }
    _ => raise ExpectedExpr(token)
  }
}

fn parse_argument_list(self: Parser) -> Array[Expr]!ParserError {
  let args = []

  self.expect!(Lparen)

  while not(self.matches(Rparen)) {
    args.push(self.parse_expr!())

    if not(self.consumes(Comma)) {
      break
    } else if self.matches(Rparen) {
      raise TrailingComma
    }
  }

  self.expect!(Rparen)

  args
}

fn parse_postfix_expr(self: Parser) -> Expr!ParserError {
  let mut lhs = self.parse_primary_expr!()

  while true {
    match self.peek() {
      PlusPlus => {
        self.advance()
        lhs = Unary(~op=Inc(~is_prefix=false), ~expr=lhs)
      }
      MinusMinus => {
        self.advance()
        lhs = Unary(~op=Dec(~is_prefix=false), ~expr=lhs)
      }
      _ => break
    }
  }

  lhs
}

fn parse_factor(self: Parser) -> Expr!ParserError {
  let token = self.peek()

  match token {
    Minus => {
      self.advance()
      let expr = self.parse_factor!()
      Unary(~op=Neg, ~expr)
    }
    Tilde => {
      self.advance()
      let expr = self.parse_factor!()
      Unary(~op=Complement, ~expr)
    }
    Bang => {
      self.advance()
      let expr = self.parse_factor!()
      Unary(~op=Not, ~expr)
    }
    PlusPlus => {
      self.advance()
      let expr = self.parse_factor!()
      Unary(~op=Inc(~is_prefix=true), ~expr)
    }
    MinusMinus => {
      self.advance()
      let expr = self.parse_factor!()
      Unary(~op=Dec(~is_prefix=true), ~expr)
    }
    _ => self.parse_postfix_expr!()
  }
}

fn parse_expr(self: Parser) -> Expr!ParserError {
  self.precedence_climbing!(0)
}

fn parse_stmt(self: Parser) -> Stmt!ParserError {
  let token = self.peek()

  match token {
    Return => {
      self.advance()
      let expr = self.parse_expr!()
      self.expect!(Semicolon)
      Return(expr)
    }
    If => {
      self.advance()
      self.expect!(Lparen)
      let cond = self.parse_expr!()
      self.expect!(Rparen)
      let then = self.parse_stmt!();
      let otherwise = if self.consumes(Else) {
        Some(self.parse_stmt!())
      } else {
        None
      }

      If(~cond, ~then, ~otherwise)
    }
    Semicolon => {
      self.advance()
      Null
    }
    Goto => {
      self.advance()
      let label = self.parse_identifier!()
      self.expect!(Semicolon)
      Goto(label)
    }
    Lbrace => self.parse_compound_stmt!()
    Break => {
      self.advance()
      self.expect!(Semicolon)
      Break(~label=None)
    }
    Continue => {
      self.advance()
      self.expect!(Semicolon)
      Continue(~label=None)
    }
    While => self.parse_while_stmt!()
    Do => self.parse_do_while_stmt!()
    For => self.parse_for_stmt!()
    Switch => self.parse_switch_stmt!()
    Case => self.parse_case_stmt!()
    Default => self.parse_default_stmt!()
    Identifier(ident) => {
      if self.lookahead(1) == Colon {
        self.index += 2
        let stmt = self.parse_stmt!()
        Labeled(~label=ident, ~stmt)
      } else {
        self.parse_expr_stmt!()
      }
    }
    _ => self.parse_expr_stmt!()
  }
}

fn parse_expr_stmt(self: Parser) -> Stmt!ParserError {
  let expr = self.parse_expr!()
  self.expect!(Semicolon)
  Expression(expr)
}

fn parse_compound_stmt(self: Parser) -> Stmt!ParserError {
  let items = []

  self.expect!(Lbrace)

  while not(self.matches(Rbrace)) {
    items.push(self.parse_block_item!())
  }

  self.expect!(Rbrace)

  Compound(items)
}

fn parse_while_stmt(self: Parser) -> Stmt!ParserError {
  self.expect!(While)
  self.expect!(Lparen)
  let cond = self.parse_expr!()
  self.expect!(Rparen)
  let body = self.parse_stmt!()

  While(~cond, ~body, ~label=None)
}

fn parse_do_while_stmt(self: Parser) -> Stmt!ParserError {
  self.expect!(Do)
  let body = self.parse_stmt!()
  self.expect!(While)
  self.expect!(Lparen)
  let cond = self.parse_expr!()
  self.expect!(Rparen)
  self.expect!(Semicolon)

  DoWhile(~body, ~cond, ~label=None)
}

fn parse_for_stmt(self: Parser) -> Stmt!ParserError {
  self.expect!(For)
  self.expect!(Lparen)

  let init = if self.matches(Semicolon) {
    self.advance()
    InitEmpty
  } else if self.matches(Int) {
    InitDecl(VarDecl(self.parse_variable_decl!()))
  } else {
    let expr = self.parse_expr!()
    self.expect!(Semicolon)
    InitExpr(expr)
  }

  let cond = if self.matches(Semicolon) {
    None
  } else {
    Some(self.parse_expr!())
  }

  self.expect!(Semicolon)

  let post = if self.matches(Rparen) {
    None
  } else {
    Some(self.parse_expr!())
  }

  self.expect!(Rparen)

  let body = self.parse_stmt!()

  For(~init, ~cond, ~post, ~body, ~label=None)
}

fn parse_case_stmt(self: Parser) -> Stmt!ParserError {
  self.expect!(Case)
  let expr = self.parse_expr!()
  self.expect!(Colon)
  let body = self.parse_stmt!()

  Case(~expr, ~body)
}

fn parse_default_stmt(self: Parser) -> Stmt!ParserError {
  self.expect!(Default)
  self.expect!(Colon)
  let body = self.parse_stmt!()

  Default(~body)
}

fn parse_switch_stmt(self: Parser) -> Stmt!ParserError {
  self.expect!(Switch)
  self.expect!(Lparen)
  let expr = self.parse_expr!()
  self.expect!(Rparen)
  let body = self.parse_stmt!()

  Switch(~expr, ~body, ~label=None, ~cases_count=0)
}

fn parse_block_item(self: Parser) -> BlockItem!ParserError {
  match self.peek() {
    Int => Decl(self.parse_decl!())
    _ => Stmt(self.parse_stmt!())
  }
}

fn parse_variable_decl(self: Parser) -> VariableDecl!ParserError {
  let ty = self.parse_type!()
  let name = self.parse_identifier!()
  self.parse_variable_decl_rhs!(ty, name)
}

fn parse_variable_decl_rhs(self: Parser, ty: Type, name: String) -> VariableDecl!ParserError {
  let value = if self.consumes(Equal) {
    Some(self.parse_expr!())
  } else {
    None
  }

  self.expect!(Semicolon)

  { ty, name, value }
}

fn parse_param_list(self: Parser) -> Array[FunctionParam]!ParserError {
  if self.consumes(Void) {
    []
  } else {
    let params: Array[FunctionParam] = []
    
    while true {
      let ty = self.parse_type!()
      let name = self.parse_identifier!()
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

fn parse_function_decl(self: Parser) -> FunctionDecl!ParserError {
  let return_ty = self.parse_type!()
  let name = self.parse_identifier!()
  self.parse_function_decl_rhs!(return_ty, name)
}

fn parse_function_decl_rhs(self: Parser, return_type: Type, name: String) -> FunctionDecl!ParserError {
  self.expect!(Lparen)
  let params = self.parse_param_list!()
  self.expect!(Rparen)

  let body = if self.consumes(Semicolon) {
    None
  } else {
    Some(self.parse_block!())
  }

  { return_type, name, params, body }
}

fn parse_decl(self: Parser) -> Decl!ParserError {
  let ty = self.parse_type!()
  let name = self.parse_identifier!()

  if self.matches(Lparen) {
    FunDecl(self.parse_function_decl_rhs!(ty, name))
  } else {
    VarDecl(self.parse_variable_decl_rhs!(ty, name))
  }
}

fn parse_block(self: Parser) -> Array[BlockItem]!ParserError {
  let items = []

  self.expect!(Lbrace)

  while not(self.matches(Rbrace)) {
    items.push(self.parse_block_item!())
  }

  self.expect!(Rbrace)

  items
}

pub fn parse(self: Parser) -> Program!ParserError {
  let functions = []

  while self.index < self.tokens.length() {
    let decl = self.parse_function_decl!()
    functions.push(decl)
  }

  { functions, }
}
