pub struct Parser {
  tokens: Array[Token]
  mut index: Int
}

pub type! ParserError {
  UnexpectedToken(Token)
  ExpectedIdentifier(Token)
  ExpectedType(Token)
  ExpectedExpr(Token)
  ExpectedStmt(Token)
} derive(Show)

pub fn Parser::new(tokens: Array[Token]) -> Parser {
  { tokens, index: 0}
}

fn peek(self: Parser) -> Token {
  self.tokens[self.index]
}

fn advance(self: Parser) -> Unit {
  self.index += 1
}

fn expect(self: Parser, expected: Token) -> Unit!ParserError {
  let token = self.peek()

  if token == expected {
    self.advance()
  } else {
    raise UnexpectedToken(token)
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
          let rhs = self.precedence_climbing!(precedence + 1)
          lhs = Binary(~op=op, ~lhs, ~rhs)
        } else {
          break
        }
      }
      None => break
    }
  }

  lhs
}

fn parse_factor(self: Parser) -> Expr!ParserError {
  let token = self.peek()

  match token {
    IntLiteral(n) => {
      self.advance()
      Constant(n)
    }
    Minus => {
      self.advance()
      let expr = self.parse_expr!()
      Unary(~op=Neg, ~expr)
    }
    Tilde => {
      self.advance()
      let expr = self.parse_expr!()
      Unary(~op=Not, ~expr)
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
    _ => raise ExpectedStmt(token)
  }
}

fn parse_function_decl(self: Parser) -> Decl!ParserError {
  let returnType = self.parse_type!()
  let name = self.parse_identifier!()
  self.expect!(Lparen)
  self.consumeIfPresent(Void)
  self.expect!(Rparen)
  self.expect!(Lbrace)
  let body = self.parse_stmt!()
  self.expect!(Rbrace)

  Function(~returnType, ~name, ~body)
}

fn parse_decl(self: Parser) -> Decl!ParserError {
  self.parse_function_decl!()
}

pub fn parse(self: Parser) -> Program!ParserError {
  let decls = []

  while self.index < self.tokens.length() {
    let decl = self.parse_decl!()
    decls.push(decl)
  }

  Program::new(decls)
}
