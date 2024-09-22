pub struct Parser {
  tokens: Array[Token]
  mut index: Int
}

pub type! ParserError {
  UnexpectedToken(Token)
  ExpectedIdentifier(Token)
  ExpectedType(Token)
  ExpectedExpr(Token)
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

          match op {
            Assign => { // right-associative
              let rhs = self.precedence_climbing!(precedence)
              lhs = Assignment(~lhs, ~rhs)
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

fn parse_factor(self: Parser) -> Expr!ParserError {
  let token = self.peek()

  match token {
    IntLiteral(n) => {
      self.advance()
      Constant(n)
    }
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
    Lparen => {
      self.advance()
      let expr = self.parse_expr!()
      self.expect!(Rparen)
      expr
    }
    Identifier(name) => {
      self.advance()
      Variable(name)
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
    Semicolon => {
      self.advance()
      Null
    }
    _ => {
      let expr = self.parse_expr!()
      self.expect!(Semicolon)
      Expression(expr)
    }
  }
}

fn parse_block_item(self: Parser) -> BlockItem!ParserError {
  match self.peek() {
    Int => Decl(self.parse_decl!())
    _ => Stmt(self.parse_stmt!())
  }
}

fn parse_variable_decl(self: Parser) -> Decl!ParserError {
  let ty = self.parse_type!()
  let name = self.parse_identifier!()
  let value = if self.consumes(Equal) {
    Some(self.parse_expr!())
  } else {
    None
  }
  self.expect!(Semicolon)

  VariableDecl(~ty, ~name, ~value)
}

fn parse_decl(self: Parser) -> Decl!ParserError {
  self.parse_variable_decl!()
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

fn parse_function_def(self: Parser) -> FunctionDef!ParserError {
  let return_type = self.parse_type!()
  let name = self.parse_identifier!()
  self.expect!(Lparen)
  self.consumeIfPresent(Void)
  self.expect!(Rparen)
  let body = self.parse_block!()

  { return_type, name, body }
}

pub fn parse(self: Parser) -> Program!ParserError {
  let functions = []

  while self.index < self.tokens.length() {
    let decl = self.parse_function_def!()
    functions.push(decl)
  }

  { functions, }
}
