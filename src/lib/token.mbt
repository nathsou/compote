
pub enum Token {
  // symbols
  Lparen; Rparen; Lbrace; Rbrace; Comma; Semicolon; Plus; Minus; Star; Slash; Percent; Tilde
  // keywords
  Int; Return; Void
  // literals
  IntLiteral(Int)
  // identifiers
  Identifier(String)
} derive(Eq)

pub fn to_binop(self: Token) -> Option[BinaryOp] {
  match self {
    Plus => Some(Add)
    Minus => Some(Sub)
    Star => Some(Mul)
    Slash => Some(Div)
    Percent => Some(Rem)
    _ => None
  }
}

pub impl Show for Token with output(self, logger: Logger) {
  logger.write_string(match self {
    Lparen => "("
    Rparen => ")"
    Lbrace => "{"
    Rbrace => "}"
    Comma => ","
    Semicolon => ";"
    Plus => "+"
    Minus => "-"
    Star => "*"
    Slash => "/"
    Percent => "%"
    Tilde => "~"
    Int => "int"
    Return => "return"
    Void => "void"
    IntLiteral(n) => n.to_string()
    Identifier(s) => s
  })
}
