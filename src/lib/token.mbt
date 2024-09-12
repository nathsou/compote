
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
