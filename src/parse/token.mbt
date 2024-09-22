
pub enum Token {
  // symbols
  Lparen; Rparen; Lbrace; Rbrace; Comma; Semicolon
  Plus; Minus; Star; Slash; Percent; Tilde; Bang
  AmpersandAmpersand; PipePipe; EqualEqual; BangEqual
  Less; Greater; LessEqual; GreaterEqual
  Ampersand; Pipe; Caret; Lshift; Rshift
  Equal
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
    Ampersand => Some(BitwiseAnd)
    Pipe => Some(BitwiseOr)
    Caret => Some(BitwiseXor)
    Lshift => Some(Shl)
    Rshift => Some(Shr)
    EqualEqual => Some(Equ)
    BangEqual => Some(Neq)
    Less => Some(Lss)
    Greater => Some(Gtr)
    LessEqual => Some(Leq)
    GreaterEqual => Some(Geq)
    AmpersandAmpersand => Some(LogicalAnd)
    PipePipe => Some(LogicalOr)
    Equal => Some(Assign)
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
    Ampersand => "&"
    Pipe => "|"
    Caret => "^"
    Lshift => "<<"
    Rshift => ">>"
    Tilde => "~"
    Bang => "!"
    AmpersandAmpersand => "&&"
    PipePipe => "||"
    EqualEqual => "=="
    BangEqual => "!="
    Less => "<"
    Greater => ">"
    LessEqual => "<="
    GreaterEqual => ">="
    Equal => "="
    Int => "int"
    Return => "return"
    Void => "void"
    IntLiteral(n) => n.to_string()
    Identifier(s) => s
  })
}
