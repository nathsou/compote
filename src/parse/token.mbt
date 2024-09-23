
pub enum Token {
  // symbols
  Lparen; Rparen; Lbrace; Rbrace; Comma; Semicolon
  QuestionMark; Colon
  Plus; Minus; Star; Slash; Percent; Tilde; Bang
  AmpersandAmpersand; PipePipe; EqualEqual; BangEqual
  Less; Greater; LessEqual; GreaterEqual
  Ampersand; Pipe; Caret; Lshift; Rshift
  Equal; PlusEqual; MinusEqual; StarEqual; SlashEqual; PercentEqual
  AmpersandEqual; PipeEqual; CaretEqual; LshiftEqual; RshiftEqual
  PlusPlus; MinusMinus
  // keywords
  Int; Return; Void; If; Else
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
    Equal => Some(Assignment(Assign))
    PlusEqual => Some(Assignment(AssignAdd))
    MinusEqual => Some(Assignment(AssignSub))
    StarEqual => Some(Assignment(AssignMul))
    SlashEqual => Some(Assignment(AssignDiv))
    PercentEqual => Some(Assignment(AssignRem))
    AmpersandEqual => Some(Assignment(AssignAnd))
    PipeEqual => Some(Assignment(AssignOr))
    CaretEqual => Some(Assignment(AssignXor))
    LshiftEqual => Some(Assignment(AssignShl))
    RshiftEqual => Some(Assignment(AssignShr))
    QuestionMark => Some(Ternary)
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
    QuestionMark => "?"
    Colon => ":"
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
    PlusEqual => "+="
    MinusEqual => "-="
    StarEqual => "*="
    SlashEqual => "/="
    PercentEqual => "%="
    AmpersandEqual => "&="
    PipeEqual => "|="
    CaretEqual => "^="
    LshiftEqual => "<<="
    RshiftEqual => ">>="
    PlusPlus => "++"
    MinusMinus => "--"
    Int => "int"
    Return => "return"
    Void => "void"
    If => "if"
    Else => "else"
    IntLiteral(n) => n.to_string()
    Identifier(s) => s
  })
}
