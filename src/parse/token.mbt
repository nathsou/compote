
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
  Int; Long; Signed; Unsigned; Double
  Return; Void; If; Else; Goto
  Do; While; For; Break; Continue
  Switch; Case; Default
  Static; Extern
  // literals
  IntLiteral(Int); LongLiteral(Int64)
  UIntLiteral(UInt); ULongLiteral(UInt64)
  DoubleLiteral(Double)
  // identifiers
  Identifier(String)
} derive(Eq)

pub fn to_binop(self: Token) -> BinaryOp? {
  match self {
    Plus => Some(Add)
    Minus => Some(Sub)
    Star => Some(Mul)
    Slash => Some(Div)
    Percent => Some(Rem)
    Ampersand => Some(BitwiseAnd)
    Pipe => Some(BitwiseOr)
    Caret => Some(BitwiseXor)
    Lshift => Some(ShiftLeft)
    Rshift => Some(ShiftRight)
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
    LshiftEqual => Some(Assignment(AssignShiftLeft))
    RshiftEqual => Some(Assignment(AssignShiftRight))
    QuestionMark => Some(Ternary)
    _ => None
  }
}

pub fn is_type_specifier(self: Token) -> Bool {
  match self {
    Int | Long | Unsigned | Signed | Double => true
    _ => false
  }
}

pub fn is_specifier(self: Token) -> Bool {
  match self {
    Extern | Static => true
    _ => self.is_type_specifier()
  }
}

pub impl Show for Token with output(self, logger: &Logger) {
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
    Long => "long"
    Signed => "signed"
    Unsigned => "unsigned"
    Double => "double"
    Return => "return"
    Void => "void"
    If => "if"
    Else => "else"
    Goto => "goto"
    Do => "do"
    While => "while"
    For => "for"
    Break => "break"
    Continue => "continue"
    Switch => "switch"
    Case => "case"
    Default => "default"
    Static => "static"
    Extern => "extern"
    IntLiteral(n) => n.to_string()
    LongLiteral(n) => n.to_string()
    UIntLiteral(n) => n.to_string()
    ULongLiteral(n) => n.to_string()
    DoubleLiteral(n) => n.to_string()
    Identifier(s) => s
  })
}
