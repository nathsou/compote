
pub struct Lexer {
  input: String
  mut index: Int
}

pub type! LexerError {
  UnexepectedChar(Char)
  InvalidIntegerLiteral(String)
} derive(Show)

fn is_whitespace(c: Char) -> Bool {
  match c {
    ' ' => true
    '\t' => true
    '\n' => true
    '\r' => true
    _ => false
  }
}

fn is_digit(c: Char) -> Bool {
  c >= '0' && c <= '9'
}

fn is_alpha(c: Char) -> Bool {
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_alphanumeric(c: Char) -> Bool {
  is_alpha(c) || is_digit(c)
}

pub fn Lexer::new(input: String) -> Lexer {
  { input, index: 0 }
}

fn peek(self: Lexer) -> Char {
  self.input[self.index]
}

fn lookahead(self: Lexer, n: Int) -> Char {
  self.input[self.index + n]
}

fn matches(self: Lexer, char: Char) -> Bool {
  self.peek() == char
}

fn matches_str(self: Lexer, chars: String) -> Bool {
  for i, char in chars {
    if self.lookahead(i) != char {
      return false
    }
  }

  true
}

fn consumes(self: Lexer, char: Char) -> Bool {
  if self.matches(char) {
    self.index += 1
    true
  } else {
    false
  }
}

fn skip_whitespaces(self: Lexer) -> Unit {
  while self.index < self.input.length() && is_whitespace(self.peek()) {
    self.index += 1
  }
}

priv enum ParsedInt {
  Int(Int)
  Long(Int64)
  UnsignedInt(UInt)
  UnsignedLong(UInt64)
}

let zero_ascii: Int = '0'.to_int()

fn parse_digit(c: Char) -> Int {
  c.to_int() - zero_ascii
}

fn parse_i64(s: String) -> Int64 {
  // assumes the string only contains (base-10) digits
  let mut n: Int64 = 0
  let p: Int64 = 10

  for c in s {
    n = n * p + parse_digit(c).to_int64()
  }

  n
}

priv enum IntKind {
  IntK; LongK; UIntK; ULongK
}

fn reinterpret_i64(n: Int64, kind: IntKind) -> ParsedInt {
  match kind {
    IntK => Int(n.to_int())
    LongK => Long(n)
    UIntK => UnsignedInt(n.to_int().reinterpret_as_uint())
    ULongK => UnsignedLong(n.to_uint64())
  }
}

let max_int: Int64 = @int.max_value.to_int64()
let min_int: Int64 = @int.min_value.to_int64()

fn parse_integer(self: Lexer) -> ParsedInt!LexerError {
  let mut digits = ""

  for i in self.index..<self.input.length() {
    let char = self.input[i]

    if is_digit(char) {
      digits += char.to_string()
      self.advance()
    } else {
      break
    }
  }

  let mut suffix = ""

  while self.index < self.input.length() {
    let char = self.peek()

    if is_alphanumeric(char) {
      suffix += char.to_string().to_lower()
      self.advance()
    } else {
      break
    }
  }

  let value = parse_i64(digits)

  let kind = match suffix {
    "l" => LongK
    "ul" | "lu" => ULongK
    "u" => {
      // if the value is out of range for an int, return a long
      if value > max_int || value < min_int {
        ULongK
      } else {
        UIntK
      }
    }
    "" => {
      // if the value is out of range for an int, return a long
      if value > max_int || value < min_int {
        LongK
      } else {
        IntK
      }
    }
    _ => raise InvalidIntegerLiteral(digits + suffix)
  }

  reinterpret_i64(value, kind)
}

fn parse_identifier(self: Lexer) -> String {
  let mut identifier = ""
  
  for i in self.index..<self.input.length() {
    let char = self.input[i]

    if is_alphanumeric(char) || char == '_' {
      identifier += char.to_string()
      self.advance()
    } else {
      break
    }
  }

  identifier
}

fn advance(self: Lexer) -> Unit {
  self.index += 1
}

enum LexerResult {
  Ok(Token)
  Err(LexerError)
  EOF
}

fn next(self: Lexer) -> LexerResult {
  self.skip_whitespaces()

  if self.index >= self.input.length() {
    return EOF
  }

  let char = self.peek()

  match char {
    '(' => {
      self.advance()
      Ok(Lparen)
    }
    ')' => {
      self.advance()
      Ok(Rparen)
    }
    '{' => {
      self.advance()
      Ok(Lbrace)
    }
    '}' => {
      self.advance()
      Ok(Rbrace)
    }
    ',' => {
      self.advance()
      Ok(Comma)
    }
    ';' => {
      self.advance()
      Ok(Semicolon)
    }
    '?' => {
      self.advance()
      Ok(QuestionMark)
    }
    ':' => {
      self.advance()
      Ok(Colon)
    }
    '+' => {
      self.advance()
      if self.consumes('=') {
        Ok(PlusEqual)
      } else if self.consumes('+') {
        Ok(PlusPlus)
      } else {
        Ok(Plus)
      }
    }
    '-' => {
      self.advance()
      if self.consumes('=') {
        Ok(MinusEqual)
      } else if self.consumes('-') {
        Ok(MinusMinus)
      } else {
        Ok(Minus)
      }
    }
    '*' => {
      self.advance()
      if self.consumes('=') {
        Ok(StarEqual)
      } else {
        Ok(Star)
      }
    }
    '/' => {
      self.advance()
      // skip comments
      if self.consumes('/') {
        while self.peek() != '\n' {
          self.advance()
        }
        
        return self.next()
      } else if self.consumes('*') {
        while not(self.matches_str("*/")) {
          self.advance()
        }

        self.index += 2 // skip "*/"

        return self.next()
      } else if self.consumes('=') {
        return Ok(SlashEqual)
      }

      Ok(Slash)
    }
    '%' => {
      self.advance()
      if self.consumes('=') {
        Ok(PercentEqual)
      } else {
        Ok(Percent)
      }
    }
    '&' => {
      self.advance()
      if self.consumes('&') {
        Ok(AmpersandAmpersand)
      } else if self.consumes('=') {
        Ok(AmpersandEqual)
      } else {
        Ok(Ampersand)
      }
    }
    '|' => {
      self.advance()
      if self.consumes('|') {
        Ok(PipePipe)
      } else if self.consumes('=') {
        Ok(PipeEqual)
      } else {
        Ok(Pipe)
      }
    }
    '^' => {
      self.advance()
      if self.consumes('=') {
        Ok(CaretEqual)
      } else {
        Ok(Caret)
      }
    }
    '<' => {
      self.advance()
      if self.consumes('<') {
        if self.consumes('=') {
          Ok(LshiftEqual)
        } else {
          Ok(Lshift)
        }
      } else if self.consumes('=') {
        Ok(LessEqual)
      } else {
        Ok(Less)
      }
    }
    '>' => {
      self.advance()
      if self.consumes('>') {
        if self.consumes('=') {
          Ok(RshiftEqual)
        } else {
          Ok(Rshift)
        }
      } else if self.consumes('=') {
        Ok(GreaterEqual)
      } else {
        Ok(Greater)
      }
    }
    '~' => {
      self.advance()
      Ok(Tilde)
    }
    '!' => {
      self.advance()
      if self.consumes('=') {
        Ok(BangEqual)
      } else {
        Ok(Bang)
      }
    }
    '=' => {
      self.advance()
      if self.consumes('=') {
        Ok(EqualEqual)
      } else {
        Ok(Equal)
      }
    }
    _ => {
      if is_digit(char) {
        match self.parse_integer?() {
          Err(error) => return Err(error)
          Ok(value) => match value {
            Int(n) => Ok(IntLiteral(n))
            Long(n) => Ok(LongLiteral(n))
            UnsignedInt(n) => Ok(UIntLiteral(n))
            UnsignedLong(n) => Ok(ULongLiteral(n))
          }
        }
      } else if is_alpha(char) || char == '_' {
        let identifier = self.parse_identifier()

        Ok(match identifier {
          "int" => Int
          "long" => Long
          "signed" => Signed
          "unsigned" => Unsigned
          "return" => Return
          "void" => Void
          "if" => If
          "else" => Else
          "goto" => Goto
          "do" => Do
          "while" => While
          "for" => For
          "break" => Break
          "continue" => Continue
          "switch" => Switch
          "case" => Case
          "default" => Default
          "static" => Static
          "extern" => Extern
          _ => Identifier(identifier)
        })
      } else {
        Err(UnexepectedChar(char))
      }
    }
  }
}

pub fn lex(self: Lexer) -> Array[Token]!LexerError {
  let tokens = []

  while true {
    match self.next() {
      Ok(token) => tokens.push(token)
      Err(error) => raise error
      EOF => break
    }
  }

  tokens
}
