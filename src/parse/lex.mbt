
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

enum ParsedInt {
  Int(Int)
  Long(Int64)
}

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
      suffix += char.to_string()
      self.advance()
    } else {
      break
    }
  }

  let is_long = match suffix {
    "" => false
    "l" | "L" => true
    _ => raise InvalidIntegerLiteral(digits + suffix)
  }

  match @strconv.parse_int64?(digits) {
    Err(_) => raise InvalidIntegerLiteral(digits)
    Ok(value) => match is_long {
      true => Long(value)
      false => {
        // if the value is out of range for an int, return a long
        if value > @int.max_value.to_int64() || value < @int.min_value.to_int64() {
          Long(value)
        } else {
          Int(value.to_int())
        }
      }
    }
  }
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
          }
        }
      } else if is_alpha(char) || char == '_' {
        let identifier = self.parse_identifier()

        Ok(match identifier {
          "int" => Int
          "long" => Long
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
