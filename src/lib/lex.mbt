
pub struct Lexer {
  input: String
  mut index: Int
}

pub type! LexerError {
  UnexepectedChar(Char)
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

fn matches(self: Lexer, char: Char) -> Bool {
  self.peek() == char
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

fn parse_integer(self: Lexer) -> Int {
  let mut value = 0
  
  for i in self.index..<self.input.length() {
    let char = self.input[i]
    if is_digit(char) {
      value = value * 10 + (char.to_int() - 48)
      self.advance()
    } else {
      break
    }
  }

  value
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
    '+' => {
      self.advance()
      Ok(Plus)
    }
    '-' => {
      self.advance()
      Ok(Minus)
    }
    '*' => {
      self.advance()
      Ok(Star)
    }
    '/' => {
      self.advance()
      // skip comments
      if self.consumes('/') {
        while self.peek() != '\n' {
          self.advance()
        }
        
        return self.next()
      }

      Ok(Slash)
    }
    '%' => {
      self.advance()
      Ok(Percent)
    }
    '~' => {
      self.advance()
      Ok(Tilde)
    }
    _ => {
      if is_digit(char) {
        let value = self.parse_integer()
        Ok(IntLiteral(value))
      } else if is_alpha(char) || char == '_' {
        let identifier = self.parse_identifier()

        match identifier {
          "int" => Ok(Int)
          "return" => Ok(Return)
          "void" => Ok(Void)
          _ => Ok(Identifier(identifier))
        }
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
