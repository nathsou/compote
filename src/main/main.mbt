
type! CompilerError {
  Lexer(@lib.LexerError)
  Parser(@lib.ParserError)
  Emitter(@lib.EmitterError)
} derive(Show)

fn read_source_file_char(index: Int) -> Char = "host" "read_source_file_char"
fn get_source_file_length() -> Int = "host" "get_source_file_length"

fn read_source_file() -> String {
  let len = get_source_file_length()
  let chars = []

  for i in 0..<len {
    chars.push(read_source_file_char(i).to_string())
  }

  chars.join("")
}

fn lex(source: String) -> Array[@lib.Token]!CompilerError {
  let lexer = @lib.Lexer::new(source)

  match lexer.lex?() {
    Ok(tokens) => tokens
    Err(error) => raise Lexer(error)
  }
}

fn parse(source: String) -> @lib.Program!CompilerError {
  let tokens = lex!(source)
  let parser = @lib.Parser::new(tokens)

  match parser.parse?() {
    Ok(program) => program
    Err(error) => raise Parser(error)
  }
}

fn tac_gen(source: String) -> @lib.TacProgram!CompilerError {
  let program = parse!(source)
  @lib.convert_prog(program)
}

fn codegen(source: String) -> @lib.AsmProgram!CompilerError {
  let program = tac_gen!(source)
  program.asm()
}

fn assemble(source: String, ~os: @lib.OperatingSystem) -> String!CompilerError {
  let program = codegen!(source)
  let emitter = @lib.Emitter::new(os)
  match emitter.emit_program?(program) {
    Ok(_) => Show::to_string(emitter)
    Err(error) => raise Emitter(error)
  }
}

fn main {
  let source = read_source_file()
  
  match assemble?(source, ~os=Linux) {
    Ok(asm) => {
      println(asm)
    }
    Err(error) => {
      println("Error: \{error}")
    }
  }
}
