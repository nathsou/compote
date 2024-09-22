
type! CompilerError {
  Lexer(@parse.LexerError)
  Parser(@parse.ParserError)
  Validator(@validate.ValidationError)
  TacBuilder(@codegen.TacBuilderError)
  AsmBuilder(@codegen.AsmBuilderError)
  Emitter(@codegen.EmitterError)
} derive(Show)

fn read_source_file_char(index: Int) -> Char = "host" "read_source_file_char"
fn get_source_file_length() -> Int = "host" "get_source_file_length"

fn output_char(c: Char, done: Bool) = "host" "output_char"

fn read_source_file() -> String {
  let len = get_source_file_length()
  let chars = []

  for i in 0..<len {
    chars.push(read_source_file_char(i).to_string())
  }

  chars.join("")
}

fn lex_stage(source: String) -> Array[@parse.Token]!CompilerError {
  let lexer = @parse.Lexer::new(source)

  match lexer.lex?() {
    Ok(tokens) => tokens
    Err(error) => raise Lexer(error)
  }
}

fn parse_stage(source: String) -> @parse.Program!CompilerError {
  let tokens = lex_stage!(source)
  let parser = @parse.Parser::new(tokens)

  match parser.parse?() {
    Ok(program) => program
    Err(error) => raise Parser(error)
  }
}

fn validate_stage(source: String) -> @parse.Program!CompilerError {
  let program = parse_stage!(source)

  match @validate.validated?(program) {
    Ok(validated) => validated
    Err(error) => raise Validator(error)
  }
}

fn tac_gen_stage(source: String) -> @codegen.TacProgram!CompilerError {
  let program = validate_stage!(source)

  match @codegen.convert_prog?(program) {
    Ok(tac) => tac
    Err(error) => raise TacBuilder(error)
  }
}

fn codegen_stage(source: String) -> @codegen.AsmProgram!CompilerError {
  let program = tac_gen_stage!(source)
  
  match program.asm?() {
    Ok(asm) => asm
    Err(error) => raise AsmBuilder(error)
  }
}

fn assemble(source: String, ~os: @codegen.OperatingSystem) -> String!CompilerError {
  let program = codegen_stage!(source)
  let emitter = @codegen.Emitter::new(os)
  match emitter.emit_program?(program) {
    Ok(_) => Show::to_string(emitter)
    Err(error) => raise Emitter(error)
  }
}

fn output_string(s: String) -> Unit {
  let len = s.length()

  for i in 0..<len {
    output_char(s[i], i == len - 1)
  }
}

pub enum DriverStage {
  Lex; Parse; Validate; Tacky; CodeGen
}

fn compile(stage: DriverStage, os: @codegen.OperatingSystem) -> String!CompilerError {
  let source = read_source_file()

  match stage {
    Lex => {
      let tokens = lex_stage!(source)
      Show::to_string(tokens)
    }
    Parse => {
      let program = parse_stage!(source)
      Show::to_string(program)
    }
    Validate => {
      let program = validate_stage!(source)
      Show::to_string(program)
    }
    Tacky => {
      let program = tac_gen_stage!(source)
      Show::to_string(program)
    }
    CodeGen => {
      assemble!(source, ~os)
    }
  }
}

pub fn drive(stage: DriverStage, os: @codegen.OperatingSystem) -> Unit {
  match compile?(stage, os) {
    Ok(output) => output_string("ok:" + output)
    Err(error) => output_string("err:" + Show::to_string(error))
  }
}
