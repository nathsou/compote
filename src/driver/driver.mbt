
priv suberror CompilerError {
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
  let builder = StringBuilder::new()

  for i in 0..<len {
    builder.write_char(read_source_file_char(i))
  }

  builder.to_string()
}

fn lex_stage(source: String) -> Array[@parse.Token] raise CompilerError {
  let lexer = @parse.Lexer::new(source)

  match (try? lexer.tokenize()) {
    Ok(tokens) => tokens
    Err(error) => raise Lexer(error)
  }
}

fn parse_stage(source: String) -> @parse.Program raise CompilerError {
  let tokens = lex_stage(source)
  let parser = @parse.Parser::new(tokens)

  match (try? parser.parse()) {
    Ok(program) => program
    Err(error) => raise Parser(error)
  }
}

fn validate_stage(source: String) -> (@parse.Program, Map[String, @validate.SymbolTableEntry]) raise CompilerError {
  let program = parse_stage(source)

  match (try? @validate.validated(program)) {
    Ok(res) => res
    Err(error) => raise Validator(error)
  }
}

fn tac_gen_stage(source: String) -> (@codegen.TacProgram, Map[String, @validate.SymbolTableEntry]) raise CompilerError {
  let (program, symbols) = validate_stage(source)

  match (try? @codegen.convert_prog(program, symbols)) {
    Ok(tac) => (tac, symbols)
    Err(error) => raise TacBuilder(error)
  }
}

fn codegen_stage(source: String) -> (@codegen.AsmProgram, Map[String, @validate.SymbolTableEntry]) raise CompilerError {
  let (program, symbols) = tac_gen_stage(source)
  
  match (try? program.to_asm(symbols)) {
    Ok(asm_prog) => (asm_prog, symbols)
    Err(error) => raise AsmBuilder(error)
  }
}

fn assemble(source: String, os~: @codegen.OperatingSystem) -> String raise CompilerError {
  let (program, symbols) = codegen_stage(source)
  let emitter = @codegen.Emitter::new(os, symbols)
  match (try? emitter.emit_program(program)) {
    Ok(_) => Show::to_string(emitter)
    Err(error) => raise Emitter(error)
  }
}

fn output_string(s: String) -> Unit {
  let len = s.length()

  for i in 0..<len {
    output_char(s[i].to_char().unwrap(), i == len - 1)
  }
}

pub enum DriverStage {
  Lex; Parse; Validate; Tacky; CodeGen
}

fn compile(stage: DriverStage, os: @codegen.OperatingSystem) -> String raise CompilerError {
  let source = read_source_file()

  match stage {
    Lex => {
      let tokens = lex_stage(source)
      tokens.map(Show::to_string).join(", ")
    }
    Parse => {
      let program = parse_stage(source)
      Show::to_string(program)
    }
    Validate => {
      let (program, _) = validate_stage(source)
      Show::to_string(program)
    }
    Tacky => {
      let (program, _) = tac_gen_stage(source)
      Show::to_string(program)
    }
    CodeGen => {
      assemble(source, os~)
    }
  }
}

pub fn drive(stage: DriverStage, os: @codegen.OperatingSystem) -> Unit {
  match (try? compile(stage, os)) {
    Ok(output) => output_string("ok: \{output}")
    Err(error) => output_string("err: \{error}")
  }
}
