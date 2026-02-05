
using @parse { type LexerError, type ParserError, type Token, type Program, type Lexer, type Parser }
using @validate { type ValidationError, type SymbolTableEntry, validated }
using @codegen { type TacBuilderError, type AsmBuilderError, type EmitterError, type TacProgram, type AsmProgram, type OperatingSystem, convert_prog, type Emitter }

priv suberror CompilerError {
  Lexer(LexerError)
  Parser(ParserError)
  Validator(ValidationError)
  TacBuilder(TacBuilderError)
  AsmBuilder(AsmBuilderError)
  Emitter(EmitterError)
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

fn lex_stage(source: String) -> Array[Token] raise CompilerError {
  let lexer = Lexer::new(source)

  try lexer.tokenize() catch {
    error => raise Lexer(error)
  }
}

fn parse_stage(source: String) -> Program raise CompilerError {
  let tokens = lex_stage(source)
  let parser = Parser::new(tokens)

  try parser.parse() catch {
    error => raise Parser(error)
  }
}

fn validate_stage(source: String) -> (Program, Map[String, SymbolTableEntry]) raise CompilerError {
  let program = parse_stage(source)

  try validated(program) catch {
    error => raise Validator(error)
  }
}

fn tac_gen_stage(source: String) -> (TacProgram, Map[String, SymbolTableEntry]) raise CompilerError {
  let (program, symbols) = validate_stage(source)

  let tac = try convert_prog(program, symbols) catch {
    error => raise TacBuilder(error)
  }
  (tac, symbols)
}

fn codegen_stage(source: String) -> (AsmProgram, Map[String, SymbolTableEntry]) raise CompilerError {
  let (program, symbols) = tac_gen_stage(source)
  
  let asm_prog = try program.to_asm(symbols) catch {
    error => raise AsmBuilder(error)
  }
  (asm_prog, symbols)
}

fn assemble(source: String, os~: OperatingSystem) -> String raise CompilerError {
  let (program, symbols) = codegen_stage(source)
  let emitter = Emitter::new(os, symbols)
  try emitter.emit_program(program) catch {
    error => raise Emitter(error)
  }
  Show::to_string(emitter)
}

fn output_string(s: String) -> Unit {
  let len = s.length()

  for i in 0..<len {
    output_char(s[i].to_char().unwrap(), i == len - 1)
  }
}

pub enum DriverStage {
  Lex = 0; Parse; Validate; Tacky; CodeGen; Assemble
}

fn compile(stage: DriverStage, os: OperatingSystem) -> String raise CompilerError {
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
      let (program, _) = codegen_stage(source)
      Show::to_string(program)
    }
    Assemble => {
      assemble(source, os~)
    }
  }
}

pub fn drive(stage: DriverStage, os: OperatingSystem) -> Unit {
  try compile(stage, os) catch {
    error => output_string("err: \{error}")
  } noraise {
    output => output_string("ok: \{output}")
  }
}
