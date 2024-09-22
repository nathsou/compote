
pub enum TacValue {
  Constant(Int)
  Variable(String)
} derive(Show)

pub enum TacInst {
  Return(TacValue)
  Unary(~op: @parse.UnaryOp, ~src: TacValue, ~dst: TacValue)
  Binary(~op: @parse.BinaryOp, ~lhs: TacValue, ~rhs: TacValue, ~dst: TacValue)
  Copy(~src: TacValue, ~dst: TacValue)
  Jump(String)
  JumpIfZero(~cond: TacValue, ~target: String)
  JumpIfNotZero(~cond: TacValue, ~target: String)
  Label(String)
} derive(Show)

pub struct TacFunctionDef {
  name: String
  body: Array[TacInst]
} derive(Show)

pub struct TacProgram {
  functions: Array[TacFunctionDef]
}

pub impl Show for TacProgram with output(self, logger: Logger) {
  for function in self.functions {
    logger.write_string("function ")
    logger.write_string(function.name)
    logger.write_string("() {\n")
    
    for inst in function.body {
      logger.write_string("  ")
      logger.write_string(Show::to_string(inst))
      logger.write_string("\n")
    }
    
    logger.write_string("}\n")
  }
}
