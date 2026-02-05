
pub enum TacValue {
  Constant(@parse.Constant)
  Variable(String)
} derive(Show)

pub enum TacInst {
  Return(TacValue)
  SignExtend(src~: TacValue, dst~: TacValue)
  Truncate(src~: TacValue, dst~: TacValue)
  ZeroExtend(src~: TacValue, dst~: TacValue)
  DoubleToInt(src~: TacValue, dst~: TacValue)
  DoubleToUInt(src~: TacValue, dst~: TacValue)
  IntToDouble(src~: TacValue, dst~: TacValue)
  UIntToDouble(src~: TacValue, dst~: TacValue)
  Unary(op~: @parse.UnaryOp, src~: TacValue, dst~: TacValue)
  Binary(op~: @parse.BinaryOp, lhs~: TacValue, rhs~: TacValue, dst~: TacValue)
  Copy(src~: TacValue, dst~: TacValue)
  Jump(String)
  JumpIfZero(cond~: TacValue, target~: String)
  JumpIfNotZero(cond~: TacValue, target~: String)
  Label(String)
  Call(name~: String, args~: Array[TacValue], dst~: TacValue)
} derive(Show)

pub struct TacFunctionDef {
  name: String
  global: Bool
  params: Array[String]
  body: Array[TacInst]
} derive(Show)

pub enum TacTopLevel {
  Function(TacFunctionDef)
  StaticVar(name~: String, global~: Bool, init~: StaticInit, ty~: Type)
  StaticConst(name~: String, init~: StaticInit, ty~: Type)
} derive(Show)

pub struct TacProgram {
  top_level: Array[TacTopLevel]
}

pub impl Show for TacProgram with output(self, logger: &Logger) {
  for top_level in self.top_level {
    match top_level {
      Function(function) => {
        logger.write_string("function ")
        logger.write_string(function.name)
        logger.write_string("(")
        logger.write_string(function.params.join(", "))
        logger.write_string(") {\n")
        
        for inst in function.body {
          logger.write_string("  ")
          logger.write_string(Show::to_string(inst))
          logger.write_string("\n")
        }
        
        logger.write_string("}\n")
      }
      StaticVar(name~, global=_, init~, ty=_) => {
        logger.write_string("static \{name} = \{init}\n")
      }
      StaticConst(name~, init~, ty=_) => {
        logger.write_string("const \{name} = \{init}\n")
      }
    }
  }
}
