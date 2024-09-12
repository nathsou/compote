
pub enum TacValue {
  Constant(Int)
  Variable(String)
} derive(Show)

pub enum TacInst {
  Return(TacValue)
  Unary(~op: UnaryOp, ~src: TacValue, ~dst: TacValue)
} derive(Show)

pub struct TacFunctionDef {
  name: String
  body: Array[TacInst]
} derive(Show)

pub struct TacProgram {
  functions: Array[TacFunctionDef]
}
