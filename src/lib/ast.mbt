
pub enum Type {
  Int; Void
} derive(Eq)

pub enum Expr {
  Constant(Int)
  Unary(~op: UnaryOp, ~expr: Expr)
  Binary(~op: BinaryOp, ~lhs: Expr, ~rhs: Expr)
}

pub enum Stmt {
  Return(Expr)
}

pub enum Decl {
  Function(~returnType: Type, ~name: String, ~body: Stmt)
}

pub struct Program {
  decls: Array[Decl]
}

pub fn Program::new(decls: Array[Decl]) -> Program {
  { decls, }
}

pub enum UnaryOp {
  Neg; Complement; Not
} derive(Eq)

pub enum BinaryOp {
  Add; Sub; Mul; Div; Rem
  BitwiseAnd; BitwiseOr; BitwiseXor;
  Shl; Shr
  Equ; Neq; Lss; Leq; Gtr; Geq
  LogicalAnd; LogicalOr
} derive(Eq)

// https://en.wikipedia.org/wiki/Operators_in_C_and_C++#Operator_precedence
pub fn precedence(self: BinaryOp) -> Int {
  match self {
    Mul | Div | Rem => 50
    Add | Sub => 45
    Shl | Shr => 40
    Lss | Leq | Gtr | Geq => 35
    Equ | Neq => 30
    BitwiseAnd => 30
    BitwiseXor => 25
    BitwiseOr => 20
    LogicalAnd => 10
    LogicalOr => 5
  }
}

pub impl Show for Type with output(self, logger: Logger) {
  logger.write_string(match self {
    Int => "int"
    Void => "void"
  })
}

pub impl Show for UnaryOp with output(self, logger: Logger) {
  logger.write_string(match self {
    Neg => "-"
    Complement => "~"
    Not => "!"
  })
}

pub impl Show for BinaryOp with output(self, logger: Logger) {
  logger.write_string(match self {
    Add => "+"
    Sub => "-"
    Mul => "*"
    Div => "/"
    Rem => "%"
    BitwiseAnd => "&"
    BitwiseOr => "|"
    BitwiseXor => "^"
    Shl => "<<"
    Shr => ">>"
    Equ => "=="
    Neq => "!="
    Lss => "<"
    Leq => "<="
    Gtr => ">"
    Geq => ">="
    LogicalAnd => "&&"
    LogicalOr => "||"
  })
}

pub impl Show for Expr with output(self, logger: Logger) {
  match self {
    Constant(n) => {
      logger.write_string(n.to_string())
    }
    Unary(~op, ~expr) => {
      logger.write_string(Show::to_string(op))
      logger.write_string("(")
      logger.write_string(Show::to_string(expr))
      logger.write_string(")")
    }
    Binary(~op, ~lhs, ~rhs) => {
      logger.write_string("(\{Show::to_string(lhs)} \{Show::to_string(op)} \{Show::to_string(rhs)})")
    }
  }
}

pub impl Show for Stmt with output(self, logger: Logger) {
  match self {
    Return(expr) => {
      logger.write_string("return \{Show::to_string(expr)};")
    }
  }
}

pub impl Show for Decl with output(self, logger: Logger) {
  match self {
    Function(~returnType, ~name, ~body) => {
      logger.write_string(Show::to_string(returnType))
      logger.write_string(" ")
      logger.write_string(name)
      logger.write_string("(void) { ")
      logger.write_string(Show::to_string(body))
      logger.write_string(" }")
    }
  }
}

pub impl Show for Program with output(self, logger: Logger) {
  for decl in self.decls {
    logger.write_string(Show::to_string(decl))
    logger.write_char('\n')
  }
}
