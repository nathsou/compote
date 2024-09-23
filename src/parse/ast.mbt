
pub enum Type {
  Int; Void
} derive(Eq)

pub enum Expr {
  Constant(Int)
  Variable(String)
  Unary(~op: UnaryOp, ~expr: Expr)
  Binary(~op: BinaryOp, ~lhs: Expr, ~rhs: Expr)
  Assignment(~op: AssignmentOp, ~lhs: Expr, ~rhs: Expr)
  Conditional(~cond: Expr, ~then: Expr, ~otherwise: Expr)
}

pub enum Stmt {
  Return(Expr)
  Expression(Expr)
  If(~cond: Expr, ~then: Stmt, ~otherwise: Option[Stmt])
  Labeled(~label: String, ~stmt: Stmt)
  Goto(String)
  Null // for empty statements (just a semicolon)
}

pub enum BlockItem {
  Stmt(Stmt)
  Decl(Decl)
}

pub struct FunctionDef {
  return_type: Type
  name: String
  body: Array[BlockItem]
}

pub enum Decl {
  VariableDecl(~ty: Type, ~name: String, ~value: Option[Expr])
}

pub struct Program {
  functions: Array[FunctionDef]
}

pub enum UnaryOp {
  Neg; Complement; Not; Inc(~is_prefix: Bool); Dec(~is_prefix: Bool)
} derive(Eq)

pub enum BinaryOp {
  Add; Sub; Mul; Div; Rem
  BitwiseAnd; BitwiseOr; BitwiseXor;
  Shl; Shr
  Equ; Neq; Lss; Leq; Gtr; Geq
  LogicalAnd; LogicalOr
  Ternary
  Assignment(AssignmentOp)
} derive(Eq)

pub enum AssignmentOp {
  Assign; AssignAdd; AssignSub; AssignMul; AssignDiv; AssignRem
  AssignAnd; AssignOr; AssignXor; AssignShl; AssignShr
} derive(Eq)

// https://en.wikipedia.org/wiki/Operators_in_C_and_C++#Operator_precedence
pub fn precedence(self: BinaryOp) -> Int {
  match self {
    Mul | Div | Rem => 50
    Add | Sub => 45
    Shl | Shr => 40
    Lss | Leq | Gtr | Geq => 35
    Equ | Neq => 30
    BitwiseAnd => 28
    BitwiseXor => 25
    BitwiseOr => 20
    LogicalAnd => 10
    LogicalOr => 5
    Ternary => 3
    Assignment(_) => 1
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
    Inc(_) => "++"
    Dec(_) => "--" 
  })
}

pub impl Show for AssignmentOp with output(self, logger: Logger) {
  logger.write_string(match self {
    Assign => "="
    AssignAdd => "+="
    AssignSub => "-="
    AssignMul => "*="
    AssignDiv => "/="
    AssignRem => "%="
    AssignAnd => "&="
    AssignOr => "|="
    AssignXor => "^="
    AssignShl => "<<="
    AssignShr => ">>="
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
    Ternary => "?:"
    Assignment(op) => Show::to_string(op)
  })
}

pub impl Show for Expr with output(self, logger: Logger) {
  match self {
    Constant(n) => {
      logger.write_string(n.to_string())
    }
    Variable(name) => {
      logger.write_string(name)
    }
    Unary(~op, ~expr) => {
      match op {
        Inc(~is_prefix=true) => {
          logger.write_string("++")
          logger.write_string(Show::to_string(expr))
        }
        Dec(~is_prefix=true) => {
          logger.write_string("--")
          logger.write_string(Show::to_string(expr))
        }
        _ => {
          logger.write_string(Show::to_string(op))
          logger.write_string("(")
          logger.write_string(Show::to_string(expr))
          logger.write_string(")")
        }
      }
    }
    Binary(~op, ~lhs, ~rhs) => {
      logger.write_string("(\{Show::to_string(lhs)} \{Show::to_string(op)} \{Show::to_string(rhs)})")
    }
    Assignment(~op, ~lhs, ~rhs) => {
      logger.write_string("\{Show::to_string(lhs)} \{Show::to_string(op)} \{Show::to_string(rhs)}")
    }
    Conditional(~cond, ~then, ~otherwise) => {
      logger.write_string("\{Show::to_string(cond)} ? \{Show::to_string(then)} : \{Show::to_string(otherwise)}")
    }
  }
}

pub impl Show for Stmt with output(self, logger: Logger) {
  match self {
    Return(expr) => {
      logger.write_string("return \{Show::to_string(expr)};")
    }
    Expression(expr) => {
      logger.write_string("\{Show::to_string(expr)};")
    }
    If(~cond, ~then, ~otherwise) => {
      logger.write_string("if (\{Show::to_string(cond)}) {\n")
      logger.write_string("\{Show::to_string(then)}\n")
      logger.write_string("}")

      match otherwise {
        Some(stmt) => {
          logger.write_string(" else {\n")
          logger.write_string("\{Show::to_string(stmt)}\n")
          logger.write_string("}")
        }
        None => ()
      }
    }
    Labeled(~label, ~stmt) => {
      logger.write_string("\{label}: \{Show::to_string(stmt)}")
    }
    Goto(label) => {
      logger.write_string("goto \{label};")
    }
    Null => {
      logger.write_string(";")
    }
  }
}

pub impl Show for Decl with output(self, logger: Logger) {
  match self {
    VariableDecl(~ty, ~name, ~value) => {
      logger.write_string(Show::to_string(ty))
      logger.write_string(" ")
      logger.write_string(name)

      match value {
        Some(expr) => {
          logger.write_string(" = ")
          logger.write_string(Show::to_string(expr))
        }
        None => ()
      }

      logger.write_string(";")
    }
  }
}

pub impl Show for BlockItem with output(self, logger: Logger) {
  match self {
    Stmt(stmt) => {
      logger.write_string(Show::to_string(stmt))
    }
    Decl(decl) => {
      logger.write_string(Show::to_string(decl))
    }
  }
}

pub impl Show for FunctionDef with output(self, logger: Logger) {
    logger.write_string(Show::to_string(self.return_type))
    logger.write_string(" ")
    logger.write_string(self.name)
    logger.write_string("(void) {\n")
    logger.write_string(self.body.join("\n"))
    logger.write_string("\n}")
}

pub impl Show for Program with output(self, logger: Logger) {
  for function in self.functions {
    logger.write_string(Show::to_string(function))
    logger.write_string("\n")
  }
}
