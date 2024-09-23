
pub struct PrettyPrinter {
  mut indent: Int
  mut _spaces: String
  logger: Logger
}

pub fn PrettyPrinter::new(logger: Logger) -> PrettyPrinter {
  { indent: 0, _spaces: "", logger }
}

fn update_indentation(self: PrettyPrinter, new_indent: Int) -> Unit {
  self.indent = new_indent
  self._spaces = "  ".repeat(new_indent)
}

fn indent(self: PrettyPrinter) -> Unit {
  self.update_indentation(self.indent + 1)
}

fn dedent(self: PrettyPrinter) -> Unit {
  self.update_indentation(self.indent - 1)
}

fn emit(self: PrettyPrinter, s: String) -> Unit {
  self.logger.write_string(self._spaces + s + "\n")
}

fn write_string(self: PrettyPrinter, s: String) -> Unit {
  self.logger.write_string(s)
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

pub fn show_expr(expr: Expr) -> String {
  match expr {
    Constant(n) => n.to_string()
    Variable(name) => name
    Unary(~op, ~expr) => {
      match op {
        Inc(~is_prefix=true) => "++" + show_expr(expr)
        Dec(~is_prefix=true) => "--" + show_expr(expr)
        _ => Show::to_string(op) + "(" + show_expr(expr) + ")"
      }
    }
    Binary(~op, ~lhs, ~rhs) => "(" + show_expr(lhs) + " " + Show::to_string(op) + " " + show_expr(rhs) + ")"
    Assignment(~op, ~lhs, ~rhs) => show_expr(lhs) + " " + Show::to_string(op) + " " + show_expr(rhs)
    Conditional(~cond, ~then, ~otherwise) => show_expr(cond) + " ? " + show_expr(then) + " : " + show_expr(otherwise)
  }
}

fn show_decl(self: PrettyPrinter, decl: Decl) -> Unit {
  match decl {
    VariableDecl(~ty, ~name, ~value) => {
      match value {
        Some(expr) => {
          self.emit(Show::to_string(ty) + " " + name + " = " + show_expr(expr) + ";")
        }
        None => {
          self.emit(Show::to_string(ty) + " " + name + ";")
        }
      }
    }
  }
}

pub fn show_block_item(self: PrettyPrinter, item: BlockItem) -> Unit {
  match item {
    Decl(decl) => {
      self.show_decl(decl)
    }
    Stmt(stmt) => {
      self.show_stmt(stmt)
    }
  }
}

pub fn show_block(self: PrettyPrinter, block: Array[BlockItem]) -> Unit {
  for item in block {
    self.show_block_item(item)
  }
}

pub fn show_stmt(self: PrettyPrinter, stmt: Stmt) -> Unit {
  match stmt {
    Return(expr) => {
      self.emit("return " + show_expr(expr) + ";")
    }
    Expression(expr) => {
      self.emit(show_expr(expr) + ";")
    }
    If(~cond, ~then, ~otherwise) => {
      self.emit("if (" + show_expr(cond) + ") ")
      self.show_stmt(then)
      match otherwise {
        Some(otherwise) => {
          self.emit("else ")
          self.show_stmt(otherwise)
        }
        None => ()
      }
    }
    Compound(items) => {
      self.emit("{")
      self.indent()
      self.show_block(items)
      self.dedent()
      self.emit("}")
    }
    Labeled(~label, ~stmt) => {
      self.emit(label + ":")
      self.indent()
      self.show_stmt(stmt)
      self.dedent()
    }
    Goto(label) => {
      self.emit("goto " + label + ";")
    }
    Null => {
      self.emit(";")
    }
  }
}

pub fn show_function_def(self: PrettyPrinter, func: FunctionDef) -> Unit {
  self.emit(Show::to_string(func.return_type) + " " + func.name + "() {")
  self.indent()
  self.show_block(func.body)
  self.dedent()
  self.emit("}")
}

pub fn show_program(self: PrettyPrinter, program: Program) -> Unit {
  for func in program.functions {
    self.show_function_def(func)
  }
}

pub impl Show for Expr with output(self, logger: Logger) {
  logger.write_string(show_expr(self))
}

pub impl Show for FunctionDef with output(self, logger: Logger) {
  let printer = PrettyPrinter::new(logger)
  printer.show_function_def(self)
}

pub impl Show for Program with output(self, logger: Logger) {
  let printer = PrettyPrinter::new(logger)
  printer.show_program(self)
}
