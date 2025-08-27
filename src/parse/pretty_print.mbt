
pub struct PrettyPrinter {
  mut indent: Int
  mut _spaces: String
  logger: &Logger
}

pub fn PrettyPrinter::new(logger: &Logger) -> PrettyPrinter {
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

pub impl Show for Type with output(self, logger: &Logger) {
  logger.write_string(match self {
    Int => "int"
    Long => "long"
    UnsignedInt => "unsigned int"
    UnsignedLong => "unsigned long"
    FunType(params~, ret~) => "fun(" + String::concat(params.map(Show::to_string), separator=", ") + ") -> " + Show::to_string(ret)
  })
}

pub impl Show for UnaryOp with output(self, logger: &Logger) {
  logger.write_string(match self {
    Neg => "-"
    Complement => "~"
    Not => "!"
    Inc(_) => "++"
    Dec(_) => "--" 
  })
}

pub impl Show for AssignmentOp with output(self, logger: &Logger) {
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

pub impl Show for BinaryOp with output(self, logger: &Logger) {
  logger.write_string(match self {
    Add => "+"
    Sub => "-"
    Mul => "*"
    Div => "/"
    Rem => "%"
    BitwiseAnd => "&"
    BitwiseOr => "|"
    BitwiseXor => "^"
    ShiftLeft => "<<"
    ShiftRight => ">>"
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

pub impl Show for StorageClass with output(self, logger: &Logger) {
  logger.write_string(match self {
    Static => "static"
    Extern => "extern"
  })
}

pub impl Show for Constant with output(self, logger: &Logger) {
  logger.write_string(match self {
    IntConst(n) => n.to_string()
    LongConst(n) => n.to_string() + "L"
    UnsignedIntConst(n) => n.to_string() + "U"
    UnsignedLongConst(n) => n.to_string() + "UL"
  })
}

pub fn show_expr(expr: Expr) -> String {
  match expr {
    Constant(val=n, ty=_) => Show::to_string(n)
    Variable(name~, ty=_) => name
    Cast(target_ty~, expr~, ty=_) => "((" + Show::to_string(target_ty) + ")" + show_expr(expr) + ")"
    Unary(op~, expr~, ty=_) => {
      match op {
        Inc(is_prefix=false) => "(" + show_expr(expr) + ")++"
        Dec(is_prefix=true) => "(" + show_expr(expr)
        _ => Show::to_string(op) + "(" + show_expr(expr) + ")"
      }
    }
    Binary(op~, lhs~, rhs~, ty=_) => "(" + show_expr(lhs) + " " + Show::to_string(op) + " " + show_expr(rhs) + ")"
    Assignment(op~, lhs~, rhs~, ty=_) => show_expr(lhs) + " " + Show::to_string(op) + " " + show_expr(rhs)
    Conditional(cond~, then~, otherwise~, ty=_) => show_expr(cond) + " ? " + show_expr(then) + " : " + show_expr(otherwise)
    Call(name~, args~, ty=_) => name + "(" + (args.map(show_expr) |> String::concat(separator=", ")) + ")"
  }
}

fn show_decl(self: PrettyPrinter, decl: Decl) -> Unit {
  match decl {
    VarDecl({ ty, name, value, storage_class }) => {
      let storage_class_str = match storage_class {
        Some(storage_class) => Show::to_string(storage_class) + " "
        None => ""
      }
      
      match value {
        Some(expr) => {
          self.emit(storage_class_str + Show::to_string(ty) + " " + name + " = " + show_expr(expr) + ";")
        }
        None => {
          self.emit(storage_class_str + Show::to_string(ty) + " " + name + ";")
        }
      }
    }
    FunDecl(decl) => {
      self.show_function_decl(decl)
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
    If(cond~, then~, otherwise~) => {
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
    Labeled(label~, stmt~) => {
      self.emit(label + ":")
      self.indent()
      self.show_stmt(stmt)
      self.dedent()
    }
    Goto(label) => {
      self.emit("goto " + label + ";")
    }
    Break(label~) => {
      match label {
        Some(label) => {
          self.emit("break " + label + ";")
        }
        None => {
          self.emit("break;")
        }
      }
    }
    Continue(label~) => {
      match label {
        Some(label) => {
          self.emit("continue " + label + ";")
        }
        None => {
          self.emit("continue;")
        }
      }
    }
    While(cond~, body~, label=_) => {
      self.emit("while (" + show_expr(cond) + ") ")
      self.show_stmt(body)
    }
    DoWhile(body~, cond~, label=_) => {
      self.emit("do ")
      self.show_stmt(body)
      self.emit("while (" + show_expr(cond) + ");")
    }
    For(init~, cond~, post~, body~, label=_) => {
      self.emit("for (")

      match init {
        InitEmpty => {
          self.write_string(";")
        }
        InitDecl(decl) => {
          self.show_decl(VarDecl(decl))
        }
        InitExpr(expr) => {
          self.write_string(show_expr(expr) + ";")
        }
      }

      match cond {
        Some(expr) => {
          self.write_string(show_expr(expr) + ";")
        }
        None => {
          self.write_string(";")
        }
      }

      match post {
        Some(expr) => {
          self.write_string(show_expr(expr))
        }
        None => ()
      }

      self.write_string(") ")
      self.show_stmt(body)
    }
    Switch(expr~, body~, label=_, cases_count=_) => {
      self.emit("switch (" + show_expr(expr) + ")")
      self.show_stmt(body)
    }
    Case(expr~, body~) => {
      self.emit("case " + show_expr(expr) + ":")
      self.indent()
      self.show_stmt(body)
      self.dedent()
    }
    Default(body~) => {
      self.emit("default:")
      self.indent()
      self.show_stmt(body)
      self.dedent()
    }
    Null => {
      self.emit(";")
    }
  }
}

pub fn show_function_decl(self: PrettyPrinter, func: FunctionDecl) -> Unit {
  let storage_class = match func.storage_class {
    Some(storage_class) => Show::to_string(storage_class) + " "
    None => ""
  }

  let params = func.params.map(fn(param) {
    Show::to_string(param.ty) + " " + param.name
  }) |> String::concat(separator=", ")

  self.emit(storage_class + Show::to_string(func.return_ty) + " " + func.name + "(" + params + ")" + (if func.body.is_empty() {
    ";"
  } else {
    " {"
  }))

  match func.body {
    Some(body) => {
      self.indent()
      self.show_block(body)
      self.dedent()
      self.emit("}")
    }
    None => ()
  }

  self.emit("")
}

pub fn show_program(self: PrettyPrinter, program: Program) -> Unit {
  for decl in program.decls {
    self.show_decl(decl)
  }
}

pub impl Show for Expr with output(self, logger: &Logger) {
  logger.write_string(show_expr(self))
}

pub impl Show for FunctionDecl with output(self, logger: &Logger) {
  let printer = PrettyPrinter::new(logger)
  printer.show_function_decl(self)
}

pub impl Show for Program with output(self, logger: &Logger) {
  let printer = PrettyPrinter::new(logger)
  printer.show_program(self)
}
