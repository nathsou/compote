pub struct SwitchContext {
  label: String
  cases: @hashset.HashSet[Constant]
  mut has_default: Bool
  ty: Type
}

pub suberror SwitchResolutionError {
  DefaultOutsideSwitch
  MultipleDefaultInSwitch
  CaseExprMustBeConstant
  CaseExprTypeMismatch
  DuplicateCase(Constant)
  UnresolvedSwitchLabel
} derive(Show)

fn analyze_stmt(stmt: Stmt, ctx: SwitchContext?) -> Stmt raise SwitchResolutionError {
  match stmt {
    Default(body~) => {
      let ctx = match ctx {
        Some(ctx) => ctx
        None => raise DefaultOutsideSwitch
      }

      if ctx.has_default {
        raise MultipleDefaultInSwitch
      }

      ctx.has_default = true

      let new_body = analyze_stmt(body, Some(ctx))

      Default(body=new_body)
    }
    Case(expr~, body~) => {
      let ctx = match ctx {
        Some(ctx) => ctx
        None => raise DefaultOutsideSwitch
      }

      let val = match expr {
        Constant(val~, ty=_) => val
        _ => raise CaseExprMustBeConstant
      }

      let fitted_val = try val.fit_val_to_type(ctx.ty) catch {
        _ => raise CaseExprTypeMismatch
      }

      if ctx.cases.contains(fitted_val) {
        raise DuplicateCase(fitted_val)
      }

      ctx.cases.add(fitted_val)

      let new_body = analyze_stmt(body, Some(ctx))

      Case(expr~, body=new_body)
    }
    Switch(expr~, body~, label~, cases_count=_) => {
      let label = match label {
        Some(label) => label
        None => raise UnresolvedSwitchLabel
      }

      let new_ctx: SwitchContext = {
        label,
        cases: @hashset.new(),
        has_default: false,
        ty: expr.ty(),
      }

      let new_body = analyze_stmt(body, Some(new_ctx))
      let cases_count = new_ctx.cases.length() + (if new_ctx.has_default { 1 } else { 0 })

      Switch(expr~, body=new_body, label=Some(label), cases_count~)
    }
    If(cond~, then~, otherwise~) => {
      let new_then = analyze_stmt(then, ctx)
      let new_otherwise = match otherwise {
        Some(stmt) => Some(analyze_stmt(stmt, ctx))
        None => None
      }

      If(cond~, then=new_then, otherwise=new_otherwise)
    }
    Compound(items) => {
      Compound(analyze_block(items, ctx))
    }
    While(cond~, body~, label~) => {
      let new_body = analyze_stmt(body, ctx)
      While(cond~, body=new_body, label~)
    }
    DoWhile(body~, cond~, label~) => {
      let new_body = analyze_stmt(body, ctx)
      DoWhile(body=new_body, cond~, label~)
    }
    For(init~, cond~, post~, body~, label~) => {
      let new_body = analyze_stmt(body, ctx)
      For(init~, cond~, post~, body=new_body, label~)
    }
    Labeled(label~, stmt~) => {
      let new_stmt = analyze_stmt(stmt, ctx)
      Labeled(label~, stmt=new_stmt)
    }
    // Statements not composed of other statements
    Return(_) | Null | Expression(_) | Break(_) | Continue(_) | Goto(_) => stmt
  }
}

fn analyze_block_item(item: BlockItem, ctx: SwitchContext?) -> BlockItem raise SwitchResolutionError {
  match item {
    Stmt(stmt) => {
      let new_stmt = analyze_stmt(stmt, ctx)
      Stmt(new_stmt)
    }
    Decl(decl) => Decl(decl)
  }
}

fn analyze_block(block: Array[BlockItem], ctx: SwitchContext?) -> Array[BlockItem] raise SwitchResolutionError {
  let new_block = []

  for item in block {
    new_block.push(analyze_block_item(item, ctx))
  }

  new_block
}

fn analyze_function_def(f: FunctionDecl) -> FunctionDecl raise SwitchResolutionError {
  let new_body = match f.body {
    Some(body) => Some(analyze_block(body, None))
    None => None
  }

  {
    return_ty: f.return_ty,
    name: f.name,
    params: f.params,
    body: new_body,
    storage_class: f.storage_class,
  }
}

fn analyze_decl(decl: Decl) -> Decl raise SwitchResolutionError {
  match decl {
    VarDecl(decl) => VarDecl(decl)
    FunDecl(decl) => FunDecl(analyze_function_def(decl))
  }
}

pub fn analyze_switch_stmts(program: Program) -> Program raise SwitchResolutionError {
  let new_decls = []

  for decl in program.decls {
    new_decls.push(analyze_decl(decl))
  }

  { decls: new_decls }
}
