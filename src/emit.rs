use crate::*;

pub const ABI: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl Define {
    pub fn compile(defines: Vec<Self>) -> Result<String, String> {
        let mut lib = String::new();
        let mut text = String::new();
        let ctx = &mut Context::default();

        for func in &defines { 
            text += &func.emit(ctx)?; 
        }
        let data = ctx.global.data.clone();

        for Define(name, _, _) in &defines {
            ctx.global.func.shift_remove(name); 
        }
        for symbol in &ctx.global.func { 
            lib += &format!("\textern {symbol}\n"); 
        }

        Ok(format!("section .data\n{data}\nsection .text\n\tglobal main\n{lib}\n{text}\n"))
    }

    fn emit(&self, ctx: &mut Context) -> Result<String, String> {
        let Define(name, args, body) = self;

        let mut addr = 8usize;
        let mut prologue = String::new();
        for (idx, _arg) in args.iter().enumerate() {
            if let Some(reg) = ABI.get(idx) {
                prologue += &format!("\tmov [rbp-{addr}], {reg}\n");
            } else {
                prologue += &format!(
                    "\tmov rax, [rbp+{}]\n\tmov [rbp-{addr}], rax\n",
                    (idx - 4) * 8
                );
            }
            addr += 8;
        }

        ctx.local = Function::default();
        ctx.local.var = args.clone();

        let body = body.emit(ctx)?;
        let bytes = ctx.local.var.len() * 8;

        Ok(format!(
            "{name}:\n\tpush rbp\n\tmov rbp, rsp\n\tsub rsp, {}\n{prologue}{body}\tleave\n\tret\n\n", 
            if bytes % 16 == 0 { bytes } else { bytes + 8 }
        ))
    }
}

impl Expr {
    fn emit(&self, ctx: &mut Context) -> Result<String, String> {
        macro_rules! op {
            ($asm: literal, $lhs: expr, $rhs: expr) => {
                format!(
                    "{}\tpush rax\n{}\tmov r10, rax\n\tpop rax\n\t{} rax, r10\n",
                    $lhs.emit(ctx)?, $rhs.emit(ctx)?, $asm,
                )
            };
        }
        macro_rules! cmp {
            ($op: literal, $lhs: expr , $rhs: expr) => {
                format!(
                    "{}\tset{} al\n\tmovzx rax, al\n",
                    op!("cmp", $lhs, $rhs), $op
                )
            };
        }
        macro_rules! label {
            () => {{
                let id = ctx.global.idx;
                ctx.global.idx += 1;
                id.to_string()
            }};
        }

        match self {
            Expr::If(cond, then, r#else) => {
                let id = label!();
                if let Some(r#else) = r#else {
                    Ok(format!(
                        "{}\tcmp rax, 0\n\tje else.{id}\n{}\tjmp if.{id}\nelse.{id}:\n{}if.{id}:\n",
                        cond.emit(ctx)?, then.emit(ctx)?, r#else.emit(ctx)?,
                    ))
                } else {
                    Ok(format!(
                        "{}\tcmp rax, 0\n\tje if.{id}\n{}if.{id}:\n",
                        cond.emit(ctx)?, then.emit(ctx)?,
                    ))
                }
            }
            Expr::While(cond, body) => {
                let id = label!();
                ctx.local.jmp.push(id.clone());
                let output = format!(
                    "while.{id}:\n{}\tcmp rax, 0\n\tje do.{id}\n{}\tjmp while.{id}\ndo.{id}:\n",
                    cond.emit(ctx)?, body.emit(ctx)?,
                );
                ctx.local.jmp.pop();
                Ok(output)
            }
            Expr::Break(expr) => {
                if let Some(jmp) = ctx.local.jmp.last().cloned() {
                    Ok(format!("{}\tjmp end_while.{jmp}\n", expr.emit(ctx)?))
                } else {
                    Err(format!("while loop not found"))
                }
            }
            Expr::Return(expr) => Ok(format!("{}\tleave\n\tret\n", expr.emit(ctx)?)),
            Expr::Block(lines) => Ok(lines
                .iter().map(|line| line.emit(ctx))
                .collect::<Result<String, String>>()?),
            Expr::Call(callee, args) => {
                let mut arg_push = String::new();
                let mut arg_mov = String::new();
                for (idx, arg) in args.iter().rev().enumerate() {
                    arg_push += &format!("{}\tpush rax\n", arg.emit(ctx)?);
                    if let Some(reg) = ABI.get(idx) {
                        arg_mov += &format!("\tpop {reg}\n");
                    }
                }
                let prepare = [arg_push, arg_mov, callee.emit(ctx)?].concat();
                Ok(format!("{prepare}\tmov r10, rax\n\txor rax, rax\n\tcall r10\n"))
            }
            Expr::Variable(name) => {
                if let Some(i) = ctx.local.var.get_index_of(name) {
                    Ok(format!("\tmov rax, [rbp-{}]\n", (i + 1) * 8))
                } else {
                    ctx.global.func.insert(name.clone());
                    Ok(format!("\tlea rax, [{name}]\n"))
                }
            }
            Expr::Pointer(var) => {
                if let Some(i) = ctx.local.var.get_index_of(var) {
                    Ok(format!("\tlea rax, [rbp-{}]\n", (i + 1) * 8))
                } else {
                    Err(format!("undefined variable: {var}"))
                }
            }
            Expr::Derefer(expr) => Ok(format!("{}\tmov rax, [rax]\n", expr.emit(ctx)?)),
            Expr::Let(name, value) => match &**name {
                Expr::Variable(name) => {
                    let env = &mut ctx.local.var;
                    let idx = env.get_index_of(name)
                    .unwrap_or({
                        env.insert(name.clone());
                        env.len() - 1
                    });
                    Ok(format!(
                        "{}\tmov [rbp-{}], rax\n",
                        value.emit(ctx)?, (idx + 1) * 8
                    ))
                }
                Expr::Derefer(ptr) => Ok(format!(
                    "{}\tpush rax\n{}\tpop r10\n\tmov [rax], r10\n",
                    value.emit(ctx)?, ptr.emit(ctx)?
                )),
                _ => Err(format!("can't assign to unknown object"))
            }
            Expr::Integer(value) => Ok(format!("\tmov rax, {value}\n")),
            Expr::String(value) => {
                let value = format!("{value}, 0")
                    .replace("\\n", "\", 10, \"")
                    .replace("\\\"", "\", 34, \"")
                    .replace("\"\", ", "");

                let name = format!("str.{}", label!());
                ctx.global.data += &format!("\t{name} db {value}\n");

                Ok(format!("\tmov rax, {name}\n"))
            }
            Expr::Undefined => Ok(String::new()),
            Expr::Add(lhs, rhs) => Ok(op!("add", lhs, rhs)),
            Expr::Sub(lhs, rhs) => Ok(op!("sub", lhs, rhs)),
            Expr::Mul(lhs, rhs) => Ok(op!("imul", lhs, rhs)),
            Expr::Eql(lhs, rhs) => Ok(cmp!("e", lhs, rhs)),
            Expr::NotEq(lhs, rhs) => Ok(cmp!("ne", lhs, rhs)),
            Expr::Gt(lhs, rhs) => Ok(cmp!("g", lhs, rhs)),
            Expr::Lt(lhs, rhs) => Ok(cmp!("l", lhs, rhs)),
            Expr::GtEq(lhs, rhs) => Ok(cmp!("ge", lhs, rhs)),
            Expr::LtEq(lhs, rhs) => Ok(cmp!("le", lhs, rhs)),
            Expr::And(lhs, rhs) => Ok(op!("and", lhs, rhs)),
            Expr::Or(lhs, rhs) => Ok(op!("or", lhs, rhs)),
            Expr::Xor(lhs, rhs) => Ok(op!("xor", lhs, rhs)),
            Expr::Div(lhs, rhs) => Ok(format!(
                "{}\tpush rax\n{}\tmov rsi, rax\n\tpop rax\n\tcqo\n\tidiv rsi\n",
                lhs.emit(ctx)?, rhs.emit(ctx)?,
            )),
            Expr::Mod(lhs, rhs) => {
                let div = Expr::Div(lhs.clone(), rhs.clone());
                Ok(div.emit(ctx)? + "\tmov rax, rdx\n")
            }
        }
    }
}
