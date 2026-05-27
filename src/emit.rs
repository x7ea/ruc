use crate::*;

pub const ABI: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl Define {
    pub fn emit(&self, ctx: &mut Context) -> Result<String, String> {
        let Define::Function(Generics(name, _), args, body) = self else {
            return Ok(String::new());
        };
        if ctx.global.meta.contains(name) {
            return Ok(String::new());
        }
        ctx.local = ctx.table.get(name).unwrap().clone();
        let (mut ptr, mut alloc) = (8usize, String::new());
        let (mut idx, mut xmm) = (0, 0);
        for (count, (_, typ)) in args.iter().enumerate() {
            if let Type::Float = typ {
                if xmm < 8 {
                    alloc += &format!("\tmovsd [rbp-{ptr}], xmm{xmm}\n")
                } else {
                    alloc += &format!(
                        "\tmovsd xmm0, [rbp+{}]\n\tmovsd [rbp-{ptr}], xmm0\n",
                        (count - 4) * 8
                    )
                };
                xmm += 1;
            } else {
                if let Some(reg) = ABI.get(idx) {
                    alloc += &format!("\tmov [rbp-{ptr}], {reg}\n")
                } else {
                    alloc += &format!(
                        "\tmov rax, [rbp+{}]\n\tmov [rbp-{ptr}], rax\n",
                        (count - 4) * 8
                    )
                }
                idx += 1;
            }
            ptr += 8;
        }
        let body = body.emit(ctx)?;
        ctx.table.insert(name.clone(), ctx.local.clone());

        let stack = ctx.local.var.len() * 8;
        let pro = format!(
            "\tpush rbp\n\tmov rbp, rsp\n\tsub rsp, {}\n",
            if stack % 16 == 0 { stack } else { stack + 8 }
        );
        Ok(format!("{name}:\n{pro}{alloc}{body}\tleave\n\tret\n\n"))
    }
}

impl Expr {
    fn emit(&self, ctx: &mut Context) -> Result<String, String> {
        macro_rules! op {
            ($asm: literal, $lhs: expr, $rhs: expr) => {
                match typ!(self) {
                    Type::Integer | Type::Bool => format!(
                        "{}\tpush rax\n{}\tmov r10, rax\n\tpop rax\n\t{} rax, r10\n",
                        $lhs.emit(ctx)?,
                        $rhs.emit(ctx)?,
                        $asm,
                    ),
                    Type::Float => format!(
                        "{lhs}{push}{rhs}\tmovsd xmm1, xmm0\n{pop}\t{op}sd xmm0, xmm1\n",
                        lhs = $lhs.emit(ctx)?,
                        rhs = $rhs.emit(ctx)?,
                        op = $asm.replace("imul", "mul"),
                        push = "\tsub rsp, 8\n\tmovsd [rsp], xmm0\n",
                        pop = "\tmovsd xmm0, [rsp]\n\tadd rsp, 8\n"
                    ),
                    _ => panic!(),
                }
            };
        }
        macro_rules! cmp {
            ($op: literal, $lhs: expr , $rhs: expr) => {
                format!(
                    "{}\tset{} al\n\tmovzx rax, al\n",
                    op!("cmp", $lhs, $rhs),
                    $op
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
        macro_rules! typ {
            ($expr: expr) => {
                ctx.local.typed.get($expr).unwrap().clone()
            };
        }
        macro_rules! expr {
            ($expr: expr) => {
                ctx.local.expand.get($expr).unwrap().clone()
            };
        }
        match self {
            Expr::Print(_) => expr!(self).emit(ctx),
            Expr::If(cond, then, els) => {
                let if_let = ctx.local.expand.get(&self.clone());
                if let Some(expr) = if_let.cloned() {
                    return expr.emit(ctx);
                }
                let id = label!();
                let [cond, then] = [cond.emit(ctx)?, then.emit(ctx)?];
                Ok(if let Some(els) = els {
                    let cmp = format!("\tcmp rax, 0\n\tje else.{id}\n");
                    format!(
                        "{cond}{cmp}{then}\tjmp if.{id}\nelse.{id}:\n{}if.{id}:\n",
                        els.emit(ctx)?,
                    )
                } else {
                    format!("{cond}\tcmp rax, 0\n\tje if.{id}\n{then}if.{id}:\n")
                })
            }
            Expr::While(cond, body) => {
                let id = label!();
                let cmp = format!("\tcmp rax, 0\n\tje do.{id}\n");
                Ok(format!(
                    "while.{id}:\n{}{cmp}{}\tjmp while.{id}\ndo.{id}:\n",
                    cond.emit(ctx)?,
                    body.emit(ctx)?,
                ))
            }
            Expr::Block(lines) => {
                let mut block = String::new();
                for line in lines {
                    block += &line.emit(ctx)?;
                }
                Ok(block)
            }
            Expr::Call(callee, args) => {
                let mut push = String::new();
                for arg in args.iter().rev() {
                    push += &arg.emit(ctx)?;
                    if typ!(arg) == Type::Float {
                        push += "\tsub rsp, 8\n\tmovsd [rsp], xmm0\n"
                    } else {
                        push += "\tpush rax\n"
                    };
                }
                let mut mov = String::new();
                let (mut idx, mut xmm) = (0, 0);
                for arg in args.iter() {
                    if typ!(arg) == Type::Float {
                        if xmm < 8 {
                            mov += &format!("\tmovsd xmm{xmm}, [rsp]\n\tadd rsp, 8\n");
                        }
                        xmm += 1;
                    } else {
                        if let Some(reg) = ABI.get(idx) {
                            mov += &format!("\tpop {reg}\n");
                        }
                        idx += 1;
                    }
                }
                Ok(format!(
                    "{push}{mov}{}\tmov r10, rax\n\tmov rax, {xmm}\n\tcall r10\n",
                    callee.emit(ctx)?
                ))
            }
            Expr::Variable(var @ Generics(name, _)) => {
                let env = &ctx.local.var;
                if let Some(i) = env.get_index_of(name) {
                    let typ = env.get(name).unwrap();
                    let addr = (i + 1) * 8;
                    if let Type::Float = typ {
                        Ok(format!("\tmovsd xmm0, [rbp-{addr}]\n"))
                    } else {
                        Ok(format!("\tmov rax, [rbp-{addr}]\n"))
                    }
                } else {
                    let name = var.generics();
                    Ok(format!("\tlea rax, [{name}]\n"))
                }
            }
            Expr::Let(name, value) => match &**name {
                Expr::Variable(Generics(name, _)) => {
                    let env = &ctx.local.var;
                    let idx = env.get_index_of(name).unwrap();
                    let typ = env.get(name).unwrap().clone();
                    let (value, addr) = (value.emit(ctx)?, (idx + 1) * 8);
                    if let Type::Float = typ {
                        Ok(format!("{value}\tmovsd [rbp-{addr}], xmm0\n"))
                    } else {
                        Ok(format!("{value}\tmov [rbp-{addr}], rax\n"))
                    }
                }
                _ => expr!(self).emit(ctx),
            },
            Expr::Array(_, len) => {
                let setlen = &format!("\tmov qword [rax], {len}\n");
                Ok(expr!(self).emit(ctx)? + setlen)
            }
            Expr::New(_) => expr!(self).emit(ctx),
            Expr::Len(_) => expr!(self).emit(ctx),
            Expr::Index(_, _) => expr!(self).emit(ctx),
            Expr::Member(_, _) => expr!(self).emit(ctx),
            Expr::Check(expr) => {
                let is_enum = ctx.local.expand.get(&self.clone());
                if let Some(expr) = is_enum.cloned() {
                    return expr.emit(ctx);
                }
                Ok(format!(
                    "{}\tcmp rax, 0\nsetne al\n\tmovzx rax, al\n",
                    expr.emit(ctx)?
                ))
            }
            Expr::Read(offset, typ, addr) => {
                let id = label!();
                let [addr, offset] = [addr.emit(ctx)?, offset.emit(ctx)?];
                let guard = format!("\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje null.{id}\n");
                let calc = format!("\tlea rax, [r11+rax*8]\n");
                Ok(format!(
                    "{addr}{guard}\tpush rax\n{offset}\tpop r11\n{calc}{}null.{id}:\n",
                    if let Type::Float = typ {
                        "\tmovsd xmm0, [rax]\n"
                    } else {
                        "\tmov rax, [rax]\n"
                    }
                ))
            }
            Expr::Write(offset, value, addr) => {
                let id = label!();
                let [addr, value, offset] = [addr.emit(ctx)?, value.emit(ctx)?, offset.emit(ctx)?];
                let guard = format!("\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje null.{id}\n");
                let calc = format!("\tpush rax\n{offset}\tpop r11\n\tlea r11, [r11+rax*8]\n");
                Ok(format!(
                    "{addr}{guard}{calc}\tpush r11\n{value}\tpop r11\n{}null.{id}:\n",
                    if let Type::Float = typ!(self) {
                        "\tmovsd [r11], xmm0\n"
                    } else {
                        "\tmov [r11], rax\n"
                    }
                ))
            }
            Expr::Integer(value) => Ok(format!("\tmov rax, {value}\n")),
            Expr::Bool(value) => Expr::Integer(if *value { 1 } else { 0 }).emit(ctx),
            Expr::Float(value) => {
                let name = format!("float.{}", label!());
                ctx.global.data += &format!("\t{name} dq {value:?}\n");
                Ok(format!("\tmovsd xmm0, [{name}]\n"))
            }
            Expr::String(value) => {
                let value = format!("\"{value}\", 0")
                    .replace("\\t", "\", 9, \"")
                    .replace("\\n", "\", 10, \"")
                    .replace("\\\"", "\", 34, \"")
                    .replace("\"\", ", "");
                let name = format!("str.{}", label!());
                ctx.global.data += &format!("\t{name} db {value}\n");
                Ok(format!("\tmov rax, {name}\n"))
            }
            Expr::Div(lhs, rhs) => {
                if let Type::Float = typ!(self) {
                    return Ok(op!("div", lhs, rhs));
                }
                Ok(format!(
                    "{}\tpush rax\n{}\tmov rsi, rax\n\tpop rax\n\tcqo\n\tidiv rsi\n",
                    lhs.emit(ctx)?,
                    rhs.emit(ctx)?,
                ))
            }
            Expr::Mod(_, _) => {
                let correct = "\tadd rdx, rsi\n\tmov rax, rdx\n\tcqo\n\tidiv rsi\n\tmov rax, rdx\n";
                Ok(expr!(self).emit(ctx)? + correct)
            }
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
            Expr::Null(_) => expr!(self).emit(ctx),
        }
    }
}
