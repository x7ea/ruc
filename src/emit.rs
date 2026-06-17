use crate::*;

impl Define {
    const CORE: [&str; 5] = ["calloc", "printf", "g_strdup_printf", "free", "memcpy"];

    pub fn compile(defines: &[Self]) -> Result<String, String> {
        macro_rules! name {
            ($define: expr) => {
                match $define.clone() {
                    Define::Function((Generics(func, _), _), _) => func,
                    Define::Class(Generics(class, _), _) => class,
                    Define::Declare((Generics(lib, _), _), _) => lib,
                }
                .clone()
            };
        }
        let ctx = &mut Context::default();
        ctx.global.def = defines.iter().map(|x| (name!(x), x.clone())).collect();
        ctx.global.lib = {
            let mut map = IndexMap::new();
            for line in Self::CORE {
                let sig = Lambda(vec![], Box::new(Type::Void), None);
                map.insert(Name::new(line)?, Type::Function(sig));
            }
            map
        };
        map!(ctx.global.def.clone(), |(_, x)| x.infer(ctx), ok)?;
        let mut text = String::new();
        for (_, define) in ctx.global.def.clone() {
            text += &define.emit(ctx)?;
        }
        let mut lib = String::from("\nsection .text\n\tglobal main\n");
        for symbol in ctx.global.lib.keys() {
            lib += &format!("\textern {symbol}\n");
        }
        let data = ctx.global.data.clone();
        Ok(format!("section .data\n{data}{lib}{text}\n"))
    }
}

pub const ABI: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl Define {
    fn emit(&self, ctx: &mut Context) -> Result<String, String> {
        let Define::Function((Generics(name, params), args), (body, _)) = self else {
            return Ok(String::new());
        };
        if !params.is_empty() {
            return Ok(String::new());
        }
        ctx.local = ctx.table[name].clone();

        let (mut ptr, mut alloc) = (8, String::new());
        let (mut idx, mut xmm) = (0, 0);
        for (arg, (_, typ)) in args.iter().enumerate() {
            let arg = (arg as isize - 4) * 8;
            if typ == &Type::Float {
                if xmm < 8 {
                    alloc += &format!("\tmovsd [rbp-{ptr}], xmm{xmm}\n")
                } else {
                    alloc += &format!("\tmovsd xmm0, [rbp+{arg}]\n\tmovsd [rbp-{ptr}], xmm0\n")
                };
                xmm += 1;
            } else {
                if let Some(reg) = ABI.get(idx) {
                    alloc += &format!("\tmov [rbp-{ptr}], {reg}\n")
                } else {
                    alloc += &format!("\tmov rax, [rbp+{arg}]\n\tmov [rbp-{ptr}], rax\n")
                }
                idx += 1;
            }
            ptr += 8;
        }
        let body = body.emit(ctx)?;
        ctx.table.insert(name.clone(), ctx.local.clone());

        let var = ctx.local.var.len() * 8;
        let pro = format!(
            "\tpush rbp\n\tmov rbp, rsp\n\tsub rsp, {}\n",
            if var.is_multiple_of(16) { var } else { var + 8 }
        );
        Ok(format!("{name}:\n{pro}{alloc}{body}\tleave\n\tret\n\n"))
    }
}

impl Expr {
    fn emit(&self, ctx: &mut Context) -> Result<String, String> {
        macro_rules! op {
            ($asm: literal, $lhs: expr, $rhs: expr) => {{
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                }
                match typ!(self) {
                    Type::Integer | Type::Boolean => format!(
                        "{}\tpush rax\n{}\tmov r10, rax\n\tpop rax\n\t{} rax, r10\n",
                        $lhs.emit(ctx)?,
                        $rhs.emit(ctx)?,
                        $asm.replace("mul", "imul"),
                    ),
                    Type::Float => format!(
                        "{}\tsub rsp, 8\n\tmovsd [rsp], xmm0\n{}\tmovsd xmm1, xmm0\n\tmovsd xmm0, [rsp]\n\tadd rsp, 8\n\t{}sd xmm0, xmm1\n",
                         $lhs.emit(ctx)?,
                         $rhs.emit(ctx)?,
                         $asm
                    ),
                    _ => panic!(),
                }
            }};
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
                ctx.local.typed[$expr].clone()
            };
        }
        macro_rules! expr {
            ($expr: expr) => {
                ctx.local.expand[$expr].clone()
            };
        }

        match self {
            Expr::If(cond, then, els) => {
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                }
                let id = label!();
                let [cond, then] = [cond.emit(ctx)?, then.emit(ctx)?];
                if let Some(els) = els {
                    Ok(format!(
                        "{cond}\tcmp rax, 0\n\tje else.{id}\n{then}\tjmp if.{id}\nelse.{id}:\n{}if.{id}:\n",
                        els.emit(ctx)?,
                    ))
                } else {
                    Ok(format!(
                        "{cond}\tcmp rax, 0\n\tje if.{id}\n{then}if.{id}:\n"
                    ))
                }
            }
            Expr::While(cond, body) => {
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                }
                let id = label!();
                Ok(format!(
                    "while.{id}:\n{}\tcmp rax, 0\n\tje do.{id}\n{}\tjmp while.{id}\ndo.{id}:\n",
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
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                }
                let env = &ctx.local.var;
                let mut name = name.clone();
                if let Some(addr) = env.get_index_of(&name) {
                    let typ = env[&name].clone();
                    let addr = (addr + 1) * 8;
                    if typ == Type::Float {
                        Ok(format!("\tmovsd xmm0, [rbp-{addr}]\n"))
                    } else {
                        Ok(format!("\tmov rax, [rbp-{addr}]\n"))
                    }
                } else {
                    if !ctx.global.extrn.contains(&name) {
                        name = var.generics();
                    }
                    Ok(format!("\tlea rax, [{name}]\n"))
                }
            }
            Expr::Let(name, val) => match &**name {
                Expr::Variable(Generics(name, _)) => {
                    let env = &ctx.local.var;
                    let typ = env[name].clone();
                    let idx = env.get_index_of(name).unwrap();
                    let (val, addr) = (val.emit(ctx)?, (idx + 1) * 8);
                    if typ == Type::Float {
                        Ok(format!("{val}\tmovsd [rbp-{addr}], xmm0\n"))
                    } else {
                        Ok(format!("{val}\tmov [rbp-{addr}], rax\n"))
                    }
                }
                _ => expr!(self).emit(ctx),
            },
            Expr::Init(_, len) => Ok(format!(
                "{}\tmov qword [rax], {len}\n",
                expr!(self).emit(ctx)?
            )),
            Expr::Check(expr) => {
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                }
                Ok(format!(
                    "{}\tcmp rax, 0\nsetne al\n\tmovzx rax, al\n",
                    expr.emit(ctx)?
                ))
            }
            Expr::Read(offset, typ, addr) => {
                let id = label!();
                let [addr, offset] = [addr.emit(ctx)?, offset.emit(ctx)?];
                Ok(format!(
                    "{addr}\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje null.{id}\n\tpush rax\n{offset}\tpop r11\n\tlea rax, [r11+rax*8]\n{}\tnull.{id}",
                    if *typ == Type::Float {
                        "\tmovsd xmm0, [rax]\n"
                    } else {
                        "\tmov rax, [rax]\n"
                    },
                ))
            }
            Expr::Write(offset, val, addr) => {
                let id = label!();
                let [addr, offset, val] = [addr.emit(ctx)?, offset.emit(ctx)?, val.emit(ctx)?];
                Ok(format!(
                    "{addr}\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje null.{id}\n\tpush rax\n{offset}\tpop r11\n\tlea r11, [r11+rax*8]\n\tpush r11\n{val}\tpop r11\n{}\tnull.{id}",
                    if typ!(self) == Type::Float {
                        "\tmovsd [r11], xmm0\n"
                    } else {
                        "\tmov [r11], rax\n"
                    }
                ))
            }
            Expr::Integer(val) => Ok(format!("\tmov rax, {val}\n")),
            Expr::Float(val) => {
                if *val == Float(0.0) {
                    return Ok("\tpxor xmm0, xmm0\n".to_string());
                }
                let name = format!("float.{}", label!());
                ctx.global.data += &format!("\t{name} dq {val:?}\n");
                Ok(format!("\tmovsd xmm0, [{name}]\n"))
            }
            Expr::String(val) => {
                let val = format!("\"{val}\", 0")
                    .replace("\\t", "\", 9, \"")
                    .replace("\\n", "\", 10, \"")
                    .replace("\\\"", "\", 34, \"")
                    .replace("\"\", ", "");
                let name = format!("str.{}", label!());
                ctx.global.data += &format!("\t{name} db {val}\n");
                Ok(format!("\tmov rax, {name}\n"))
            }
            Expr::Div(lhs, rhs) => {
                if typ!(self) == Type::Float {
                    return Ok(op!("div", lhs, rhs));
                }
                Ok(format!(
                    "{}\tpush rax\n{}\tmov rsi, rax\n\tpop rax\n\tcqo\n\tidiv rsi\n",
                    lhs.emit(ctx)?,
                    rhs.emit(ctx)?,
                ))
            }
            Expr::Mod(_, _) => Ok(format!(
                "{}\tadd rdx, rsi\n\tmov rax, rdx\n\tcqo\n\tidiv rsi\n\tmov rax, rdx\n",
                expr!(self).emit(ctx)?
            )),
            Expr::Add(lhs, rhs) => Ok(op!("add", lhs, rhs)),
            Expr::Sub(lhs, rhs) => Ok(op!("sub", lhs, rhs)),
            Expr::Mul(lhs, rhs) => Ok(op!("mul", lhs, rhs)),
            Expr::Eq(lhs, rhs) => Ok(cmp!("e", lhs, rhs)),
            Expr::Ne(lhs, rhs) => Ok(cmp!("ne", lhs, rhs)),
            Expr::Gt(lhs, rhs) => Ok(cmp!("g", lhs, rhs)),
            Expr::Lt(lhs, rhs) => Ok(cmp!("l", lhs, rhs)),
            Expr::Ge(lhs, rhs) => Ok(cmp!("ge", lhs, rhs)),
            Expr::Le(lhs, rhs) => Ok(cmp!("le", lhs, rhs)),
            Expr::And(lhs, rhs) => Ok(op!("and", lhs, rhs)),
            Expr::Or(lhs, rhs) => Ok(op!("or", lhs, rhs)),
            Expr::Xor(lhs, rhs) => Ok(op!("xor", lhs, rhs)),
            _ => expr!(self).emit(ctx),
        }
    }
}
