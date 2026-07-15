use crate::*;

impl Define {
    pub fn compile(defines: &[Self]) -> Result<String, String> {
        macro_rules! name {
            ($define: expr) => {
                match $define.clone() {
                    Define::Function((Generic(func, _), _), _) => func,
                    Define::Declare((Generic(lib, _), _), _) => lib,
                    Define::Class(Generic(class, _), _) => class,
                    Define::Symbol(sym, _) => sym,
                }
                .clone()
            };
        }
        let ctx = &mut Context::default();
        ctx.global.def = defines.iter().map(|x| (name!(x), x.clone())).collect();
        map!({ defines }, |define| define.infer(ctx))?;

        let mut text = String::from("\n");
        for (_, define) in ctx.global.def.clone() {
            text += &define.emit(ctx)?;
        }
        let mut lib = String::from("\nsection .text\n\tglobal main\n");
        for symbol in ctx.global.extrn.clone() {
            lib += &format!("\textern {symbol}\n");
        }
        let data = ctx.global.data.clone();
        Ok(format!("section .data\n{data}{lib}{text}\n"))
    }
}
pub const ABI: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl Define {
    fn emit(&self, ctx: &mut Context) -> Result<String, String> {
        let Define::Function((Generic(name, params), args), (body, _)) = self else {
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
            ($asm: literal, $lhs: expr, $rhs: expr) => {
                match typ!(&**$rhs) {
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
                    _ => expr!(self).emit(ctx)?,
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
            ($expr: expr) => {{ ctx.local.typed[$expr].clone() }};
        }
        macro_rules! expr {
            ($expr: expr) => {{ ctx.local.expand[$expr].clone() }};
        }
        match self {
            Expr::If(cond, then, els) => {
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                };
                let [id, then] = [label!(), then.emit(ctx)?];
                let cond = format!("{}\tcmp rax, 0\n", cond.emit(ctx)?);
                if let Some(els) = els {
                    return Ok(format!(
                        "{cond}\tje .Lelse{id}\n{then}\tjmp .Lif{id}\n.Lelse{id}:\n{}.Lif{id}:\n",
                        els.emit(ctx)?,
                    ));
                }
                Ok(format!("{cond}\tje .Lif{id}\n{then}.Lif{id}:\n"))
            }
            Expr::While(cond, body) => {
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                };
                let id = label!();
                Ok(format!(
                    ".Lwhile{id}:\n{}\tcmp rax, 0\n\tje .Ldo{id}\n{}\tjmp .Lwhile{id}\n.Ldo{id}:\n",
                    cond.emit(ctx)?,
                    body.emit(ctx)?,
                ))
            }
            Expr::Block(lines) => Ok(map!({ lines }, |line| line.emit(ctx))?.concat()),
            Expr::Call(callee, args) => {
                let mut push = String::new();
                for arg in args.iter().rev() {
                    push += &arg.emit(ctx)?;
                    match typ!(arg) {
                        Type::Float => push += "\tsub rsp, 8\n\tmovsd [rsp], xmm0\n",
                        _ => push += "\tpush rax\n",
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
            Expr::Variable(var @ Generic(name, _)) => {
                if let Some(expr) = ctx.local.expand.get(self) {
                    return expr.clone().emit(ctx);
                };
                let env = &ctx.local.var;
                let mut name = name.clone();
                if let Some(addr) = env.get_index_of(&name) {
                    let (typ, addr) = (env[&name].clone(), (addr + 1) * 8);
                    match typ {
                        Type::Float => Ok(format!("\tmovsd xmm0, [rbp-{addr}]\n")),
                        _ => Ok(format!("\tmov rax, [rbp-{addr}]\n")),
                    }
                } else {
                    if !ctx.global.extrn.contains(&name) {
                        name = var.generic();
                    }
                    Ok(format!("\tlea rax, [{name}]\n"))
                }
            }
            Expr::Let(name, val) => match &**name {
                Expr::Variable(Generic(name, _)) => {
                    let env = &ctx.local.var;
                    let (typ, idx) = (env[name].clone(), env.get_index_of(name).unwrap());
                    let (val, addr) = (val.emit(ctx)?, (idx + 1) * 8);
                    match typ {
                        Type::Float => Ok(format!("{val}\tmovsd [rbp-{addr}], xmm0\n")),
                        _ => Ok(format!("{val}\tmov [rbp-{addr}], rax\n")),
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
                };
                Ok(format!(
                    "{}\tcmp rax, 0\nsetne al\n\tmovzx rax, al\n",
                    expr.emit(ctx)?
                ))
            }
            Expr::Read(offset, typ, addr) => {
                let id = label!();
                let [addr, offset] = [addr.emit(ctx)?, offset.emit(ctx)?];
                Ok(format!(
                    "{addr}\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje .Lnull{id}\n\tpush rax\n{offset}\tpop r10\n\tlea rax, [r10+rax*8]\n{}.Lnull{id}:\n",
                    match typ {
                        Type::Float => "\tmovsd xmm0, [rax]\n",
                        _ => "\tmov rax, [rax]\n",
                    }
                ))
            }
            Expr::Write(offset, val, addr) => {
                let id = label!();
                let [addr, offset, val] = [addr.emit(ctx)?, offset.emit(ctx)?, val.emit(ctx)?];
                Ok(format!(
                    "{addr}\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje .Lnull{id}\n\tpush rax\n{offset}\tpop r10\n\tlea r10, [r10+rax*8]\n\tpush r10\n{val}\tpop r10\n{}.Lnull{id}:\n",
                    match typ!(self) {
                        Type::Float => "\tmovsd [r10], xmm0\n",
                        _ => "\tmov [r10], rax\n",
                    }
                ))
            }
            Expr::Integer(val) if *val == 0 => Ok(String::from("\txor rax, rax\n")),
            Expr::Integer(val) => Ok(format!("\tmov rax, {val}\n")),
            Expr::Float(val) if *val == Float(0.0) => Ok(String::from("\tpxor xmm0, xmm0\n")),
            Expr::Float(val) => {
                let name = format!("float.{}", label!());
                ctx.global.data += &format!("\t{name} dq {val:?}\n");
                Ok(format!("\tmovsd xmm0, [{name}]\n"))
            }
            Expr::String(val) => {
                let val = format!("\"{val}\", 0")
                    .replace("\\t", "\", 9, \"")
                    .replace("\\n", "\", 10, \"")
                    .replace("\\r", "\", 13, \"")
                    .replace("\\\"", "\", 34, \"")
                    .replace("\"\", ", "");
                let name = format!("str.{}", label!());
                ctx.global.data += &format!("\t{name} db {val}\n");
                Ok(format!("\tmov rax, {name}\n"))
            }
            Expr::Div(lhs, rhs) if typ!(self) == Type::Float => Ok(op!("div", lhs, rhs)),
            Expr::Div(lhs, rhs) => Ok(format!(
                "{}\tpush rax\n{}\tmov rsi, rax\n\tpop rax\n\tcqo\n\tidiv rsi\n",
                lhs.emit(ctx)?,
                rhs.emit(ctx)?,
            )),
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
impl Generic {
    pub fn generic(&self) -> Name {
        let Generic(mut name, typ) = self.clone();
        for typ in typ {
            name = name.class(typ);
        }
        name
    }
}

#[macro_export]
macro_rules! new {
    ($layout: expr) => {
        Expr::Call(
            Box::new(var!("calloc")),
            vec![Expr::Integer($layout as i64), Expr::Integer(8)],
        )
    };
}
#[macro_export]
macro_rules! len {
    ($arr: expr) => {
        Box::new(Expr::Member($arr.clone(), Name::new("len")?))
    };
}
#[macro_export]
macro_rules! array {
    ($arr: expr, $idx: expr) => {
        Box::new(Expr::Add(
            Box::new(Expr::Mod($idx.clone(), len!($arr))),
            Box::new(Expr::Integer(1)),
        ))
    };
}

#[macro_export]
macro_rules! hash {
    ($val: expr) => {{
        use std::hash::{DefaultHasher, Hasher};
        let mut state = DefaultHasher::new();
        $val.hash(&mut state);
        state.finish()
    }};
}
#[macro_export]
macro_rules! map {
    ($arr: block, $lambda: expr) => {{ $arr.iter().map($lambda).collect::<Result<Vec<_>, String>>() }};
    ($arr: expr, $lambda: expr) => {{ $arr.iter().map($lambda).collect::<Vec<_>>() }};
}
#[macro_export]
macro_rules! var {
    ($name: expr) => {{ Expr::Variable(Generic(Name::new(&$name)?, Vec::new())) }};
    ($name: expr, $typ: block) => {{ Expr::Variable(Generic(Name::new($name)?.class($typ), Vec::new())) }};
    ($name: expr, $typ: expr) => {{ Expr::Variable(Generic(Name::new(&$name)?, vec![$typ])) }};
}
