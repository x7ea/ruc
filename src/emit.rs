use crate::*;

impl Define {
    pub fn compile(mut program: &mut [Define]) -> Result<String, String> {
        macro_rules! name {
            ($define: expr) => {
                match $define.clone() {
                    Define::Function((Generic(func, _), _), _) => func,
                    Define::Declare((Generic(lib, _), _), _) => lib,
                    Define::Class(Generic(class, _), _) => class,
                    Define::Symbol(sym, _) => sym,
                }
            };
        }
        let ctx = &mut Context::default();
        ctx.global.used.insert(Name::new("main")?);
        ctx.global.def = program.iter().map(|x| (name!(x), x.clone())).collect();

        let _typ = map!({ &mut program }, |define| define.infer(ctx))?;
        let asm = map!(ctx.global.def.clone(), |(_, func)| func.emit(ctx)).concat();
        let lib = map!(&ctx.global.extrn, |lib| format!("\textern {lib}\n")).concat();

        Ok(format!(
            "section .data\n{}\nsection .text\n\tglobal main\n{lib}\n{asm}",
            ctx.global.data.clone()
        ))
    }
}

pub const ABI: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl Define {
    fn emit(&self, ctx: &mut Context) -> String {
        let Define::Function((Generic(name, params), args), (body, _)) = self else {
            return String::default();
        };
        if !params.is_empty() || !ctx.global.used.contains(name) {
            return String::default();
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
        let body = body.emit(ctx);
        ctx.table.insert(name.clone(), ctx.local.clone());

        let size = ctx.local.var.len() * 8;
        let frame = format!(
            "\tpush rbp\n\tmov rbp, rsp\n\tsub rsp, {}\n",
            if size % 16 == 0 { size } else { size + 8 }
        );
        format!("{name}:\n{frame}{alloc}{body}\tleave\n\tret\n\n")
    }
}

impl Expr {
    fn emit(&self, ctx: &mut Context) -> String {
        macro_rules! emit {
            ($ast: expr) => {{ $ast.clone().emit(ctx) }};
        }
        macro_rules! expanding_optimized_code {
            () => {
                if let Some(expr) = ctx.local.expand.get(self) {
                    return emit!(expr);
                };
            };
        }
        macro_rules! op {
            ($asm: literal, $lhs: expr, $rhs: expr) => {
                match typ!(&**$rhs) {
                    Type::Integer | Type::Boolean => format!(
                        "{}\tpush rax\n{}\tmov r10, rax\n\tpop rax\n\t{} rax, r10\n",
                        emit!($lhs),
                        emit!($rhs),
                        $asm.replace("mul", "imul"),
                    ),
                    Type::Float => format!(
                        "{}\tsub rsp, 8\n\tmovsd [rsp], xmm0\n{}\tmovsd xmm1, xmm0\n\tmovsd xmm0, [rsp]\n\tadd rsp, 8\n\t{}sd xmm0, xmm1\n",
                         emit!($lhs),
                         emit!($rhs),
                         $asm
                    ),
                    _ => expr!(self).emit(ctx),
                }
            };
        }
        macro_rules! cmp {
            ($op: literal, $lhs: expr , $rhs: expr) => {{
                let [test, op] = [op!("cmp", $lhs, $rhs), $op.to_string()];
                format!("{test}\tset{op} al\n\tmovzx rax, al\n",)
            }};
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
                expanding_optimized_code!();
                let [l1, then] = [label!(), emit!(then)];
                let cond = format!("{}\tcmp rax, 0\n", emit!(cond));
                match els {
                    None => format!("{cond}\tje .L{l1}\n{then}.L{l1}:\n"),
                    Some(case) => {
                        let (l2, case) = (label!(), emit!(case));
                        format!("{cond}\tje .L{l2}\n{then}\tjmp .L{l1}\n.L{l2}:\n{case}.L{l1}:\n")
                    }
                }
            }
            Expr::While(test, body) => {
                expanding_optimized_code!();
                let [top, lp, test, body] = [label!(), label!(), emit!(test), emit!(body)];
                format!(".L{top}:\n{test}\tcmp rax, 0\n\tje .L{lp}\n{body}\tjmp .L{top}\n.L{lp}:\n",)
            }
            Expr::Block(lines) => map!(&lines, |code| code.emit(ctx)).concat(),
            Expr::Return(val) => format!("{}\tleave\n\tret\n", val.emit(ctx)),
            Expr::Call(func, args) => {
                let mut push = String::new();
                for arg in args.iter().rev() {
                    push += &emit!(arg);
                    push += match typ!(arg) {
                        Type::Float => "\tsub rsp, 8\n\tmovsd [rsp], xmm0\n",
                        _ => "\tpush rax\n",
                    };
                }
                let mut mov = String::new();
                let (mut idx, mut xmm) = (0, 0);
                for arg in args.iter() {
                    if typ!(arg) == Type::Float {
                        if xmm < 8 {
                            mov += &format!("\tmovsd xmm{xmm}, [rsp]\n\tadd rsp, 8\n")
                        }
                        xmm += 1;
                    } else {
                        if let Some(reg) = ABI.get(idx) {
                            mov += &format!("\tpop {reg}\n");
                        }
                        idx += 1;
                    }
                }
                let (func, len) = (emit!(func), emit!(Expr::Integer(xmm)));
                format!("{push}{mov}{len}\tmov r10, rax\n{func}\tcall r10\n")
            }
            Expr::Variable(var @ Generic(name, _)) => {
                expanding_optimized_code!();
                let env = &ctx.local.var;
                let mut name = name.clone();
                if let Some(addr) = env.get_index_of(&name) {
                    let (typ, addr) = (env[&name].clone(), (addr + 1) * 8);
                    match typ {
                        Type::Float => format!("\tmovsd xmm0, [rbp-{addr}]\n"),
                        _ => format!("\tmov rax, [rbp-{addr}]\n"),
                    }
                } else {
                    if !ctx.global.extrn.contains(&name) {
                        name = var.generic();
                    }
                    format!("\tlea rax, [{name}]\n")
                }
            }
            Expr::Let(name, val) => match &**name {
                Expr::Variable(Generic(name, _)) => {
                    let env = ctx.local.var.clone();
                    let (typ, idx) = (&env[name], env.get_index_of(name).unwrap());
                    let (val, addr) = (emit!(val), (idx + 1) * 8);
                    match typ {
                        Type::Float => format!("{val}\tmovsd [rbp-{addr}], xmm0\n"),
                        _ => format!("{val}\tmov [rbp-{addr}], rax\n"),
                    }
                }
                _ => expr!(self).emit(ctx),
            },
            Expr::Init(_, len) => {
                let (len, new) = (emit!(len), emit!(expr!(self)));
                format!("{new}\tpush rax\n{len}\tmov r10, rax\n\tpop rax\n\tmov qword [rax], r10\n")
            }
            Expr::Check(obj) => {
                expanding_optimized_code!();
                format!("{}\tcmp rax, 0\n\tsetne al\n\tmovzx rax, al\n", emit!(obj))
            }
            Expr::Read(offset, typ, addr) => {
                let [l1, addr, offset] = [label!(), emit!(addr), emit!(offset)];
                format!(
                    "{addr}\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje .L{l1}\n\tpush rax\n{offset}\tpop r10\n\tlea rax, [r10+rax*8]\n{}.L{l1}:\n",
                    match typ {
                        Type::Float => "\tmovsd xmm0, [rax]\n",
                        _ => "\tmov rax, [rax]\n",
                    }
                )
            }
            Expr::Write(offset, val, addr) => {
                let [id, addr, offset, val] = [label!(), emit!(addr), emit!(offset), emit!(val)];
                format!(
                    "{addr}\tpxor xmm0, xmm0\n\tcmp rax, 0\n\tje .L{id}\n\tpush rax\n{offset}\tpop r10\n\tlea r10, [r10+rax*8]\n\tpush r10\n{val}\tpop r10\n{}.L{id}:\n",
                    match typ!(self) {
                        Type::Float => "\tmovsd [r10], xmm0\n",
                        _ => "\tmov [r10], rax\n",
                    }
                )
            }
            Expr::Integer(0) => String::from("\txor rax, rax\n"),
            Expr::Integer(val) => format!("\tmov rax, {val}\n"),
            Expr::Float(Float(0.0)) => String::from("\tpxor xmm0, xmm0\n"),
            Expr::Float(val) => {
                let name = format!("float{}", label!());
                ctx.global.data += &format!("\t{name} dq {val:?}\n");
                format!("\tmovsd xmm0, [{name}]\n")
            }
            Expr::String(val) => {
                let val = format!("\"{val}\", 0")
                    .replace("\\t", "\", 9, \"")
                    .replace("\\n", "\", 10, \"")
                    .replace("\\r", "\", 13, \"")
                    .replace("\\\"", "\", 34, \"")
                    .replace("\"\", ", "");
                let name = format!("str{}", label!());
                ctx.global.data += &format!("\t{name} db {val}\n");
                format!("\tmov rax, {name}\n")
            }
            Expr::Div(lhs, rhs) if typ!(self) == Type::Float => op!("div", lhs, rhs),
            Expr::Div(lhs, rhs) => format!(
                "{}\tpush rax\n{}\tmov rsi, rax\n\tpop rax\n\tcqo\n\tidiv rsi\n",
                lhs.emit(ctx),
                rhs.emit(ctx),
            ),
            Expr::Mod(_, _) => format!(
                "{}\tadd rdx, rsi\n\tmov rax, rdx\n\tcqo\n\tidiv rsi\n\tmov rax, rdx\n",
                expr!(self).emit(ctx)
            ),
            Expr::Not(term) => format!(
                "{}\tcmp rax, 0\n\tsete al\n\tmovzx rax, al\n",
                term.emit(ctx)
            ),
            Expr::Add(lhs, rhs) => op!("add", lhs, rhs),
            Expr::Sub(lhs, rhs) => op!("sub", lhs, rhs),
            Expr::Mul(lhs, rhs) => op!("mul", lhs, rhs),
            Expr::Eq(lhs, rhs) => cmp!("e", lhs, rhs),
            Expr::Ne(lhs, rhs) => cmp!("ne", lhs, rhs),
            Expr::Gt(lhs, rhs) => cmp!("g", lhs, rhs),
            Expr::Lt(lhs, rhs) => cmp!("l", lhs, rhs),
            Expr::Ge(lhs, rhs) => cmp!("ge", lhs, rhs),
            Expr::Le(lhs, rhs) => cmp!("le", lhs, rhs),
            Expr::And(lhs, rhs) => op!("and", lhs, rhs),
            Expr::Or(lhs, rhs) => op!("or", lhs, rhs),
            Expr::Xor(lhs, rhs) => op!("xor", lhs, rhs),
            _ => expr!(self).emit(ctx),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (_, Type::Any) => true,
            (Type::Integer, Type::Integer)
            | (Type::Float, Type::Float)
            | (Type::String, Type::String)
            | (Type::Boolean, Type::Boolean)
            | (Type::Void, Type::Void) => true,
            (Type::Array(lhs), Type::Array(rhs)) => lhs == rhs,
            (Type::Class(lhs), Type::Class(rhs)) => lhs == rhs,
            (Type::Function(lhs), Type::Function(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Generic {
    pub fn generic(&self) -> Name {
        let Generic(mut name, args) = self.clone();
        for typ in args {
            name = name.generic(&typ);
        }
        name
    }
}

#[macro_export]
macro_rules! new {
    ($layout: expr) => {{ Expr::Call(Box::new(var!("calloc")), vec![$layout, Expr::Integer(8)]) }};
}

#[macro_export]
macro_rules! len {
    ($arr: expr) => {{ Box::new(Expr::Member($arr.clone(), Name::new("len")?)) }};
}

#[macro_export]
macro_rules! array {
    ($arr: expr, $idx: expr) => {{
        let idx = Box::new(Expr::Mod($idx.clone(), len!($arr)));
        Box::new(Expr::Add(idx, Box::new(Expr::Integer(1))))
    }};
}

#[macro_export]
macro_rules! map {
    ($a: block, $f: expr) => {{ $a.iter_mut().map($f).collect::<Result<Vec<_>, String>>() }};
    ($a: expr, $f: expr) => {{ $a.iter().map($f).collect::<Vec<_>>() }};
}

#[macro_export]
macro_rules! var {
    ($name: expr) => {{ Expr::Variable(Generic(Name($name.to_string()), Vec::new())) }};
    ($name: expr, $arg: expr) => {{ Expr::Variable(Generic(Name($name.to_string()), vec![$arg])) }};
}

#[macro_export]
macro_rules! method {
    ($typ: expr, $name: expr) => {{
        let name = Name::new($name)?.class(&$typ.remove_generic());
        Expr::Variable(Generic(name, $typ.generic_args()))
    }};
}
