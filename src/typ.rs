use crate::*;

impl Define {
    pub fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        match self {
            Define::Function((Generic(name, params), args), (body, ret)) => {
                let sig = Type::Function(Lambda(
                    (params.clone(), Box::new(ret.clone())),
                    Some(args.values().cloned().collect()),
                ));
                ctx.global.lib.insert(name.clone(), sig.clone());
                if params.is_empty() {
                    let parent = ctx.local.clone();
                    ctx.local = Function {
                        scope: args.clone(),
                        ..Function::default()
                    };
                    let body = body.infer(ctx)?;
                    if ret.solve(ctx) != body {
                        return Err(format!("return: {ret} != {body}"));
                    }
                    ctx.table.insert(name.clone(), ctx.local.clone());
                    ctx.local = parent;
                }
                Ok(sig)
            }
            Define::Declare((Generic(name, params), args), ret) => {
                let sig = Type::Function(Lambda(
                    (params.clone(), Box::new(ret.clone())),
                    Some(args.values().cloned().collect()),
                ));
                ctx.global.extrn.insert(name.clone());
                ctx.global.lib.insert(name.clone(), sig.clone());
                Ok(sig)
            }
            Define::Class(Generic(name, args), layout) => {
                let val = (args.clone(), layout.clone());
                ctx.global.table.insert(name.clone(), val);
                Ok(Type::Void)
            }
            Define::Symbol(name, ret) => {
                let sig = Type::Function(Lambda((Vec::new(), Box::new(ret.clone())), None));
                ctx.global.lib.insert(name.clone(), sig.clone());
                ctx.global.extrn.insert(name.clone());
                Ok(sig)
            }
        }
    }
}

impl Expr {
    fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        macro_rules! typing {
            ($typ: expr) => {{
                let typ = $typ.clone();
                ctx.local.typed.insert(self.clone(), typ.clone());
                Ok::<Type, String>(typ)
            }};
        }
        macro_rules! expands {
            ($expr: expr) => {{
                let expr = $expr.clone();
                ctx.local.expand.insert(self.clone(), expr.clone());
                expr.infer(ctx)?
            }};
        }
        macro_rules! expand {
            ($expr: expr) => {{
                let _ = expands!($expr);
            }};
        }
        macro_rules! tmp {
            ($typ: expr) => {{ var!(&format!("tmp{}", hash!(&self))) }};
        }
        macro_rules! op {
            ($typ: pat, $lhs: expr, $rhs: expr) => {{
                match ($lhs.infer(ctx)?, $rhs.infer(ctx)?) {
                    ($typ, ret @ $typ) => typing!(ret.clone()),
                    (lhs, rhs) if lhs != rhs => Err(format!("operator term: {lhs} != {rhs}")),
                    (typ, _) => typing!(expands!(Expr::Call(
                        Box::new(var!(format!("{typ}.{}", self.as_ref()))),
                        vec![*$lhs, *$rhs],
                    ))),
                }
            }};
            ($typ: pat, $lhs: expr, $rhs: expr, $ret: expr) => {{
                op!($typ, $lhs, $rhs)?;
                typing!($ret.clone())
            }};
        }

        if let Some(typ) = ctx.local.typed.get(self) {
            return Ok(typ.clone());
        }
        match self.clone() {
            Expr::Print(is_output, vals) => {
                let mut fmt = String::new();
                for i in vals.iter() {
                    let typ = i.infer(ctx)?;
                    fmt += match typ {
                        Type::Integer => "%ld",
                        Type::String => "%s",
                        Type::Float => "%g",
                        _ => return Err(format!("can't print: {typ}")),
                    }
                }
                is_output.then(|| fmt += "\\n");
                let handler = ["g_strdup_printf", "printf"];
                typing!(expands!(Expr::Call(
                    Box::new(var!(handler[is_output as usize])),
                    [vec![Expr::String(fmt)], vals.to_vec()].concat(),
                )))
            }
            Expr::If(cond, then, els) => {
                if let Expr::Let(bind, check) = *cond {
                    return typing!(expands!(Expr::If(
                        Box::new(Expr::Check(check.clone())),
                        Box::new(Expr::Block(vec![Expr::Let(bind, check), *then])),
                        els,
                    )));
                }
                let cond = cond.infer(ctx)?;
                if Type::Boolean != cond {
                    return Err(format!("if-else test: Bool != {cond}"));
                }
                let lhs = then.infer(ctx)?;
                match els {
                    Some(els) => {
                        let rhs = els.infer(ctx)?;
                        if *els != Expr::Null(Type::Void) && lhs != rhs {
                            return Err(format!("if-else term: {lhs} != {rhs}"));
                        }
                        typing!(lhs)
                    }
                    None => typing!(Type::Void),
                }
            }
            Expr::Match(val, pats) => {
                let typ = val.infer(ctx)?.solve(ctx);
                if let Type::Class(Generic(name, _)) = &typ
                    && let (_, Object::Enum(mut layout)) = ctx.global.table[name].clone()
                {
                    let _ = map!(pats, |(x, _, _)| layout.shift_remove(x));
                    if let Some((lacked, _)) = layout.first() {
                        return Err(format!("not covered: {name}.{lacked}"));
                    }
                } else {
                    return Err(format!("match: Enum != {typ}"));
                };
                let mut expr = Expr::Null(Type::Void);
                for (key, bind, ret) in pats {
                    let acc = Box::new(Expr::Member(val.clone(), key.clone()));
                    expr = Expr::If(
                        Box::new(match bind {
                            Some(bind) => Expr::Let(Box::new(bind.clone()), acc),
                            None => Expr::Check(acc),
                        }),
                        Box::new(ret.clone()),
                        Some(Box::new(expr)),
                    )
                }
                typing!(expands!(expr))
            }
            Expr::While(cond, body) => {
                if let Expr::Let(bind, check) = *cond {
                    return typing!(expands!(Expr::While(
                        Box::new(Expr::Check(check.clone())),
                        Box::new(Expr::Block(vec![Expr::Let(bind, check), *body])),
                    )));
                }
                let cond = cond.infer(ctx)?;
                if Type::Boolean != cond {
                    return Err(format!("while-do test: Bool != {cond}"));
                }
                body.infer(ctx)
            }
            Expr::For(cnt, arr, body) => {
                let typ = arr.infer(ctx)?;
                let Type::Array(_) = typ else {
                    return Err(format!("not iterable: {typ}"));
                };
                let temp = Box::new(tmp!(Type::Integer));
                let inc = Box::new(Expr::Add(temp.clone(), Box::new(Expr::Integer(1))));
                let each = Box::new(Expr::Block(vec![
                    Expr::Let(cnt, Box::new(Expr::Index(arr.clone(), temp.clone()))),
                    Expr::Let(temp.clone(), inc),
                    *body,
                ]));
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(temp.clone(), Box::new(Expr::Integer(0))),
                    Expr::While(Box::new(Expr::Lt(temp.clone(), len!(arr))), each)
                ])))
            }
            Expr::Block(lines) => {
                let mut ret = Type::Void;
                let parent = ctx.local.scope.clone();
                for line in lines {
                    ret = line.infer(ctx)?;
                }
                for (name, val) in &ctx.local.scope {
                    if let Some(typ) = ctx.local.var.get(name)
                        && typ != val
                    {
                        return Err(format!("duplicated {name}: {typ} != {val}"));
                    }
                    ctx.local.var.insert(name.clone(), val.clone());
                }
                ctx.local.scope = parent;
                typing!(ret.clone())
            }
            Expr::Call(callee, args) => {
                if let Some(obj) = args.first()
                    && let Type::Class(Generic(name, _)) = obj.infer(ctx)?
                {
                    ctx.local.class = Some(name);
                }
                match callee.infer(ctx)? {
                    Type::Function(Lambda((_, ret), Some(params))) => {
                        let (pl, al) = (params.len(), args.len());
                        if pl != al {
                            return Err(format!("arguments length: {pl} != {al}"));
                        }
                        for (param, arg) in params.iter().zip(args) {
                            let arg = arg.infer(ctx)?.solve(ctx);
                            if param.solve(ctx) != arg {
                                return Err(format!("argument types: {param} != {arg}"));
                            }
                        }
                        typing!(*ret)
                    }
                    Type::Function(Lambda((_, ret), None)) => {
                        map!(args, |x| x.infer(ctx), ok)?;
                        typing!(*ret)
                    }
                    typ => Err(format!("callee: {typ}")),
                }
            }
            Expr::Variable(Generic(name, args)) => {
                if let Some(obj) = &ctx.local.class {
                    let name = Name::new(&format!("{obj}.{name}"))?;
                    if ctx.global.lib.contains_key(&name) {
                        return typing!(expands!(Expr::Variable(Generic(name, args.clone()))));
                    }
                    ctx.local.class = None;
                }
                if let Some(typ) = ctx.global.lib.get(&name) {
                    typing!(typ.clone().mono(ctx, &Generic(name, args))?)
                } else if let Some(typ) = ctx.local.scope.get(&name) {
                    typing!(typ.clone().solve(ctx))
                } else {
                    Err(format!("undefined: {name}"))
                }
            }
            Expr::Let(name, val) => match &*name {
                Expr::Variable(Generic(name, _)) => {
                    let val = val.infer(ctx)?;
                    if let Some(typ) = ctx.local.scope.get(name) {
                        let typ = typ.clone().solve(ctx);
                        if val != typ {
                            return Err(format!("{name}: {typ} != {val}"));
                        }
                    } else {
                        ctx.local.scope.insert(name.clone(), val.clone());
                    }
                    typing!(Type::Void)
                }
                acc @ Expr::Index(arr, idx) => {
                    expand!(Expr::Write(array!(arr, idx), val.clone(), arr.clone()));
                    let [val, typ] = [val.infer(ctx)?, acc.infer(ctx)?];
                    if typ.clone() != val {
                        return Err(format!("array: {typ} != {val}"));
                    }
                    typing!(Type::Void)
                }
                acc @ Expr::Member(obj, key) => {
                    let typ = acc.infer(ctx)?;
                    let Generic(name, _) = &obj.infer(ctx)?.unwrap_class();
                    let rhs = val.infer(ctx)?;
                    if typ != rhs {
                        return Err(format!("{name}.{key}: {typ} != {rhs}"));
                    }
                    match &ctx.global.table[name] {
                        (_, Object::Struct(layout)) => {
                            let offset = layout.get_index_of(key).unwrap();
                            let offset = Box::new(Expr::Integer(offset as i64));
                            expand!(Expr::Write(offset, val.clone(), obj.clone()));
                        }
                        (_, Object::Enum(layout)) => {
                            let tag = layout.get_index_of(key).unwrap();
                            let offset = |x| Box::new(Expr::Integer(x));
                            expand!(Expr::Block(vec![
                                Expr::Write(offset(0), offset(tag as i64), obj.clone()),
                                Expr::Write(offset(8), val.clone(), obj.clone()),
                            ]));
                        }
                    }
                    typing!(Type::Void)
                }
                other => Err(format!("assign target: {}", other.infer(ctx)?)),
            },
            Expr::Sequence(array) => {
                let typ = array[0].infer(ctx)?;
                let temp = tmp!(typ.clone());
                let mut expr = vec![Expr::Let(
                    Box::new(temp.clone()),
                    Box::new(Expr::Init(typ, array.len())),
                )];
                for (idx, val) in array.iter().enumerate() {
                    expr.push(Expr::Let(
                        Box::new(Expr::Index(
                            Box::new(temp.clone()),
                            Box::new(Expr::Integer(idx as i64)),
                        )),
                        Box::new(val.clone()),
                    ));
                }
                expr.push(temp);
                typing!(expands!(Expr::Block(expr)))
            }
            Expr::Index(arr, idx) => {
                let typ = arr.infer(ctx)?;
                let Type::Array(typ) = typ else {
                    return Err(format!("array: {typ}"));
                };
                match idx.infer(ctx)? {
                    Type::Integer => {
                        expand!(Expr::Read(array!(arr, idx), *typ.clone(), arr.clone()));
                        typing!(*typ.clone())
                    }
                    typ => Err(format!("index: {typ}")),
                }
            }
            Expr::Len(obj) => typing!(expands!(match obj.infer(ctx)? {
                Type::String => Expr::Call(Box::new(var!("strlen")), vec![*obj.clone()]),
                Type::Array(_) => Expr::Read(Box::new(Expr::Integer(0)), Type::Integer, obj),
                typ => return Err(format!("no length: {typ}")),
            })),
            Expr::New(typ) => {
                let Type::Class(_) = typ.clone() else {
                    return Err(format!("no constructor: {typ}"));
                };
                let typ = typ.mono(ctx, &Generic::default())?;
                expand!(new!(typ.size(ctx)? / 8));
                typing!(typ.solve(ctx))
            }
            Expr::Enum(typ, key, val) => {
                let temp = Box::new(tmp!(typ.clone()));
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(temp.clone(), Box::new(Expr::New(typ.clone()))),
                    Expr::Let(
                        Box::new(Expr::Member(temp.clone(), key.clone())),
                        val.clone(),
                    ),
                    *temp
                ])))
            }
            Expr::Member(obj, key) => {
                let typ = obj.infer(ctx)?;
                let Type::Class(name) = &typ else {
                    if "len" == key.to_string() {
                        return typing!(expands!(Expr::Len(obj)));
                    }
                    return Err(format!("not class: {typ}"));
                };
                let Some((_, class)) = ctx.global.table.get(&name.generics()) else {
                    return Err(format!("undefined: {name}"));
                };
                let (Object::Struct(layout) | Object::Enum(layout)) = class;
                let Some(typ) = layout.get(&key).cloned() else {
                    return Err(format!("undefined: {name}.{key}"));
                };
                match class {
                    Object::Struct(layout) => {
                        let offset = Expr::Integer(layout.get_index_of(&key).unwrap() as i64);
                        expand!(Expr::Read(Box::new(offset), typ.clone(), obj.clone()));
                    }
                    Object::Enum(_) => {
                        let offset = Box::new(Expr::Integer(8));
                        expand!(Expr::If(
                            Box::new(Expr::Check(Box::new(self.clone()))),
                            Box::new(Expr::Read(offset, typ.clone(), obj.clone())),
                            Some(Box::new(Expr::Null(typ.clone())))
                        ));
                    }
                }
                typing!(typ.solve(ctx))
            }
            Expr::Check(expr) => {
                if let Expr::Member(obj, key) = &*expr
                    && let Type::Class(Generic(name, _)) = &obj.infer(ctx)?
                    && let Some((_, Object::Enum(layout))) = ctx.global.table.get(name)
                {
                    let Some(tag) = layout.get_index_of(key) else {
                        return Err(format!("undefined: {name}.{key}"));
                    };
                    let offset = Box::new(Expr::Integer(0));
                    expand!(Expr::Eq(
                        Box::new(Expr::Read(offset, Type::Integer, obj.clone())),
                        Box::new(Expr::Integer(tag as i64)),
                    ));
                    return typing!(Type::Boolean);
                }
                let typ = expr.infer(ctx)?;
                let Type::Class(_) = typ else {
                    return Err(format!("can't null-check: {typ}"));
                };
                typing!(Type::Boolean)
            }
            Expr::Init(typ, len) => {
                expand!(new!(len + 1));
                typing!(Type::Array(Box::new(typ.clone())))
            }
            Expr::Read(addr, typ, offset) => {
                let offset = offset.infer(ctx)?;
                if let Type::Integer = offset {
                    return Err(format!("not address: {offset}"));
                }
                addr.infer(ctx)?;
                typing!(typ.clone())
            }
            Expr::Write(addr, val, offset) => {
                let offset = offset.infer(ctx)?;
                if let Type::Integer = offset {
                    return Err(format!("not address: {offset}"));
                }
                addr.infer(ctx)?;
                typing!(val.infer(ctx)?)
            }
            Expr::Clone(expr) => {
                let typ = expr.infer(ctx)?;
                let dest = Box::new(tmp!(typ));
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(dest.clone(), Box::new(Expr::New(typ.clone()))),
                    Expr::Call(
                        Box::new(var!("memcpy", typ.clone())),
                        vec![*dest.clone(), *expr, Expr::Integer(typ.size(ctx)? as i64)]
                    ),
                    *dest.clone()
                ])))
            }
            Expr::Mod(lhs, rhs) => {
                expand!(Expr::Div(lhs.clone(), rhs.clone()));
                op!(Type::Integer, lhs, rhs)
            }
            Expr::Null(typ) => {
                expand!(Expr::Block(vec![
                    Expr::Float(Float::from(0.0)),
                    Expr::Integer(0)
                ]));
                typing!(typ.solve(ctx))
            }
            Expr::Integer(_) => typing!(Type::Integer),
            Expr::Float(_) => typing!(Type::Float),
            Expr::String(_) => typing!(Type::String),
            Expr::Boolean(val) => {
                expand!(Expr::Integer(if val { 1 } else { 0 }));
                typing!(Type::Boolean)
            }
            Expr::Add(lhs, rhs)
            | Expr::Sub(lhs, rhs)
            | Expr::Mul(lhs, rhs)
            | Expr::Div(lhs, rhs) => op!(Type::Integer | Type::Float, lhs, rhs),
            Expr::Eq(lhs, rhs)
            | Expr::Ne(lhs, rhs)
            | Expr::Gt(lhs, rhs)
            | Expr::Lt(lhs, rhs)
            | Expr::Ge(lhs, rhs)
            | Expr::Le(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Boolean),
            Expr::And(lhs, rhs) | Expr::Or(lhs, rhs) | Expr::Xor(lhs, rhs) => {
                op!(Type::Boolean, lhs, rhs)
            }
        }
    }
}

impl Type {
    fn mono(self, ctx: &mut Context, func @ Generic(name, args): &Generic) -> Result<Type, String> {
        let mut typ = self.solve(ctx);
        let args = map!(args, |x| x.solve(ctx));
        match typ.clone() {
            Type::Function(Lambda((params, _), _)) if !params.is_empty() => {
                let mut alias = IndexMap::new();
                for (arg, param) in args.iter().zip(&params) {
                    alias.insert(param.clone(), arg.clone());
                    typ = typ.rewrite(param, arg);
                }
                let (mangle, mut unify) = (func.generics(), ctx.global.def[name].clone());
                if let Define::Function((_, params), _) | Define::Declare((_, params), _) = &unify
                    && let Type::Function(Lambda((_, ret), Some(args))) = typ.clone()
                {
                    let head = (
                        Generic(mangle.clone(), Vec::new()),
                        params.keys().cloned().zip(args).collect(),
                    );
                    unify = match unify {
                        Define::Function(_, (body, _)) => Define::Function(head, (body, *ret)),
                        _ => Define::Declare(head, *ret),
                    };
                };
                let parent = ctx.global.alias.clone();
                ctx.global.alias = alias.clone();
                {
                    typ = unify.infer(ctx)?;
                }
                ctx.global.def.insert(mangle, unify.clone());
                ctx.global.alias = parent;
            }
            Type::Class(Generic(name, args)) => {
                let Some((params, table)) = ctx.global.table.get(&name) else {
                    return Err(format!("undefined: {name}"));
                };
                let layout = {
                    let (Object::Enum(layout) | Object::Struct(layout)) = &table;
                    let mut layout = layout.clone();
                    for (_, field) in layout.iter_mut() {
                        for (arg, param) in args.iter().zip(params) {
                            *field = field.rewrite(param, arg);
                        }
                    }
                    layout
                };
                let unify = match table {
                    Object::Enum(_) => Object::Enum(layout).clone(),
                    Object::Struct(_) => Object::Struct(layout).clone(),
                };
                let mangle = Generic(name.clone(), args).generics();
                ctx.global.table.insert(mangle.clone(), (vec![], unify));
            }
            _ => {}
        }
        Ok(typ.solve(ctx))
    }

    fn rewrite(&self, old: &Type, new: &Type) -> Type {
        if self == old {
            return new.clone();
        }
        match self {
            Type::Function(Lambda((typ, ret), Some(args))) => {
                let args = Some(map!(args, |x| x.rewrite(old, new)));
                Type::Function(Lambda((typ.clone(), Box::new(ret.rewrite(old, new))), args))
            }
            Type::Class(Generic(name, args)) => {
                Type::Class(Generic(name.clone(), map!(args, |x| x.rewrite(old, new))))
            }
            Type::Array(typ) => Type::Array(Box::new(typ.rewrite(old, new))),
            _ => self.clone(),
        }
    }

    fn solve(&self, ctx: &mut Context) -> Type {
        let mut typ = self.clone();
        for (old, new) in &ctx.global.alias {
            typ = typ.rewrite(old, new);
        }
        typ
    }

    fn size(&self, ctx: &Context) -> Result<usize, String> {
        match self {
            Type::Class(Generic(name, _)) => match &ctx.global.table[name] {
                (_, Object::Struct(layout)) => Ok(layout.len() * 8),
                (_, Object::Enum(_)) => Ok(16),
            },
            _ => Err(format!("can't clone: {self}")),
        }
    }
}
