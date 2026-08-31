use crate::*;

impl Define {
    pub fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        match self {
            Define::Function((Generic(name, params), args), (body, ret)) => {
                ctx.global.lib.insert(name.clone(), self.signature());
                if params.is_empty() {
                    let parent = ctx.local.clone();
                    ctx.local = Function {
                        scope: args.clone(),
                        ..Function::default()
                    };
                    ctx.local.ret = ret.solve(ctx);
                    let body = body.infer(ctx)?;
                    if ctx.local.ret != body {
                        return Err(format!("return: {ret} != {body}"));
                    }
                    ctx.table.insert(name.clone(), ctx.local.clone());
                    ctx.local = parent;
                }
            }
            Define::Declare((Generic(name, _), _), _) => {
                ctx.global.extrn.insert(name.clone());
                ctx.global.lib.insert(name.clone(), self.signature());
            }
            Define::Class(Generic(name, args), layout) => {
                let obj = (args.clone(), layout.clone());
                ctx.global.table.insert(name.clone(), obj);
            }
            Define::Symbol(name, _) => {
                ctx.global.lib.insert(name.clone(), self.signature());
                ctx.global.extrn.insert(name.clone());
            }
        }
        Ok(self.signature())
    }

    fn signature(&self) -> Type {
        match self {
            Define::Function((Generic(_, params), args), (_, ret))
            | Define::Declare((Generic(_, params), args), ret) => Type::Function(Lambda(
                (params.clone(), Box::new(ret.clone())),
                Some(args.values().cloned().collect()),
            )),
            Define::Symbol(_, ret) => Type::Function(Lambda((vec![], Box::new(ret.clone())), None)),
            _ => Type::Void,
        }
    }
}

impl Expr {
    fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        macro_rules! typing {
            ($ret: expr) => {{
                let typ = $ret.clone();
                ctx.local.typed.insert(self.clone(), typ.clone());
                Ok::<Type, String>(typ)
            }};
        }
        macro_rules! expands {
            ($value: expr) => {{
                let expr = $value.clone();
                ctx.local.expand.insert(self.clone(), expr.clone());
                expr.infer(ctx)?
            }};
        }
        macro_rules! expand {
            ($expr: expr) => {
                let _ = expands!($expr);
            };
        }
        macro_rules! temp {
            () => {{ var!(&format!("temp{}", hash!(&self))) }};
        }
        macro_rules! op {
            ($typ: pat, $lhs: expr, $rhs: expr) => {{
                let op = &self.as_ref().to_lowercase();
                match ($lhs.infer(ctx)?, $rhs.infer(ctx)?) {
                    ($typ, ret @ $typ) => typing!(ret.clone()),
                    (lhs, rhs) if lhs != rhs => Err(format!("{op} term: {lhs} != {rhs}",)),
                    (typ, _) => typing!(expands!(Expr::Call(
                        Box::new(method!(&typ, op)),
                        vec![*$lhs, *$rhs],
                    ))),
                }
            }};
            ($typ: pat, $lhs: expr, $rhs: expr, $ret: expr) => {{ op!($typ, $lhs, $rhs).and_then(|_| typing!($ret.clone())) }};
        }

        match self.clone() {
            Expr::Print(is_output, mut vals) => {
                let mut fmt = String::new();
                for val in vals.iter_mut() {
                    fmt += &val.fmtgen(ctx)?
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
                let typ = cond.infer(ctx)?;
                if Type::Boolean != typ {
                    return Err(format!("if-else test: Bool != {typ}"));
                }
                match (then.infer(ctx)?, els) {
                    (lhs, Some(rhs)) => match (lhs, rhs.infer(ctx)?) {
                        (lhs, rhs) if lhs == rhs => typing!(lhs),
                        (lhs, rhs) => Err(format!("if-else term: {lhs} != {rhs}")),
                    },
                    (_, None) => typing!(Type::Void),
                }
            }
            Expr::Match(val, pats) => {
                let typ = val.infer(ctx)?.solve(ctx);
                if let Type::Class(Generic(name, _)) = &typ
                    && let (_, Object::Enum(mut layout)) = ctx.global.table[name].clone()
                {
                    let _ = map!(pats, |(key, _, _)| layout.shift_remove(key));
                    if let Some((lacked, _)) = layout.first() {
                        return Err(format!("not covered: {name}.{lacked}"));
                    }
                } else {
                    return Err(format!("match: Enum != {typ}"));
                };
                let mut expr = Expr::Null(Type::Any);
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
                match cond.infer(ctx)? {
                    Type::Boolean => typing!(body.infer(ctx)?),
                    cond => Err(format!("while-do test: Bool != {cond}")),
                }
            }
            Expr::For(cnt, arr, body) => {
                let (typ, temp) = (arr.infer(ctx)?, Box::new(temp!()));
                let Type::Array(_) = typ else {
                    return Err(format!("not iterable: {typ}"));
                };
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
                let parent = ctx.local.scope.clone();
                let lines = map!({ &lines }, |x| x.infer(ctx))?;
                let ret = lines.last().unwrap_or(&Type::Void);
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
            Expr::Return(val) => match (val.infer(ctx)?, ctx.local.ret.solve(ctx)) {
                (val, ret) if ret == val => typing!(Type::Void),
                (val, ret) => Err(format!("return: {ret} != {val}")),
            },
            Expr::Call(callee, args) => {
                let args = map!({ args }, |x| x.infer(ctx))?;
                if let Some(obj) = args.first() {
                    ctx.local.class = Some(obj.clone());
                }
                match callee.infer(ctx)? {
                    Type::Function(Lambda((_, ret), Some(params))) => {
                        let (pl, al) = (params.len(), args.len());
                        for (param, arg) in params.iter().zip(args) {
                            if param.solve(ctx) != arg {
                                return Err(format!("argument: {param} != {arg}"));
                            }
                        }
                        if pl != al {
                            return Err(format!("argument: {pl} != {al}"));
                        }
                        typing!(*ret)
                    }
                    Type::Function(Lambda((_, ret), None)) => typing!(*ret),
                    typ => Err(format!("not callable: {typ}")),
                }
            }
            Expr::Variable(Generic(name, mut args)) => {
                if let Some(class) = &ctx.local.class {
                    let name = name.class(&class.remove_generic());
                    if ctx.global.lib.contains_key(&name) {
                        args.append(&mut class.generic_args());
                        return typing!(expands!(Expr::Variable(Generic(name, args))));
                    }
                    ctx.local.class = None;
                }
                if let Some(typ) = ctx.global.lib.get(&name).cloned() {
                    let args = if name.is_generic() { vec![] } else { args };
                    let var = Expr::Variable(Generic(name.clone(), map!(args, |x| x.solve(ctx))));
                    if self != &var {
                        ctx.local.expand.insert(self.clone(), var);
                    }
                    typing!(typ.mono(ctx, Generic(name, args))?)
                } else if let Some(typ) = ctx.local.scope.get(&name) {
                    typing!(typ.solve(ctx))
                } else {
                    Err(format!("undefined: {name}"))
                }
            }
            Expr::Let(name, val) => match &*name {
                Expr::Variable(Generic(name, _)) => {
                    let val = val.infer(ctx)?;
                    if let Some(typ) = ctx.local.scope.get(name) {
                        let typ = typ.solve(ctx);
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
                    let [typ, rhs] = [acc.infer(ctx)?, val.infer(ctx)?];
                    let Generic(name, _) = &obj.infer(ctx)?.unwrap_class();
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
                let (typ, temp) = (array[0].infer(ctx)?, temp!());
                let mut expr = vec![Expr::Let(
                    Box::new(temp.clone()),
                    Box::new(Expr::Init(typ, Box::new(Expr::Integer(array.len() as i64)))),
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
                let Type::Class(generic) = typ.clone() else {
                    return Err(format!("no constructor: {typ}"));
                };
                let typ = typ.mono(ctx, generic)?;
                expand!(new!(Expr::Integer(typ.size(ctx) as i64 / 8)));
                typing!(typ.solve(ctx))
            }
            Expr::Enum(typ, key, val) => {
                let temp = Box::new(temp!());
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
                    return match key.to_string().as_str() {
                        "len" => typing!(expands!(Expr::Len(obj))),
                        key => Err(format!("undefined: {typ}.{key}")),
                    };
                };
                let unify = &name.generic();
                if !ctx.global.table.contains_key(unify) {
                    Expr::New(typ.clone()).infer(ctx)?;
                }
                let (_, class) = ctx.global.table[unify].clone();
                let (Object::Struct(layout) | Object::Enum(layout)) = &class;
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
                match expr.infer(ctx)? {
                    Type::Class(_) => typing!(Type::Boolean),
                    typ => Err(format!("can't null-check: {typ}")),
                }
            }
            Expr::Init(typ, len) => {
                expand!(new!(Expr::Add(len, Box::new(Expr::Integer(1)))));
                typing!(Type::Array(Box::new(typ.clone())))
            }
            Expr::Read(offset, typ, addr) => {
                addr.infer(ctx)?;
                match offset.infer(ctx)? {
                    Type::Integer => typing!(typ.clone()),
                    typ => Err(format!("not address: {typ}")),
                }
            }
            Expr::Write(offset, val, addr) => {
                addr.infer(ctx)?;
                match offset.infer(ctx)? {
                    Type::Integer => typing!(val.infer(ctx)?),
                    typ => Err(format!("not address: {typ}")),
                }
            }
            Expr::Clone(expr) => {
                let (dest, typ) = (Box::new(temp!()), expr.infer(ctx)?);
                let (init, size) = match typ.clone() {
                    Type::Array(typ) => (
                        Expr::Init(*typ, len!(expr)),
                        Expr::Mul(
                            Box::new(Expr::Add(len!(expr), Box::new(Expr::Integer(1)))),
                            Box::new(Expr::Integer(8)),
                        ),
                    ),
                    Type::Class(_) => (Expr::New(typ.clone()), Expr::Integer(typ.size(ctx) as i64)),
                    typ => return Err(format!("can't clone: {typ}")),
                };
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(dest.clone(), Box::new(init)),
                    Expr::Call(
                        Box::new(var!("memcpy", typ.clone())),
                        vec![*dest.clone(), *expr, size]
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
            Expr::Not(term) => {
                let typ = Box::new(Expr::Null(Type::Boolean));
                op!(Type::Boolean, typ, term)
            }
            Expr::And(lhs, rhs) | Expr::Or(lhs, rhs) | Expr::Xor(lhs, rhs) => {
                op!(Type::Boolean, lhs, rhs)
            }
        }
    }

    fn fmtgen(&mut self, ctx: &mut Context) -> Result<String, String> {
        macro_rules! custom {
            ($fmter: expr) => {{
                *self = Expr::Call(Box::new($fmter), vec![self.clone()]);
                self.fmtgen(ctx)
            }};
        }
        match self.infer(ctx)? {
            Type::Integer => Ok(String::from("%ld")),
            Type::String => Ok(String::from("%s")),
            Type::Float => Ok(String::from("%g")),
            Type::Array(typ) => custom!(var!("Vec", *typ)),
            typ => custom!(method!(typ, "print")),
        }
    }
}

impl Type {
    fn mono(&self, ctx: &mut Context, Generic(name, args): Generic) -> Result<Type, String> {
        let (mut typ, args) = (self.solve(ctx), map!(args, |x| x.solve(ctx)));
        let mangle = Generic(name.clone(), args.clone()).generic();
        match typ.clone() {
            Type::Function(Lambda((params, _), _)) if !params.is_empty() => {
                let mut alias = IndexMap::new();
                for (param, arg) in params.iter().zip(&args) {
                    alias.insert(param.clone(), arg.clone());
                    typ = typ.rewrite(param, arg);
                }
                let mut unify = ctx.global.def[&name].clone();
                if let Define::Function((_, params), _) | Define::Declare((_, params), _) = &unify
                    && let Type::Function(Lambda((_, ret), Some(args))) = typ.clone()
                {
                    let head = (
                        Generic(mangle.clone(), Vec::new()),
                        params.keys().cloned().zip(args).collect(),
                    );
                    unify = match unify.clone() {
                        Define::Function(_, (body, _)) => Define::Function(head, (body, *ret)),
                        _ => Define::Declare(head, *ret),
                    };
                };
                let parent = ctx.global.alias.clone();
                ctx.global.alias = alias.clone();
                if let Define::Function(_, _) = unify {
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
                ctx.global.table.insert(mangle.clone(), (Vec::new(), unify));
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
            Type::Function(Lambda((typ, ret), Some(args))) => Type::Function(Lambda(
                (typ.clone(), Box::new(ret.rewrite(old, new))),
                Some(map!(args, |x| x.rewrite(old, new))),
            )),
            Type::Class(Generic(name, args)) => {
                Type::Class(Generic(name.clone(), map!(args, |x| x.rewrite(old, new))))
            }
            Type::Array(typ) => Type::Array(Box::new(typ.rewrite(old, new))),
            _ => self.clone(),
        }
    }

    fn remove_generic(&self) -> Type {
        match self {
            Type::Function(Lambda((_removed, ret), Some(args))) => Type::Function(Lambda(
                (Vec::new(), Box::new(ret.remove_generic())),
                Some(map!(args, Type::remove_generic)),
            )),
            Type::Class(Generic(name, _removed)) => Type::Class(Generic(name.clone(), Vec::new())),
            Type::Array(typ) => Type::Array(Box::new(typ.remove_generic())),
            _ => self.clone(),
        }
    }

    fn generic_args(&self) -> Vec<Type> {
        match self {
            Type::Class(Generic(_, generic)) => generic.clone(),
            _ => Vec::new(),
        }
    }

    fn solve(&self, ctx: &Context) -> Type {
        let mut typ = self.clone();
        for (old, new) in &ctx.global.alias {
            typ = typ.rewrite(old, new);
        }
        typ
    }

    fn size(&self, ctx: &Context) -> usize {
        let Generic(name, _) = self.clone().unwrap_class();
        match &ctx.global.table[&name] {
            (_, Object::Struct(layout)) => layout.len() * 8,
            (_, Object::Enum(_)) => 16,
        }
    }
}
