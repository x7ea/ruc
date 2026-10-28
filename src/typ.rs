use crate::*;

impl Define {
    pub fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        match self.clone() {
            Define::Function((Generic(name, params), args), (body, ret)) => {
                ctx.global.lib.insert(name.clone(), self.signature());
                if params.is_empty() {
                    let parent = ctx.local.clone();
                    ctx.local = Function {
                        scope: args.clone(),
                        ret: ret.solve(ctx),
                        ..Function::default()
                    };
                    let value = body.infer(ctx)?;
                    if ctx.local.ret != value {
                        return Err(format!("return: {ret} != {value}"));
                    }
                    ctx.table.insert(name.clone(), ctx.local.clone());
                    ctx.local = parent;
                }
            }
            Define::Declare((Generic(name, _), _), _) => {
                ctx.global.extrn.insert(name.clone());
                ctx.global.lib.insert(name, self.signature());
            }
            Define::Class(Generic(name, args), layout) => {
                let obj = (args.clone(), layout.clone());
                ctx.global.table.insert(name, obj);
            }
            Define::Symbol(name, _) => {
                ctx.global.lib.insert(name.clone(), self.signature());
                ctx.global.extrn.insert(name);
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
        macro_rules! infer {
            ($ast: expr) => {{ $ast.clone().infer(ctx)? }};
        }
        macro_rules! typing {
            ($ret: expr) => {{
                let typ = $ret.solve(ctx).clone();
                ctx.local.typed.insert(self.clone(), typ.clone());
                Ok::<Type, String>(typ)
            }};
        }
        macro_rules! expands {
            ($value: expr) => {{
                let expr = $value.clone();
                ctx.local.expand.insert(self.clone(), expr.clone());
                infer!(expr)
            }};
        }
        macro_rules! expand {
            ($expr: expr) => {{
                let _ = expands!($expr);
            }};
        }
        macro_rules! temp {
            () => {{ var!(&format!("temp{self:?}")) }};
        }
        macro_rules! op {
            ($typ: pat, $lhs: expr, $rhs: expr) => {{
                let op = &self.as_ref().to_lowercase();
                match (infer!($lhs), infer!($rhs)) {
                    (lhs, rhs) if lhs != rhs => Err(format!("{op} term: {lhs} != {rhs}",)),
                    ($typ, ret @ $typ) => typing!(ret.clone()),
                    (typ, _) => typing!(expands!(Expr::Call(
                        Box::new(method!(&typ, op)),
                        vec![*$lhs, *$rhs],
                    ))),
                }
            }};
            ($typ: pat, $lhs: expr, $rhs: expr, $ret: expr) => {{ op!($typ, $lhs, $rhs).and_then(|_| typing!($ret.clone())) }};
        }
        let offset = |n| Box::new(Expr::Integer(n));

        match self.clone() {
            Expr::Print(is_output, mut vals) => {
                let fmt = map!({ &mut vals }, |x| x.fmtgen(ctx))?.concat();
                let fmt = Expr::String(if is_output { fmt + "\\n" } else { fmt });
                typing!(expands!(Expr::Call(
                    Box::new(var!(["g_strdup_printf", "printf"][is_output as usize])),
                    [vec![fmt], vals.to_vec()].concat(),
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
                if Type::Boolean != infer!(cond) {
                    return Err(format!("if-else test: Bool != {}", infer!(cond)));
                }
                match (infer!(then), els) {
                    (lhs, Some(rhs)) => match (lhs, infer!(rhs)) {
                        (lhs, rhs) if lhs == rhs => typing!(lhs),
                        (lhs, rhs) => Err(format!("if-else term: {lhs} != {rhs}")),
                    },
                    (_, None) => typing!(Type::Void),
                }
            }
            Expr::Match(val, pats) => {
                let typ = infer!(val).solve(ctx);
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
                match infer!(cond) {
                    Type::Boolean => typing!(infer!(body)),
                    cond => Err(format!("while-do test: Bool != {cond}")),
                }
            }
            Expr::For(cnt, arr, body) => {
                let (typ, temp) = (infer!(arr), Box::new(temp!()));
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
            Expr::Block(mut lines) => {
                let parent = ctx.local.scope.clone();
                let lines = map!({ lines }, |x| Ok(infer!(x)))?;
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
            Expr::Return(val) => match (infer!(val), ctx.local.ret.solve(ctx)) {
                (val, ret) if ret == val => typing!(Type::Void),
                (val, ret) => Err(format!("return: {ret} != {val}")),
            },
            Expr::Call(func, args) => {
                let args = map!({ args }, |x| Ok(infer!(x)))?;
                if let Some(obj) = args.first() {
                    ctx.local.class = Some(obj.clone());
                }
                match infer!(func) {
                    Type::Function(Lambda((_, ret), Some(params))) => {
                        for (param, arg) in params.iter().zip(&args) {
                            if param.solve(ctx) != *arg {
                                return Err(format!("argument: {param} != {arg}"));
                            }
                        }
                        match (params.len(), args.len()) {
                            (pl, al) if pl == al => typing!(*ret),
                            (pl, al) => Err(format!("argument: {pl} != {al}")),
                        }
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
                    ctx.global.used.insert(name.clone());
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
                    let val = infer!(val);
                    if let Some(typ) = ctx.local.scope.get(name) {
                        if val != typ.solve(ctx) {
                            return Err(format!("{name}: {typ} != {val}"));
                        }
                    } else {
                        ctx.local.scope.insert(name.clone(), val.clone());
                    }
                    typing!(Type::Void)
                }
                acc @ Expr::Index(arr, idx) => {
                    expand!(Expr::Write(array!(arr, idx), val.clone(), arr.clone()));
                    match (infer!(val), infer!(acc)) {
                        (val, typ) if val == typ => typing!(Type::Void),
                        (val, typ) => Err(format!("array: {typ} != {val}")),
                    }
                }
                acc @ Expr::Member(obj, key) => {
                    let [typ, rhs] = [infer!(acc), infer!(val)];
                    let Generic(name, _) = &infer!(obj).unwrap_class();
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
                            expand!(Expr::Block(vec![
                                Expr::Write(offset(0), offset(tag as i64), obj.clone()),
                                Expr::Write(offset(8), val.clone(), obj.clone()),
                            ]));
                        }
                    }
                    typing!(Type::Void)
                }
                other => Err(format!("assign target: {}", infer!(other))),
            },
            Expr::Sequence(array) => {
                let (typ, temp) = (infer!(array[0]), temp!());
                let mut expr = vec![Expr::Let(
                    Box::new(temp.clone()),
                    Box::new(Expr::Init(typ, offset(array.len() as i64))),
                )];
                for (idx, val) in array.iter().enumerate() {
                    expr.push(Expr::Let(
                        Box::new(Expr::Index(Box::new(temp.clone()), offset(idx as i64))),
                        Box::new(val.clone()),
                    ));
                }
                expr.push(temp);
                typing!(expands!(Expr::Block(expr)))
            }
            Expr::Index(arr, idx) => {
                let Type::Array(typ) = infer!(arr) else {
                    return Err(format!("not array: {}", infer!(arr)));
                };
                match infer!(idx) {
                    Type::Integer => {
                        expand!(Expr::Read(array!(arr, idx), *typ.clone(), arr.clone()));
                        typing!(*typ.clone())
                    }
                    typ => Err(format!("not index: {typ}")),
                }
            }
            Expr::Len(obj) => typing!(expands!(match infer!(obj) {
                Type::String => Expr::Call(Box::new(var!("strlen")), vec![*obj.clone()]),
                Type::Array(_) => Expr::Read(Box::new(Expr::Integer(0)), Type::Integer, obj),
                typ => return Err(format!("no length: {typ}")),
            })),
            Expr::New(class) => {
                let Type::Class(generic) = class.clone() else {
                    return Err(format!("no constructor: {class}"));
                };
                let typ = class.mono(ctx, generic)?;
                expand!(new!(Expr::Integer(typ.size(ctx) as i64 / 8)));
                typing!(typ.solve(ctx))
            }
            Expr::Enum(typ, key, val) => {
                let temp = Box::new(temp!());
                let acc = Box::new(Expr::Member(temp.clone(), key.clone()));
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(temp.clone(), Box::new(Expr::New(typ.clone()))),
                    Expr::Let(acc, val.clone()),
                    *temp
                ])))
            }
            Expr::Member(obj, key) => {
                let typ = infer!(obj);
                let Type::Class(name) = &typ else {
                    return match key.to_string().as_str() {
                        "len" => typing!(expands!(Expr::Len(obj))),
                        key => Err(format!("undefined: {typ}.{key}")),
                    };
                };
                let unify = &name.generic();
                if !ctx.global.table.contains_key(unify) {
                    infer!(Expr::New(typ.clone()));
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
                    Object::Enum(_) => expand!(Expr::If(
                        Box::new(Expr::Check(Box::new(self.clone()))),
                        Box::new(Expr::Read(offset(8), typ.clone(), obj.clone())),
                        Some(Box::new(Expr::Null(typ.clone())))
                    )),
                }
                typing!(typ.solve(ctx))
            }
            Expr::Check(expr) => {
                if let Expr::Member(obj, key) = &*expr
                    && let Type::Class(Generic(name, _)) = &infer!(obj)
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
                match infer!(expr) {
                    Type::Class(_) => typing!(Type::Boolean),
                    typ => Err(format!("can't null-check: {typ}")),
                }
            }
            Expr::Init(typ, len) => {
                expand!(new!(Expr::Add(len, Box::new(Expr::Integer(1)))));
                typing!(Type::Array(Box::new(typ.clone())))
            }
            Expr::Read(offset, typ, addr) => {
                infer!(addr);
                match infer!(offset) {
                    Type::Integer => typing!(typ.clone()),
                    typ => Err(format!("not address: {typ}")),
                }
            }
            Expr::Write(offset, val, addr) => {
                infer!(addr);
                match infer!(offset) {
                    Type::Integer => typing!(infer!(val)),
                    typ => Err(format!("not address: {typ}")),
                }
            }
            Expr::Clone(expr) => {
                let offset = |n| Box::new(Expr::Integer(n));
                let (dest, typ) = (Box::new(temp!()), infer!(expr));
                let bound = Box::new(Expr::Add(len!(expr), offset(1)));
                let (init, size) = match typ.clone() {
                    Type::Array(typ) => (Expr::Init(*typ, len!(expr)), Expr::Mul(bound, offset(8))),
                    Type::Class(_) => (Expr::New(typ.clone()), Expr::Integer(typ.size(ctx) as i64)),
                    typ => return Err(format!("can't clone: {typ}")),
                };
                let arg = vec![*dest.clone(), *expr, size];
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(dest.clone(), Box::new(init)),
                    Expr::Call(Box::new(var!("memcpy", typ.clone())), arg),
                    *dest.clone()
                ])))
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
            Expr::Mod(lhs, rhs) => {
                expand!(Expr::Div(lhs.clone(), rhs.clone()));
                op!(Type::Integer, lhs, rhs)
            }
            Expr::Eq(lhs, rhs)
            | Expr::Ne(lhs, rhs)
            | Expr::Gt(lhs, rhs)
            | Expr::Lt(lhs, rhs)
            | Expr::Ge(lhs, rhs)
            | Expr::Le(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Boolean),
            Expr::And(lhs, rhs) | Expr::Or(lhs, rhs) | Expr::Xor(lhs, rhs) => {
                op!(Type::Boolean, lhs, rhs)
            }
            Expr::Not(rhs) => {
                let lhs = Box::new(Expr::Null(Type::Boolean));
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
                        Generic(mangle.clone(), vec![]),
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
                ctx.global.used.insert(mangle.clone());
                ctx.global.def.insert(mangle, unify.clone());
                ctx.global.alias = parent;
            }
            Type::Class(Generic(name, args)) => {
                let Some((params, table)) = ctx.global.table.get(&name) else {
                    return Err(format!("undefined: {name}"));
                };
                let (Object::Enum(mut layout) | Object::Struct(mut layout)) = table.clone();
                for (_, field) in layout.iter_mut() {
                    for (arg, param) in args.iter().zip(params) {
                        *field = field.rewrite(param, arg);
                    }
                }
                let unify = match table {
                    Object::Enum(_) => Object::Enum(layout),
                    Object::Struct(_) => Object::Struct(layout),
                };
                ctx.global.table.insert(mangle.clone(), (vec![], unify));
            }
            _ => (),
        }
        Ok(typ.solve(ctx))
    }

    fn rewrite(&self, old: &Type, new: &Type) -> Type {
        if self == old {
            return new.clone();
        }
        match self.clone() {
            Type::Function(Lambda((typ, ret), args)) => Type::Function(Lambda(
                (typ, Box::new(ret.rewrite(old, new))),
                args.and_then(|args| Some(map!(args, |x| x.rewrite(old, new)))),
            )),
            Type::Class(Generic(name, args)) => {
                Type::Class(Generic(name, map!(args, |x| x.rewrite(old, new))))
            }
            Type::Array(typ) => Type::Array(Box::new(typ.rewrite(old, new))),
            _ => self.clone(),
        }
    }

    fn remove_generic(&self) -> Type {
        match self {
            Type::Class(Generic(name, _removed)) => Type::Class(Generic(name.clone(), vec![])),
            Type::Array(typ) => Type::Array(Box::new(typ.remove_generic())),
            _ => self.clone(),
        }
    }

    fn generic_args(&self) -> Vec<Type> {
        match self {
            Type::Class(Generic(_, generic)) => generic.clone(),
            _ => vec![],
        }
    }

    fn solve(&self, ctx: &Context) -> Type {
        let alias = ctx.global.alias.iter();
        alias.fold(self.clone(), |typ, (old, new)| typ.rewrite(old, new))
    }

    fn size(&self, ctx: &Context) -> usize {
        let Generic(name, _) = self.clone().unwrap_class();
        match &ctx.global.table[&name] {
            (_, Object::Struct(layout)) => layout.len() * 8,
            (_, Object::Enum(_)) => 16,
        }
    }
}
