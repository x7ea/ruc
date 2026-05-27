use crate::*;

impl Define {
    pub fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        match self {
            Define::Function(Generics(name, param), args, body) => {
                let parent = ctx.local.clone();
                ctx.local = Function::default();
                ctx.local.scope = args.clone();

                let ret = body.infer(ctx);
                let args = Some(args.values().cloned().collect::<Vec<Type>>());
                let sig = if !param.is_empty() {
                    ctx.global.meta.insert(name.clone());
                    Type::Function(param.clone(), Box::new(Type::None), args)
                } else {
                    Type::Function(param.clone(), Box::new(ret?), args)
                };

                ctx.table.insert(name.clone(), ctx.local.clone());
                ctx.global.lib.insert(name.clone(), sig.clone());
                ctx.local = parent;
                Ok(sig)
            }
            Define::Class(Generics(name, args), layout) => {
                let value = (args.clone(), layout.clone());
                ctx.global.table.insert(name.clone(), value);
                Ok(Type::None)
            }
        }
    }
}

impl Expr {
    fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        macro_rules! typing {
            ($typ: expr) => {{
                let typ = $typ;
                ctx.local.typed.insert(self.clone(), typ.clone());
                Ok::<Type, String>(typ)
            }};
        }
        macro_rules! expand {
            ($expr: expr) => {{
                #![allow(unused_must_use)]
                let expr = $expr.clone();
                let typ = expr.infer(ctx)?;
                ctx.local.expand.insert(self.clone(), expr.clone());
                typ.clone()
            }};
        }
        macro_rules! initializer {
            ($layout: expr) => {
                Expr::Call(
                    Box::new(Expr::Variable(Generics(Name::new("calloc")?, vec![]))),
                    vec![Expr::Integer($layout as i64), Expr::Integer(8)],
                )
            };
        }
        macro_rules! array {
            ($arr: expr, $idx: expr) => {
                Box::new(Expr::Add(
                    Box::new(Expr::Mod(
                        $idx.clone(),
                        Box::new(Expr::Member($arr.clone(), Name::new("len")?)),
                    )),
                    Box::new(Expr::Integer(1)),
                ))
            };
        }
        macro_rules! op {
            ($typ: pat, $lhs: expr, $rhs: expr $(, $ret: expr)?) => {{
                let [lt, rt] =[$lhs.infer(ctx)?, $rhs.infer(ctx)?];
                if lt == rt {
                    #[allow(warnings)]
                    if let $typ = lt {
                        $( return typing!($ret); )?
                        typing!(lt.clone())
                    } else {
                        Err(format!("no operation: {lt}"))
                    }
                } else {
                    Err(format!("term: {lt} != {rt}"))
                }
            }};
        }
        macro_rules! get {
            ($name:ident, $obj: expr) => {{
                let Type::$name(class) = $obj else { panic!() };
                class.clone()
            }};
        }
        match self {
            Expr::Print(values) => {
                let mut fmt = String::new();
                for i in values.iter() {
                    let typ = i.infer(ctx)?;
                    fmt += match typ {
                        Type::Integer => "%ld",
                        Type::Float => "%g",
                        Type::String => "%s",
                        _ => return Err(format!("can't print: {typ}")),
                    }
                }
                expand!(Expr::Call(
                    Box::new(Expr::Variable(Generics(Name::new("printf")?, vec![]))),
                    [vec![Expr::String(fmt + "\\n")], values.to_vec()].concat(),
                ));
                typing!(Type::None)
            }
            Expr::If(cond, then, els) => {
                if let Expr::Let(bind, check) = &**cond {
                    return Ok(expand!(Expr::If(
                        Box::new(Expr::Check(check.clone())),
                        Box::new(Expr::Block(vec![
                            Expr::Let(bind.clone(), check.clone()),
                            *then.clone(),
                        ])),
                        els.clone(),
                    )));
                }
                let cond = cond.infer(ctx)?;
                if cond != Type::Bool {
                    return Err(format!("if-else test: Bool != {cond}"));
                }
                if let Some(els) = els {
                    return op!(_, then, els);
                }
                then.infer(ctx)?;
                Ok(Type::None)
            }
            Expr::While(cond, body) => {
                let cond = cond.infer(ctx)?;
                if cond != Type::Bool {
                    return Err(format!("while-do test: Bool != {cond}"));
                }
                body.infer(ctx)
            }
            Expr::Block(lines) => {
                let mut ret = Type::None;
                let parent = ctx.local.scope.clone();
                for line in lines {
                    ret = line.infer(ctx)?;
                }
                for (name, value) in &ctx.local.scope {
                    if let Some(typ) = ctx.local.var.get(name) {
                        if typ != value {
                            return Err(format!("duplicated {name}: {typ} != {value}"));
                        }
                    }
                    ctx.local.var.insert(name.clone(), value.clone());
                }
                ctx.local.scope = parent;
                typing!(ret.clone())
            }
            Expr::Call(callee, args) => {
                let typ = callee.infer(ctx)?;
                if let Type::Function(_, ret, params) = typ {
                    let Some(params) = params else {
                        map!(args => |x| x.infer(ctx));
                        return typing!(*ret.clone());
                    };
                    let (pl, al) = (params.len(), args.len());
                    if pl != al {
                        return Err(format!("length: {pl} != {al}",));
                    }
                    for (param, arg) in params.iter().zip(args) {
                        let arg = arg.infer(ctx)?;
                        if arg.solve(ctx) != param.solve(ctx) {
                            return Err(format!("arguments: {param} != {arg}"));
                        }
                    }
                    typing!(*ret.clone())
                } else {
                    Err(format!("not callee: {typ}"))
                }
            }
            Expr::Variable(func @ Generics(name, args)) => {
                let env = &ctx.local.scope;
                let mut args = args.clone();
                if let Some(typ) = env.get(name) {
                    typing!(typ.clone())
                } else if let Some(typ) = ctx.global.lib.get(name) {
                    let typ = &mut typ.clone().solve(ctx);
                    if let Type::Function(params, _, Some(_)) = typ.clone() {
                        if params.len() != args.len() {
                            return Err(format!("generics: {typ}"));
                        }
                        let mut alias = IndexMap::new();
                        for arg in args.iter_mut() {
                            *arg = arg.solve(ctx);
                        }
                        for (arg, param) in args.iter().zip(params) {
                            alias.insert(param.clone(), arg.clone());
                            typ.rewrite(&param, arg);
                        }
                        let mangle = func.generics();
                        let mut unify = ctx.global.def.get(name).unwrap().clone();
                        if let Type::Function(_, _, Some(args)) = typ.clone() {
                            if let Define::Function(Generics(_, _), params, body) = &unify {
                                let mut map = IndexMap::new();
                                for (param, arg) in params.keys().zip(args) {
                                    map.insert(param.clone(), arg.solve(ctx));
                                }
                                let name = Generics(mangle.clone(), vec![]);
                                unify = Define::Function(name, map.clone(), body.clone());
                            }
                        };
                        let parent = ctx.global.alias.clone();
                        ctx.global.alias = alias.clone();
                        {
                            *typ = unify.infer(ctx)?;
                        }
                        ctx.global.alias = parent;
                        ctx.global.def.insert(mangle, unify.clone());
                    }
                    typing!(typ.clone())
                } else {
                    Err(format!("undefined: {name}"))
                }
            }
            Expr::Let(name, value) => match &**name {
                Expr::Variable(Generics(name, _)) => {
                    let val = value.infer(ctx)?;
                    let env = &mut ctx.local.scope;
                    if let Some(typ) = env.get(name) {
                        if val != *typ {
                            return Err(format!("{name}: {typ} != {val}"));
                        }
                    } else {
                        env.insert(name.clone(), val.clone());
                    }
                    typing!(Type::None)
                }
                acc @ Expr::Index(arr, idx) => {
                    let [val, typ] = [value.infer(ctx)?, acc.infer(ctx)?];
                    if typ.clone() != val {
                        return Err(format!("array: {typ} != {val}"));
                    }
                    expand!(Expr::Write(array!(arr, idx), value.clone(), arr.clone()));
                    typing!(Type::None)
                }
                acc @ Expr::Member(obj, key) => {
                    let [val, typ] = [value.infer(ctx)?, acc.infer(ctx)?];
                    let Generics(name, _) = &get!(Class, obj.infer(ctx)?);
                    if &typ.solve(ctx) != &val {
                        return Err(format!("{name}.{key}: {typ} != {val}"));
                    }
                    match ok!(ctx.global.table.get(name))? {
                        (_, Object::Struct(layout)) => {
                            let offset = layout.get_index_of(key).unwrap();
                            let offset = Box::new(Expr::Integer(offset as i64));
                            expand!(Expr::Write(offset, value.clone(), obj.clone()));
                        }
                        (_, Object::Enum(layout)) => {
                            let tag = layout.get_index_of(key).unwrap() as i64;
                            let offset = |x| Box::new(Expr::Integer(x));
                            expand!(Expr::Block(vec![
                                Expr::Write(offset(0), offset(tag), obj.clone()),
                                Expr::Write(offset(8), value.clone(), obj.clone()),
                            ]));
                        }
                    }
                    typing!(Type::None)
                }
                other => Err(format!("not assign target: {}", other.infer(ctx)?)),
            },
            Expr::Array(typ, len) => {
                expand!(initializer!(*len + 1));
                typing!(Type::Array(Box::new(typ.clone())))
            }
            Expr::New(typ) => {
                let Type::Class(Generics(name, mut args)) = typ.clone() else {
                    return Err(format!("no constructor: {typ}"));
                };
                let Some((params, table)) = ctx.global.table.get(&name).cloned() else {
                    return Err(format!("undefined: {name}"));
                };
                for arg in args.iter_mut() {
                    *arg = arg.solve(ctx);
                }
                let layout = {
                    let (Object::Enum(layout) | Object::Struct(layout)) = &table;
                    let mut layout = layout.clone();
                    if params.len() != args.len() {
                        return Err(format!("generics: {typ}"));
                    }
                    for (key, mut field) in layout.clone() {
                        for (arg, param) in args.iter().zip(&params) {
                            field.rewrite(&param, &arg);
                            layout.insert(key.clone(), field.clone());
                        }
                    }
                    layout
                };
                let unify = match table {
                    Object::Enum(_) => {
                        expand!(initializer!(2));
                        Object::Enum(layout).clone()
                    }
                    Object::Struct(inner) => {
                        expand!(initializer!(inner.len()));
                        Object::Struct(layout).clone()
                    }
                };
                let mangle = Generics(name.clone(), args).generics();
                ctx.global.table.insert(mangle.clone(), (vec![], unify));
                typing!(typ.solve(ctx))
            }
            Expr::Index(arr, idx) => {
                let typ = arr.infer(ctx)?;
                let Type::Array(typ) = typ else {
                    return Err(format!("not array: {typ}"));
                };
                let idx_t = idx.infer(ctx)?;
                let Type::Integer = idx_t else {
                    return Err(format!("not index: {idx_t}"));
                };
                expand!(Expr::Read(array!(arr, idx), *typ.clone(), arr.clone()));
                typing!(*typ.clone())
            }
            Expr::Member(obj, key) => {
                let typ = obj.infer(ctx)?;
                if let Type::Array(_) = typ.clone() {
                    if key.to_string() == "len" {
                        expand!(Expr::Read(
                            Box::new(Expr::Integer(0)),
                            Type::Integer,
                            obj.clone()
                        ));
                        return typing!(Type::Integer);
                    }
                }
                let Type::Class(class @ Generics(name, _)) = &typ else {
                    return Err(format!("not class: {typ}"));
                };
                let Some((_, class)) = ctx.global.table.get(&class.generics()) else {
                    return Err(format!("undefined: {name}"));
                };
                let (Object::Struct(layout) | Object::Enum(layout)) = class;
                let Some(typ) = layout.get(key).cloned() else {
                    return Err(format!("undefined: {name}.{key}"));
                };
                match class {
                    Object::Struct(layout) => {
                        let offset = Expr::Integer(layout.get_index_of(key).unwrap() as i64);
                        expand!(Expr::Read(Box::new(offset), typ.clone(), obj.clone()));
                    }
                    Object::Enum(_) => {
                        let offset = Box::new(Expr::Integer(8));
                        expand!(Expr::If(
                            Box::new(Expr::Check(obj.clone())),
                            Box::new(Expr::Read(offset, typ.clone(), obj.clone())),
                            Some(Box::new(Expr::Null(typ.clone())))
                        ));
                    }
                }
                typing!(typ)
            }
            Expr::Check(expr) => {
                if let Expr::Member(obj, key) = &**expr {
                    let typ = obj.infer(ctx)?;
                    if let Type::Class(Generics(name, _)) = &typ {
                        if let Some((_, Object::Enum(layout))) = ctx.global.table.get(name) {
                            let tag = layout.get_index_of(key).unwrap();
                            let offset = Box::new(Expr::Integer(0));
                            expand!(Expr::Eql(
                                Box::new(Expr::Read(offset, Type::Integer, obj.clone())),
                                Box::new(Expr::Integer(tag as i64)),
                            ));
                            return typing!(Type::Bool);
                        }
                    }
                }
                let typ = expr.infer(ctx)?;
                if !matches!(typ, Type::Class(_)) {
                    return Err(format!("not nullable: {typ}"));
                }
                typing!(Type::Bool)
            }
            Expr::Read(addr, typ, offset) => {
                let offset = offset.infer(ctx)?;
                if let Type::Integer = offset {
                    return Err(format!("not address: {offset}"));
                }
                addr.infer(ctx)?;
                typing!(typ.clone())
            }
            Expr::Write(addr, value, offset) => {
                let offset = offset.infer(ctx)?;
                if let Type::Integer = offset {
                    return Err(format!("not address: {offset}"));
                }
                addr.infer(ctx)?;
                typing!(value.infer(ctx)?)
            }
            Expr::Mod(lhs, rhs) => {
                expand!(Expr::Div(lhs.clone(), rhs.clone()));
                op!(Type::Integer, lhs, rhs)
            }
            Expr::Null(typ) => {
                if let Type::Float = typ {
                    expand!(Expr::Float(Float::from(0.0)));
                } else {
                    expand!(Expr::Integer(0));
                }
                typing!(typ.clone())
            }
            Expr::Integer(_) => typing!(Type::Integer),
            Expr::Float(_) => typing!(Type::Float),
            Expr::String(_) => typing!(Type::String),
            Expr::Bool(_) => typing!(Type::Bool),
            Expr::Add(lhs, rhs) => op!(Type::Integer | Type::Float, lhs, rhs),
            Expr::Sub(lhs, rhs) => op!(Type::Integer | Type::Float, lhs, rhs),
            Expr::Mul(lhs, rhs) => op!(Type::Integer | Type::Float, lhs, rhs),
            Expr::Div(lhs, rhs) => op!(Type::Integer | Type::Float, lhs, rhs),
            Expr::Eql(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Bool),
            Expr::NotEq(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Bool),
            Expr::Gt(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Bool),
            Expr::Lt(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Bool),
            Expr::GtEq(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Bool),
            Expr::LtEq(lhs, rhs) => op!(Type::Integer, lhs, rhs, Type::Bool),
            Expr::And(lhs, rhs) => op!(Type::Bool, lhs, rhs),
            Expr::Or(lhs, rhs) => op!(Type::Bool, lhs, rhs),
            Expr::Xor(lhs, rhs) => op!(Type::Bool, lhs, rhs),
        }
    }
}

impl Type {
    fn rewrite(&mut self, old: &Type, new: &Type) {
        if self == old {
            *self = new.clone()
        }
        match self {
            Type::Function(_, ret, args) => {
                ret.rewrite(old, new);
                if let Some(args) = args {
                    for arg in args {
                        arg.rewrite(old, new);
                    }
                }
            }
            Type::Class(Generics(_, args)) => {
                for arg in args {
                    arg.rewrite(old, new);
                }
            }
            Type::Array(typ) => typ.rewrite(old, new),
            _ => {}
        }
    }

    pub fn solve(&self, ctx: &mut Context) -> Type {
        if let Some(typ) = ctx.global.alias.get(self) {
            return typ.clone();
        }
        match self {
            Type::Function(poly, ret, Some(args)) => Type::Function(
                poly.clone(),
                Box::new(ret.solve(ctx)),
                Some(map!(args, |x| x.solve(ctx))),
            ),
            Type::Class(Generics(name, args)) => {
                Type::Class(Generics(name.clone(), map!(args, |x| x.solve(ctx))))
            }
            Type::Array(typ) => Type::Array(Box::new(typ.solve(ctx))),
            _ => self.clone(),
        }
    }
}
