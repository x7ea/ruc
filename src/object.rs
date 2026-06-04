use crate::*;

impl Expr {
    fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        macro_rules! typing {
            ($typ: expr) => {{
                let typ = $typ;
                ctx.local.typed.insert(self.clone(), typ.clone());
                Ok::<Type, String>(typ)
            }};
        }
        macro_rules! expands {
            ($expr: expr) => {{
                let expr = $expr.clone();
                let typ = expr.infer(ctx)?;
                ctx.local.expand.insert(self.clone(), expr.clone());
                typ.clone()
            }};
        }
        macro_rules! expand {
            ($expr: expr) => {{
                let _ = expands!($expr);
            }};
        }
        macro_rules! new {
            ($layout: expr) => {
                Expr::Call(
                    Box::new(Expr::Variable(Generics(Name::new("calloc")?, vec![]))),
                    vec![Expr::Integer($layout as i64), Expr::Integer(8)],
                )
            };
        }
        macro_rules! len {
            ($arr: expr) => {
                Box::new(Expr::Member($arr.clone(), Name::new("len")?))
            };
        }
        macro_rules! array {
            ($arr: expr, $idx: expr) => {
                Box::new(Expr::Add(
                    Box::new(Expr::Mod($idx.clone(), len!($arr))),
                    Box::new(Expr::Integer(1)),
                ))
            };
        }
        macro_rules! get {
            ($name:ident, $obj: expr) => {{
                let Type::$name(class) = $obj else { panic!() };
                class.clone()
            }};
        }
        match self.clone() {
            Expr::Variable(func) => {
                let Generics(name, mut args) = func.clone();
                if let Some(obj) = ctx.local.class.clone() {
                    ctx.local.class = None;
                    let name = Name::new(&format!("{obj}.{name}"))?;
                    if ctx.global.lib.contains_key(&name) {
                        return typing!(expands!(Expr::Variable(Generics(name, args.clone()))));
                    }
                }
                if let Some(typ) = ctx.global.lib.get(&name) {
                    let typ = &mut typ.clone().solve(ctx);
                    if let Type::Function(params, _, Some(_)) = typ.clone()
                        && !params.is_empty()
                    {
                        if params.len() != args.len() {
                            return Err(format!("generics: {typ}"));
                        }
                        let mut alias = IndexMap::new();
                        for arg in args.iter_mut() {
                            *arg = arg.solve(ctx);
                        }
                        for (arg, param) in args.iter().zip(&params) {
                            alias.insert(param.clone(), arg.clone());
                            *typ = typ.rewrite(param, arg);
                        }
                        let mangle = func.generics();
                        let mut unify = ctx.global.def.get(&name).unwrap().clone();
                        if let Define::Function(Generics(_, _), params, body) = &unify
                            && let Type::Function(_, _, Some(args)) = typ.clone()
                        {
                            let mut map = IndexMap::new();
                            for (param, arg) in params.keys().zip(args) {
                                map.insert(param.clone(), arg);
                            }
                            let name = Generics(mangle.clone(), vec![]);
                            unify = Define::Function(name, map.clone(), body.clone());
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
                } else if let Some(typ) = ctx.local.scope.get(&name) {
                    typing!(typ.clone().solve(ctx))
                } else {
                    Err(format!("undefined: {name}"))
                }
            }
            Expr::Let(name, val) => match &*name {
                Expr::Variable(Generics(name, _)) => {
                    let val = val.infer(ctx)?;
                    if let Some(typ) = ctx.local.scope.get(name) {
                        let typ = typ.clone().solve(ctx);
                        if val != typ {
                            return Err(format!("{name}: {typ} != {val}"));
                        }
                    } else {
                        ctx.local.scope.insert(name.clone(), val.clone());
                    }
                    typing!(Type::None)
                }
                acc @ Expr::Index(arr, idx) => {
                    {
                        let [val, typ] = [val.infer(ctx)?, acc.infer(ctx)?];
                        if typ.clone() != val {
                            return Err(format!("array[n] {typ} != {val}"));
                        }
                    }
                    expand!(Expr::Write(array!(arr, idx), val.clone(), arr.clone()));
                    typing!(Type::None)
                }
                acc @ Expr::Member(obj, key) => {
                    let typ = acc.infer(ctx)?;
                    let Generics(name, _) = &get!(Class, obj.infer(ctx)?);
                    {
                        let val = val.infer(ctx)?;
                        if typ.solve(ctx) != val {
                            return Err(format!("{name}.{key}: {typ} != {val}"));
                        }
                    }
                    match ok!(ctx.global.table.get(name))? {
                        (_, Object::Struct(layout)) => {
                            let offset = layout.get_index_of(key).unwrap();
                            let offset = Box::new(Expr::Integer(offset as i64));
                            expand!(Expr::Write(offset, val.clone(), obj.clone()));
                        }
                        (_, Object::Enum(layout)) => {
                            let tag = layout.get_index_of(key).unwrap() as i64;
                            let offset = |x| Box::new(Expr::Integer(x));
                            expand!(Expr::Block(vec![
                                Expr::Write(offset(0), offset(tag), obj.clone()),
                                Expr::Write(offset(8), val.clone(), obj.clone()),
                            ]));
                        }
                    }
                    typing!(Type::None)
                }
                other => Err(format!("not assign target: {}", other.infer(ctx)?)),
            },
            Expr::Constructor(typ) => {
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
                    for (key, field) in layout.clone() {
                        for (arg, param) in args.iter().zip(&params) {
                            let field = field.rewrite(param, arg);
                            layout.insert(key.clone(), field.clone());
                        }
                    }
                    layout
                };
                let unify = match table {
                    Object::Enum(_) => {
                        expand!(new!(2));
                        Object::Enum(layout).clone()
                    }
                    Object::Struct(inner) => {
                        expand!(new!(inner.len()));
                        Object::Struct(layout).clone()
                    }
                };
                let mangle = Generics(name.clone(), args).generics();
                ctx.global.table.insert(mangle.clone(), (vec![], unify));
                typing!(typ.solve(ctx))
            }
            Expr::Member(obj, key) => {
                let typ = obj.infer(ctx)?;
                if let Type::Array(_) = typ.clone()
                    && key.to_string() == "len"
                {
                    return typing!(expands!(Expr::Read(
                        Box::new(Expr::Integer(0)),
                        Type::Integer,
                        obj.clone()
                    )));
                }
                let Type::Class(name) = &typ else {
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
                typing!(typ)
            }
        }
    }
}

impl Type {
    fn rewrite(&self, old: &Type, new: &Type) -> Type {
        if self == old {
            return new.clone();
        }
        match self {
            Type::Function(typ, ret, Some(args)) => Type::Function(
                typ.clone(),
                Box::new(ret.rewrite(old, new)),
                Some(map!(args, |x| x.rewrite(old, new))),
            ),
            Type::Class(Generics(name, args)) => {
                Type::Class(Generics(name.clone(), map!(args, |x| x.rewrite(old, new))))
            }
            Type::Array(typ) => Type::Array(Box::new(typ.rewrite(old, new))),
            _ => self.clone(),
        }
    }

    pub fn size(&self, ctx: &Context) -> Result<usize, String> {
        match self {
            Type::Class(Generics(name, _)) => match ctx.global.table.get(name) {
                Some((_, Object::Struct(layout))) => Ok(layout.len() * 8),
                Some((_, Object::Enum(_))) => Ok(16),
                _ => Err(format!("undefined: {name}")),
            },
            _ => Err(format!("can't clone: {self}")),
        }
    }

    pub fn solve(&self, ctx: &mut Context) -> Type {
        if let Some(typ) = ctx.global.alias.get(self) {
            return typ.clone();
        }
        match self {
            Type::Function(typ, ret, Some(args)) => Type::Function(
                typ.clone(),
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
