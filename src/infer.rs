use crate::*;

impl Expr {
    pub fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
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
        macro_rules! temp {
            ($typ: expr) => {{
                let name = Name::new(&format!("temp{}", ctx.label()))?;
                Expr::Variable(Generics(
                    Generics(name, vec![$typ.clone()]).generics(),
                    Vec::new(),
                ))
            }};
        }
        macro_rules! op {
            ($typ: pat, $lhs: expr, $rhs: expr) => {{
                let [lt, rt] = [$lhs.infer(ctx)?, $rhs.infer(ctx)?];
                if lt != rt {
                    return Err(format!("operator term: {lt} != {rt}"));
                }
                let $typ = lt else {
                    return Err(format!("no operation: {lt}"));
                };
                typing!(lt.clone())
            }};
            ($typ: pat, $lhs: expr, $rhs: expr, $ret: expr) => {{
                op!($typ, $lhs, $rhs)?;
                typing!($ret.clone())
            }};
        }
        macro_rules! get {
            ($name:ident, $obj: expr) => {{
                let Type::$name(class) = $obj else { panic!() };
                class.clone()
            }};
        }
        match self.clone() {
            Expr::Print(is_output, vals) => {
                let mut fmt = String::new();
                let mut name = "g_strdup_printf";
                for i in vals.iter() {
                    let typ = i.infer(ctx)?;
                    fmt += match typ {
                        Type::Integer => "%ld",
                        Type::String => "%s",
                        Type::Float => "%g",
                        _ => return Err(format!("can't print: {typ}")),
                    }
                }
                if is_output { fmt += "\\n"; name = "printf"; }
                expand!(Expr::Call(
                    Box::new(Expr::Variable(Generics(Name::new(name)?, vec![]))),
                    [vec![Expr::String(fmt)], vals.to_vec()].concat(),
                ));
                typing!(if is_output { Type::Void } else { Type::String })
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
                if cond != Type::Boolean {
                    return Err(format!("if-else test: Bool != {cond}"));
                }
                if let Some(els) = els {
                    let [then, els] = [then.infer(ctx)?, els.infer(ctx)?];
                    if els != Type::Void && then != els {
                        return Err(format!("if-else term: {then} != {els}"));
                    }
                    typing!(then.clone())
                } else {
                    then.infer(ctx)?;
                    Ok(Type::Void)
                }
            }
            Expr::While(cond, body) => {
                if let Expr::Let(bind, check) = *cond {
                    return typing!(expands!(Expr::While(
                        Box::new(Expr::Check(check.clone())),
                        Box::new(Expr::Block(vec![Expr::Let(bind, check), *body])),
                    )));
                }
                let cond = cond.infer(ctx)?;
                if cond != Type::Boolean {
                    return Err(format!("while-do test: Bool != {cond}"));
                }
                body.infer(ctx)
            }
            Expr::Match(val, pats) => {
                let mut expr = Expr::Null(Type::Void);
                for (key, bind, ret) in pats {
                    let acc = Box::new(Expr::Member(val.clone(), key.clone()));
                    expr = Expr::If(
                        if let Some(bind) = bind {
                            Box::new(Expr::Let(Box::new(bind.clone()), acc))
                        } else {
                            Box::new(Expr::Check(acc))
                        },
                        Box::new(ret.clone()),
                        Some(Box::new(expr)),
                    )
                }
                typing!(expands!(expr))
            }
            Expr::Enum(typ, key, val) => {
                let temp = Box::new(temp!(typ.clone()));
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(temp.clone(), Box::new(Expr::New(typ.clone()))),
                    Expr::Let(
                        Box::new(Expr::Member(temp.clone(), key.clone())),
                        val.clone(),
                    ),
                    *temp
                ])))
            }
            Expr::For(cnt, arr, body) => {
                let temp = Box::new(temp!(Type::Integer));
                let read = Box::new(Expr::Index(arr.clone(), temp.clone()));
                let inc = Box::new(Expr::Add(temp.clone(), Box::new(Expr::Integer(1))));
                let body = [Expr::Let(cnt, read), *body, Expr::Let(temp.clone(), inc)];
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(temp.clone(), Box::new(Expr::Integer(0))),
                    Expr::While(
                        Box::new(Expr::Lt(temp.clone(), len!(arr))),
                        Box::new(Expr::Block(body.to_vec()))
                    ),
                ])))
            }
            Expr::Sequence(array) => {
                let typ = array[0].infer(ctx)?;
                let temp = temp!(typ.clone());
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
            Expr::Block(lines) => {
                let mut ret = Type::Void;
                let parent = ctx.local.scope.clone();
                for line in lines {
                    ret = line.infer(ctx)?;
                }
                for (name, val) in &ctx.local.scope {
                    if let Some(typ) = ctx.local.var.get(name) && typ != val {
                        return Err(format!("duplicated {name}: {typ} != {val}"));
                    }
                    ctx.local.var.insert(name.clone(), val.clone());
                }
                ctx.local.scope = parent;
                typing!(ret.clone())
            }
            Expr::Call(callee, args) => {
                if let Some(obj) = args.first() && let Type::Class(name) = obj.infer(ctx)? {
                    ctx.local.class = Some(name.0);
                }
                let typ = callee.infer(ctx)?;
                if let Type::Function(_, ret, params) = typ {
                    let Some(params) = params else {
                        for arg in args { arg.infer(ctx)?; }
                        return typing!(*ret.clone());
                    };
                    let (pl, al) = (params.len(), args.len());
                    if pl != al {
                        return Err(format!("length: {pl} != {al}"));
                    }
                    for (param, arg) in params.iter().zip(args) {
                        if param == &Type::Void { continue; }
                        let arg = arg.infer(ctx)?.solve(ctx);
                        if arg != param.solve(ctx) {
                            return Err(format!("arguments: {param} != {arg}"));
                        }
                    }
                    typing!(*ret.clone())
                } else {
                    Err(format!("not callee: {typ}"))
                }
            }
            Expr::Variable(generics) => {
                let Generics(name, args) = generics.clone();
                if let Some(obj) = ctx.local.class.clone() {
                    ctx.local.class = None;
                    let name = Name::new(&format!("{obj}.{name}"))?;
                    if ctx.global.lib.contains_key(&name) {
                        return typing!(expands!(Expr::Variable(Generics(name, args.clone()))));
                    }
                }
                if let Some(typ) = ctx.global.lib.get(&name) {
                    typing!(typ.clone().mono(ctx, generics)?)
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
                    typing!(Type::Void)
                }
                acc @ Expr::Index(arr, idx) => {
                    {
                        let [val, typ] = [val.infer(ctx)?, acc.infer(ctx)?];
                        if typ.clone() != val {
                            return Err(format!("array[n] {typ} != {val}"));
                        }
                    }
                    expand!(Expr::Write(array!(arr, idx), val.clone(), arr.clone()));
                    typing!(Type::Void)
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
                    typing!(Type::Void)
                }
                other => Err(format!("not assign target: {}", other.infer(ctx)?)),
            },
            Expr::New(typ) => {
                let Type::Class(_) = typ.clone() else {
                    return Err(format!("no constructor: {typ}"));
                };
                let typ = typ.mono(ctx, Generics::default())?;
                expand!(new!(typ.size(ctx)? / 8));
                typing!(typ.solve(ctx))
            }
            Expr::Member(obj, key) if key == Name::new("len")? => {
                let typ = obj.infer(ctx)?;
                let Type::Array(_) = typ.clone() else {
                    return Err(format!("no length: {typ}"));
                };
                typing!(expands!(Expr::Read(
                    Box::new(Expr::Integer(0)),
                    Type::Integer,
                    obj.clone()
                )))
            }
            Expr::Member(obj, key) => {
                let typ = obj.infer(ctx)?;
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
            Expr::Check(expr) => {
                if let Expr::Member(obj, key) = &*expr
                    && let Type::Class(Generics(name, _)) = &obj.infer(ctx)?
                    && let Some((_, Object::Enum(layout))) = ctx.global.table.get(name)
                {
                    let Some(tag) = layout.get_index_of(key) else {
                        return Err(format!("undefined: {name}.{key}"));
                    };
                    let offset = Box::new(Expr::Integer(0));
                    expand!(Expr::Eql(
                        Box::new(Expr::Read(offset, Type::Integer, obj.clone())),
                        Box::new(Expr::Integer(tag as i64)),
                    ));
                    return typing!(Type::Boolean);
                }
                let typ = expr.infer(ctx)?;
                let Type::Class(_) = typ else {
                    return Err(format!("not nullable: {typ}"));
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
                let dest = Box::new(temp!(typ));
                typing!(expands!(Expr::Block(vec![
                    Expr::Let(dest.clone(), Box::new(Expr::New(typ.clone()))),
                    Expr::Call(
                        Box::new(Expr::Variable(Generics(Name::new("memcpy")?, vec![]))),
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
                let typ = typ.solve(ctx);
                expand!(Expr::Block(vec![
                    Expr::Float(Float::from(0.0)),
                    Expr::Integer(0)
                ]));
                typing!(typ)
            }
            Expr::Integer(_) => typing!(Type::Integer),
            Expr::Float(_) => typing!(Type::Float),
            Expr::String(_) => typing!(Type::String),
            Expr::Bool(val) => {
                expand!(Expr::Integer(if val { 1 } else { 0 }));
                typing!(Type::Boolean)
            }
            Expr::Add(lhs, rhs)
            | Expr::Sub(lhs, rhs)
            | Expr::Mul(lhs, rhs)
            | Expr::Div(lhs, rhs)
            => op!((Type::Integer | Type::Float), lhs, rhs),
            Expr::Eql(lhs, rhs)
            | Expr::NotEq(lhs, rhs)
            | Expr::Gt(lhs, rhs)
            | Expr::Lt(lhs, rhs)
            | Expr::GtEq(lhs, rhs)
            | Expr::LtEq(lhs, rhs)
            => op!(Type::Integer, lhs, rhs, Type::Boolean),
            Expr::And(lhs, rhs)
            | Expr::Or(lhs, rhs)
            | Expr::Xor(lhs, rhs)
            => op!(Type::Boolean, lhs, rhs)
        }
    }
}