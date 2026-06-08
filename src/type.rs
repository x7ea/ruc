use crate::*;

impl Define {
    pub fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        macro_rules! types {
            ($args: expr) => {
                Some($args.values().cloned().collect::<Vec<Type>>())
            };
        }
        match self {
            Define::Function(Generics(name, param), args, (Some(body), Some(ret))) => {
                let sig = Type::Function(param.clone(), Box::new(ret.clone()), types!(args));
                ctx.global.lib.insert(name.clone(), sig.clone());
                let parent = ctx.local.clone();
                ctx.local = Function::default();
                ctx.local.scope = args.clone();
                if *ret != body.infer(ctx)? {
                    return Err(format!("expected: returns {ret}"));
                }
                ctx.table.insert(name.clone(), ctx.local.clone());
                ctx.local = parent;
                Ok(sig)
            }
            Define::Function(Generics(name, param), args, (Some(body), None)) => {
                if !param.is_empty() {
                    let sig = Type::Function(param.clone(), Box::new(Type::Void), types!(args));
                    ctx.global.lib.insert(name.clone(), sig.clone());
                    return Ok(sig);
                }
                let parent = ctx.local.clone();
                ctx.local = Function::default();
                ctx.local.scope = args.clone();
                let ret = Box::new(body.infer(ctx)?);
                let sig = Type::Function(param.clone(), ret, types!(args));
                ctx.table.insert(name.clone(), ctx.local.clone());
                ctx.global.lib.insert(name.clone(), sig.clone());
                ctx.local = parent;
                Ok(sig)
            }
            Define::Function(Generics(name, param), args, (None, Some(ret))) => {
                let sig = Type::Function(param.clone(), Box::new(ret.clone()), types!(args));
                ctx.table.insert(name.clone(), ctx.local.clone());
                ctx.global.lib.insert(name.clone(), sig.clone());
                ctx.global.extrn.insert(name.clone());
                Ok(sig)
            }
            Define::Class(Generics(name, args), layout) => {
                let val = (args.clone(), layout.clone());
                ctx.global.table.insert(name.clone(), val);
                Ok(Type::Void)
            }
            _ => panic!(),
        }
    }
}
impl Type {
  pub  fn mono(self, ctx: &mut Context, func: Generics) -> Result<Type, String> {
        let mut typ = self.solve(ctx);
        let Generics(name, mut args) = func.clone();
        for arg in args.iter_mut() {
            *arg = arg.solve(ctx);
        }
        match typ.clone() {
            Type::Function(params, _, _) if !params.is_empty() => {
                let mut alias = IndexMap::new();
                for (arg, param) in args.iter().zip(&params) {
                    alias.insert(param.clone(), arg.clone());
                    typ = self.rewrite(param, arg);
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
                    typ = unify.infer(ctx)?;
                }
                ctx.global.alias = parent;
                ctx.global.def.insert(mangle, unify.clone());
            }
            Type::Class(Generics(name, args)) => {
                let Some((params, table)) = ctx.global.table.get(&name) else {
                    return Err(format!("undefined: {name}"));
                };
                let layout = {
                    let (Object::Enum(layout) | Object::Struct(layout)) = &table;
                    let mut layout = layout.clone();
                    for (key, field) in layout.clone() {
                        for (arg, param) in args.iter().zip(params) {
                            let field = field.rewrite(param, arg);
                            layout.insert(key.clone(), field.clone());
                        }
                    }
                    layout
                };
                let unify = match table {
                    Object::Enum(_) => Object::Enum(layout).clone(),
                    Object::Struct(_) => Object::Struct(layout).clone(),
                };
                let mangle = Generics(name.clone(), args).generics();
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
            Type::Function(typ, ret, Some(args)) => Type::Function(
                typ.clone(), Box::new(ret.rewrite(old, new)),
                Some(map!(args, |x| x.rewrite(old, new))),
            ),
            Type::Class(Generics(name, args)) => Type::Class(
                Generics(name.clone(), 
                map!(args, |x| x.rewrite(old, new)))
            ), 
            Type::Array(typ) => Type::Array(Box::new(typ.rewrite(old, new))),
            _ => self.clone(),
        }
    }
    pub fn solve(&self, ctx: &mut Context) -> Type {
        let mut typ = self.clone();
        for (old, new) in &ctx.global.alias {
            typ = self.rewrite(old, new);
        }
        typ
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
}