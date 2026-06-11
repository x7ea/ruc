use crate::*;
impl Expr {
    pub fn expand(&self, ctx: &mut Context) -> Result<Expr, String> {
        macro_rules! temp {
            ($typ: expr) => {{
                let name = Name::new(&format!("temp{}", ctx.label()))?;
                Expr::Variable(Generics(
                    Generics(name, vec![$typ.clone()]).generics(),
                    Vec::new(),
                ))
            }};
        }
        match self.clone() {
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
                Ok(expr)
            }
            Expr::Enum(typ, key, val) => {
                let temp = Box::new(temp!(typ.clone()));
                Ok(Expr::Block(vec![
                    Expr::Let(temp.clone(), Box::new(Expr::New(typ.clone()))),
                    Expr::Let(
                        Box::new(Expr::Member(temp.clone(), key.clone())),
                        val.clone(),
                    ),
                    *temp,
                ]))
            }
            Expr::For(cnt, arr, body) => {
                let temp = Box::new(temp!(Type::Integer));
                let read = Box::new(Expr::Index(arr.clone(), temp.clone()));
                let inc = Box::new(Expr::Add(temp.clone(), Box::new(Expr::Integer(1))));
                let body = [Expr::Let(cnt, read), *body, Expr::Let(temp.clone(), inc)];
                Ok(Expr::Block(vec![
                    Expr::Let(temp.clone(), Box::new(Expr::Integer(0))),
                    Expr::While(
                        Box::new(Expr::Lt(temp.clone(), len!(arr))),
                        Box::new(Expr::Block(body.to_vec())),
                    ),
                ]))
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
                Ok(Expr::Block(expr))
            }
            Expr::Clone(expr) => {
                let typ = expr.infer(ctx)?;
                let dest = Box::new(temp!(typ));
                Ok(Expr::Block(vec![
                    Expr::Let(dest.clone(), Box::new(Expr::New(typ.clone()))),
                    Expr::Call(
                        Box::new(Expr::Variable(Generics(Name::new("memcpy")?, vec![]))),
                        vec![*dest.clone(), *expr, Expr::Integer(typ.size(ctx)? as i64)],
                    ),
                    *dest.clone(),
                ]))
            }
            _ => Ok(self.clone()),
        }
    }
}
