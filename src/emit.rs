use crate::*;

impl Expr {
    pub fn ir(&self, ctx: &mut Context) -> Result<IR, String> {
        macro_rules! typ {
            ($expr: expr) => {
                ctx.local.typed.get($expr).unwrap().clone()
            };
        }
        macro_rules! expr {
            ($expr: expr) => {
                ctx.local.expand.get($expr).unwrap().clone()
            };
        }
        match self.clone() {
            Expr::Variable(Generics(name, _)) => {
                let id = ctx.local.var.get_index_of(&name).unwrap();
                if typ!(self) == Type::Float {
                    Ok(IR::FLocal(id))
                } else {
                    Ok(IR::ILocal(id))
                }
            }
        }
    }
}
