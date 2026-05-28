use crate::*;
use std::fmt::{self, Display};

pub const SPACE: &str = " ";

impl Define {
    pub fn parse(src: &str) -> Result<Vec<Define>, String> {
        let src = src.trim().to_string();
        let mut result = Vec::new();
        for line in lexer(&src, "\n")? {
            macro_rules! args {
                ($args: expr) => {{
                    let mut map = IndexMap::new();
                    for arg in lexer($args, ",")? {
                        if arg.trim().is_empty() {
                            continue;
                        }
                        let (name, typ) = once!(&arg, ":")?;
                        map.insert(Name::new(&name)?, Type::parse(&typ)?);
                    }
                    map
                }};
            }
            if let Some(func) = line.strip_prefix("fn ") {
                let (head, body) = once!(func, SPACE)?;
                let (name, args) = surround!(&head, "(", ")")?;
                let func = Define::Function(
                    Generics::parse(&name)?,
                    args!(&args),
                    Expr::Block(vec![Expr::parse(&body)?]),
                );
                result.push(func);
            } else if let Some(head) = line.strip_prefix("struct ") {
                let (name, args) = surround!(&head, "{", "}")?;
                result.push(Define::Class(
                    Generics::parse(&name)?,
                    Object::Struct(args!(&args)),
                ));
            } else if let Some(head) = line.strip_prefix("enum ") {
                let (name, args) = ok!(surround!("{", "}", &head))?;
                result.push(Define::Class(
                    Generics::parse(name)?,
                    Object::Enum(args!(&args)),
                ));
            }
        }
        Ok(result)
    }
}

impl Expr {
    pub fn parse(src: &str) -> Result<Expr, String> {
        let src = src.trim();
        fn is_operator(src: &str) -> Result<(String, String, String), String> {
            let tokens: Vec<String> = lexer(src, SPACE)?;
            if tokens.len() >= 3 {
                let pos: usize = tokens.len() - 2;
                let lhs = tokens[..pos].join(SPACE);
                let opr = tokens[pos].to_string();
                let rhs = tokens[pos + 1].to_string();
                Ok((lhs, opr, rhs))
            } else {
                Err(String::new())
            }
        }

        if let Some(x) = src.strip_prefix("print ") {
            Ok(Expr::Print(map!(lexer(x, ",")? => |i| Expr::parse(i))))
        } else if let Some(x) = src.strip_prefix("let ") {
            if let Ok((name, value)) = once!(x, "=") {
                Ok(Expr::Let(
                    Box::new(Expr::parse(&name)?),
                    Box::new(Expr::parse(&value)?),
                ))
            } else {
                let (name, typ) = once!(x, ":")?;
                Ok(Expr::Let(
                    Box::new(Expr::parse(&name)?),
                    Box::new(Expr::Null(Type::parse(&typ)?)),
                ))
            }
        } else if let Some(x) = src.strip_prefix("if ") {
            let (cond, body) = once!(x, "then")?;
            if let Ok((then, r#else)) = once!(&body, "else") {
                Ok(Expr::If(
                    Box::new(Expr::parse(&cond)?),
                    Box::new(Expr::parse(&then)?),
                    Some(Box::new(Expr::parse(&r#else)?)),
                ))
            } else {
                Ok(Expr::If(
                    Box::new(Expr::parse(&cond)?),
                    Box::new(Expr::parse(&body)?),
                    None,
                ))
            }
        } else if let Some(x) = src.strip_prefix("while ") {
            let (cond, body) = once!(x, "do")?;
            Ok(Expr::While(
                Box::new(Expr::parse(&cond)?),
                Box::new(Expr::parse(&body)?),
            ))
        } else if let Some(class) = src.strip_prefix("new ") {
            if let Some(arr) = surround!("[", class, "]") {
                let (typ, len) = ok!(arr.rsplit_once(";"))?;
                let Ok(len) = len.trim().parse::<usize>() else {
                    return Err(format!("not length: {len}"));
                };
                Ok(Expr::Array(Type::parse(typ)?, len))
            } else {
                Ok(Expr::New(Type::parse(class)?))
            }
        } else if let Some(x) = surround!("{", src, "}") {
            let mut block = vec![];
            for line in lexer(x, "\n")? {
                let (line, _) = once!(&line, ";").unwrap_or((line, String::new()));
                if !line.trim().is_empty() {
                    block.push(Expr::parse(&line)?);
                }
            }
            Ok(Expr::Block(block))
        } else if let Ok((lhs, op, rhs)) = is_operator(src) {
            let lhs = Box::new(Expr::parse(&lhs)?);
            let rhs = Box::new(Expr::parse(&rhs)?);
            Ok(match op.as_str() {
                "+" => Expr::Add(lhs, rhs),
                "-" => Expr::Sub(lhs, rhs),
                "*" => Expr::Mul(lhs, rhs),
                "/" => Expr::Div(lhs, rhs),
                "%" => Expr::Mod(lhs, rhs),
                "==" => Expr::Eql(lhs, rhs),
                "!=" => Expr::NotEq(lhs, rhs),
                "&" => Expr::And(lhs, rhs),
                "|" => Expr::Or(lhs, rhs),
                "^" => Expr::Xor(lhs, rhs),
                ">" => Expr::Gt(lhs, rhs),
                "<" => Expr::Lt(lhs, rhs),
                ">=" => Expr::GtEq(lhs, rhs),
                "<=" => Expr::LtEq(lhs, rhs),
                op => return Err(format!("unknown operator: {op}")),
            })
        } else if src == "()" {
            Ok(Expr::Null(Type::None))
        } else if let Some(x) = surround!("\"", src, "\"") {
            Ok(Expr::String(x.to_owned()))
        } else if let Some(expr) = surround!("(", src, ")") {
            Expr::parse(expr)
        } else if let Some(arr) = surround!("[", src, "]") {
            let arr = map!(lexer(arr, ",")? => |x| Expr::parse(x));
            Ok(Expr::Sequence(arr))
        } else if let Ok((func, args)) = surround!(src, "(", ")") {
            Ok(Expr::Call(
                Box::new(Expr::parse(&func)?),
                map!(lexer(&args, ",")? => |x| Expr::parse(x)),
            ))
        } else if let Ok((arr, idx)) = surround!(src, "[", "]") {
            Ok(Expr::Index(
                Box::new(Expr::parse(&arr)?),
                Box::new(Expr::parse(&idx)?),
            ))
        } else if let Ok(literal) = src.parse::<bool>() {
            Ok(Expr::Bool(literal))
        } else if let Ok(literal) = src.parse::<i64>() {
            Ok(Expr::Integer(literal))
        } else if let Ok(literal) = src.parse::<f64>() {
            use ordered_float::OrderedFloat;
            Ok(Expr::Float(OrderedFloat(literal)))
        } else if let Some(class) = src.strip_suffix("?") {
            Ok(Expr::Check(Box::new(Expr::parse(class)?)))
        } else if let Some((obj, key)) = src.rsplit_once(".") {
            Ok(Expr::Member(Box::new(Expr::parse(obj)?), Name::new(key)?))
        } else {
            Ok(Expr::Variable(Generics::parse(src)?))
        }
    }
}

impl Type {
    pub fn parse(src: &str) -> Result<Type, String> {
        match src.trim() {
            "Int" => Ok(Type::Integer),
            "Str" => Ok(Type::String),
            "Bool" => Ok(Type::Bool),
            "Float" => Ok(Type::Float),
            "()" => Ok(Type::None),
            x => {
                if let Ok((func, args)) = surround!(x, "(", ")") {
                    Ok(Type::Function(
                        vec![],
                        Box::new(Type::parse(&func)?),
                        Some(map!(lexer(&args, ",")? => |x| Type::parse(x))),
                    ))
                } else if let Some(arr) = surround!("[", x, "]") {
                    Ok(Type::Array(Box::new(Type::parse(arr)?)))
                } else {
                    Ok(Type::Class(Generics::parse(x)?))
                }
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "Int"),
            Type::String => write!(f, "Str"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::None => write!(f, "()"),
            Type::Array(typ) => write!(f, "[{typ}]"),
            Type::Class(Generics(name, args)) if args.is_empty() => write!(f, "{name}"),
            Type::Class(Generics(name, args)) => {
                let args = map!(args, |x| x.to_string()).join(", ");
                write!(f, "{name}<{args}>")
            }
            Type::Function(_, ret, Some(args)) => {
                let args = map!(args, |x| x.to_string()).join(", ");
                write!(f, "{ret}({args})")
            }
            Type::Function(_, ret, None) => write!(f, "{ret}()"),
        }
    }
}

impl Generics {
    pub fn parse(src: &str) -> Result<Generics, String> {
        let x = src.trim();
        if let Some((var, args)) = surround!("<", ">", x) {
            Ok(Generics(
                Name::new(var)?,
                map!(lexer(args, ",")? => |x| Type::parse(x)),
            ))
        } else {
            Ok(Generics(Name::new(x)?, vec![]))
        }
    }
}
