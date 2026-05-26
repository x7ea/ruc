use crate::*;
use std::fmt::{self, Display};

pub const SPACE: &str = " ";

impl Define {
    pub fn parse(source: &str) -> Result<Vec<Define>, String> {
        let mut result = Vec::new();
        for line in tokenize(source, "\n")? {
            macro_rules! args {
                ($args: expr) => {{
                    let mut map = IndexMap::new();
                    for arg in tokenize($args, ",")? {
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
                let (name, args) = ok!(ok!(head.strip_suffix(")"))?.split_once("("))?;
                let func = Define::Function(
                    Name::new(name)?,
                    args!(args),
                    Expr::Block(vec![Expr::parse(&body)?]),
                );
                result.push(func);
            } else if let Some(head) = line.strip_prefix("struct ") {
                let (name, args) = ok!(ok!(head.trim().strip_suffix("}"))?.split_once("{"))?;
                result.push(Define::Class(Name::new(name)?, Object::Struct(args!(args))));
            } else if let Some(head) = line.strip_prefix("enum ") {
                let (name, args) = ok!(ok!(head.trim().strip_suffix("}"))?.split_once("{"))?;
                result.push(Define::Class(Name::new(name)?, Object::Enum(args!(args))));
            }
        }
        Ok(result)
    }
}

macro_rules! surround {
    ($ls: literal, $x: expr, $rs: literal) => {
        $x.strip_prefix($ls).and_then(|x| x.strip_suffix($rs))
    };
    ($x: expr, $ls: literal, $rs: literal) => {
        tokenize($x, &$ls).and_then(|x| {
            if x.len() < 2 {
                return Err(String::new());
            }
            let args = ok!(x.last())?.to_string();
            let func = ok!(x.get(..x.len() - 1))?.concat();

            let args = ok!(args.get(1..args.len() - 1))?.to_string();
            Ok((func, args))
        })
    };
}

impl Expr {
    pub fn parse(source: &str) -> Result<Expr, String> {
        let x = source.trim();
        fn is_operator(source: &str) -> Result<(String, String, String), String> {
            let tokens: Vec<String> = tokenize(source, SPACE)?;
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
        if let Some(x) = x.strip_prefix("print ") {
            Ok(Expr::Print(map!(tokenize(x, ",")?, |i| Expr::parse(&i))))
        } else if let Some(x) = x.strip_prefix("let ") {
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
        } else if let Some(x) = x.strip_prefix("if ") {
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
        } else if let Some(x) = x.strip_prefix("while ") {
            let (cond, body) = once!(x, "do")?;
            Ok(Expr::While(
                Box::new(Expr::parse(&cond)?),
                Box::new(Expr::parse(&body)?),
            ))
        } else if let Some(x) = surround!("{", x, "}") {
            let mut block = vec![];
            for line in tokenize(x, "\n")? {
                let (line, _) = once!(&line, ";").unwrap_or((line, String::new()));
                if !line.trim().is_empty() {
                    block.push(Expr::parse(&line)?);
                }
            }
            Ok(Expr::Block(block))
        } else if let Ok((lhs, op, rhs)) = is_operator(x) {
            Ok(match op.as_str() {
                "+" => Expr::Add(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "-" => Expr::Sub(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "*" => Expr::Mul(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "/" => Expr::Div(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "%" => Expr::Mod(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "==" => Expr::Eql(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "!=" => Expr::NotEq(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                ">" => Expr::Gt(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "<" => Expr::Lt(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                ">=" => Expr::GtEq(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "<=" => Expr::LtEq(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "&" => Expr::And(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "|" => Expr::Or(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                "^" => Expr::Xor(Box::new(Expr::parse(&lhs)?), Box::new(Expr::parse(&rhs)?)),
                op => return Err(format!("unknown operator: {op}")),
            })
        } else if let Some(class) = x.strip_suffix("?") {
            Ok(Expr::Check(Box::new(Expr::parse(class)?)))
        } else if x == "()" {
            Ok(Expr::Null(Type::None))
        } else if let Some(x) = surround!("\"", x, "\"") {
            Ok(Expr::String(x.to_owned()))
        } else if let Some(expr) = surround!("(", x, ")") {
            Expr::parse(expr)
        } else if let Some(arr) = surround!("[", x, "]") {
            let (typ, len) = ok!(arr.rsplit_once(";"))?;
            let Ok(len) = len.trim().parse::<usize>() else {
                return Err(format!("not length: {len}"));
            };
            Ok(Expr::Array(Type::parse(&typ)?, len))
        } else if let Ok((func, args)) = surround!(x, "(", ")") {
            Ok(Expr::Call(
                Box::new(Expr::parse(&func)?),
                map!(tokenize(&args, ",")?, |x| Expr::parse(x)),
            ))
        } else if let Ok((arr, idx)) = surround!(x, "[", "]") {
            Ok(Expr::Index(
                Box::new(Expr::parse(&arr)?),
                Box::new(Expr::parse(&idx)?),
            ))
        } else if let Ok(literal) = x.parse::<bool>() {
            Ok(Expr::Bool(literal))
        } else if let Ok(literal) = x.parse::<i64>() {
            Ok(Expr::Integer(literal))
        } else if let Ok(literal) = x.parse::<f64>() {
            use ordered_float::OrderedFloat;
            Ok(Expr::Float(OrderedFloat(literal)))
        } else if let Some((obj, key)) = x.rsplit_once(".") {
            if key.trim() == "len" {
                return Ok(Expr::Len(Box::new(Expr::parse(obj)?)));
            }
            Ok(Expr::Member(Box::new(Expr::parse(obj)?), Name::new(key)?))
        } else if let Some(class) = x.strip_prefix("new ") {
            Ok(Expr::New(Type::parse(&class)?))
        } else if let Ok((var, args)) = surround!(x, "<", ">") {
            Ok(Expr::Variable(Generics(
                Name::new(&var)?,
                map!(tokenize(&args, ",")?, |x| Type::parse(x)),
            )))
        } else {
            Ok(Expr::Variable(Generics(Name::new(x)?, vec![])))
        }
    }
}

impl Type {
    pub fn parse(source: &str) -> Result<Type, String> {
        match source.trim() {
            "Int" => Ok(Type::Integer),
            "Str" => Ok(Type::String),
            "Bool" => Ok(Type::Bool),
            "Float" => Ok(Type::Float),
            "()" => Ok(Type::None),
            x => {
                if let Ok((func, args)) = surround!(x, "(", ")") {
                    Ok(Type::Function(
                        Box::new(Type::parse(&func)?),
                        Some(map!(tokenize(&args, ",")?, |x| Type::parse(&x))),
                    ))
                } else if let Some(arr) = surround!("[", x, "]") {
                    Ok(Type::Array(Box::new(Type::parse(&arr)?)))
                } else if let Ok((var, args)) = surround!(x, "<", ">") {
                    Ok(Type::Class(Generics(
                        Name::new(&var)?,
                        map!(tokenize(&args, ",")?, |x| Type::parse(x)),
                    )))
                } else {
                    Ok(Type::Class(Generics(Name::new(x)?, vec![])))
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
            Type::Class(name) => write!(f, "{name}"),
            Type::Function(ret, Some(arg)) => {
                let arg = map!(arg).join(", ");
                write!(f, "{ret}({arg})")
            }
            Type::Function(ret, None) => write!(f, "{ret}(...)"),
        }
    }
}
