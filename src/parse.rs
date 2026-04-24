use crate::*;

pub const SPACE: &str = " ";

impl Define {
    pub fn parse(source: &str) -> Result<Vec<Define>, String> {
        let mut result = Vec::new();
        for line in tokenize(source, "\n")? {
            if let Some(func) = line.strip_prefix("fn ") {
                let (head, body) = ok!(func.split_once(")"))?;
                let (name, args) = ok!(head.split_once("("))?;
                let args = tokenize(args, ",")?
                    .iter()
                    .map(|x| Name::new(&x))
                    .collect::<Result<IndexSet<Name>, String>>()?;
                let body = Expr::parse(body)?;
                result.push(Define(Name::new(name)?, args, body));
            }
        }
        Ok(result)
    }
}

impl Expr {
    pub fn parse(source: &str) -> Result<Expr, String> {
        let x = source.trim();
        macro_rules! surround {
            ($ls: literal, $x: expr, $rs: literal) => {
                $x.strip_prefix($ls).and_then(|x| x.strip_suffix($rs))
            };
            ($x: expr, $ls: literal, $rs: literal) => {
                tokenize(x, &$ls).and_then(|x| {
                    if x.len() < 2 {
                        return Err(format!("not surrounded"));
                    }
                    let args = ok!(x.last())?.to_string();
                    let func = ok!(x.get(..x.len() - 1))?
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<String>();

                    let args = ok!(args.get(1..args.len() - 1))?.to_string();
                    Ok((func, args))
                })
            };
        }

        type Operator = (Box<Expr>, String, Box<Expr>);
        fn is_operator(source: &str) -> Result<Operator, String> {
            let tokens: Vec<String> = tokenize(source, SPACE)?;

            let op = ok!(tokens.len().checked_sub(2))?;
            let lhs = &ok!(tokens.get(..op))?.join(SPACE);
            let rhs = &ok!(tokens.get(op + 1..))?.join(SPACE);

            let op = ok!(tokens.get(op))?.to_owned();
            let lhs = Box::new(Expr::parse(lhs)?);
            let rhs = Box::new(Expr::parse(rhs)?);
            Ok((lhs, op, rhs))
        }

        if let Some(x) = x.strip_prefix("let ") {
            if let Ok((name, value)) = once!(x, "=") {
                Ok(Expr::Let(
                    Box::new(Expr::parse(&name)?),
                    Box::new(Expr::parse(&value)?),
                ))
            } else {
                Ok(Expr::Let(
                    Box::new(Expr::parse(x)?),
                    Box::new(Expr::Undefined),
                ))
            }
        } else if let Some(x) = x.strip_prefix("if ") {
            if let Ok((cond, body)) = once!(x, "then") {
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
            } else {
                Err(format!("parse \"if\" but \"then\" not found: {source}"))
            }
        } else if let Some(x) = x.strip_prefix("while ") {
            if let Ok((condition, loop_body)) = once!(x, "do") {
                Ok(Expr::While(
                    Box::new(Expr::parse(&condition)?),
                    Box::new(Expr::parse(&loop_body)?),
                ))
            } else {
                Err(format!("parse \"while\" but \"do\" not found: {source}"))
            }
        } else if let Some(x) = surround!("{", x, "}") {
            let mut block = vec![];
            for line in tokenize(x, "\n")? {
                let (line, _) = once!(&line, ";").unwrap_or((line, String::new()));
                if !line.trim().is_empty() {
                    block.push(Expr::parse(&line)?);
                }
            }
            Ok(Expr::Block(block))
        } else if x == "break" {
            Ok(Expr::Break(Box::new(Expr::Undefined)))
        } else if x == "return" {
            Ok(Expr::Return(Box::new(Expr::Undefined)))
        } else if let Some(x) = x.strip_prefix("break ") {
            Ok(Expr::Break(Box::new(Expr::parse(&x)?)))
        } else if let Some(x) = x.strip_prefix("return ") {
            Ok(Expr::Return(Box::new(Expr::parse(&x)?)))
        } else if let Ok((lhs, op, rhs)) = is_operator(x) {
            match op.as_str() {
                "+" => Ok(Expr::Add(lhs, rhs)),
                "-" => Ok(Expr::Sub(lhs, rhs)),
                "*" => Ok(Expr::Mul(lhs, rhs)),
                "/" => Ok(Expr::Div(lhs, rhs)),
                "%" => Ok(Expr::Mod(lhs, rhs)),
                "==" => Ok(Expr::Eql(lhs, rhs)),
                "!=" => Ok(Expr::NotEq(lhs, rhs)),
                ">" => Ok(Expr::Gt(lhs, rhs)),
                "<" => Ok(Expr::Lt(lhs, rhs)),
                ">=" => Ok(Expr::GtEq(lhs, rhs)),
                "<=" => Ok(Expr::LtEq(lhs, rhs)),
                "&" => Ok(Expr::And(lhs, rhs)),
                "|" => Ok(Expr::Or(lhs, rhs)),
                "^" => Ok(Expr::Xor(lhs, rhs)),
                op => Err(format!("unknown operator: {op}")),
            }
        } else if let Some(pointer) = x.strip_prefix("*") {
            Ok(Expr::Derefer(Box::new(Expr::parse(pointer)?)))
        } else if let Some(ptr) = x.strip_prefix("&") {
            if let Ok(name) = Name::new(ptr) {
                Ok(Expr::Pointer(name))
            } else if let Ok(Expr::Derefer(ptr)) = Expr::parse(ptr) {
                Ok(*ptr.clone())
            } else {
                Err(format!("invalid reference: {ptr}"))
            }
        } else if let Some(_) = surround!("\"", x, "\"") {
            Ok(Expr::String(x.to_owned()))
        } else if let Some(expr) = surround!("(", x, ")") {
            Expr::parse(expr)
        } else if let Ok((func, args)) = surround!(x, "(", ")") {
            Ok(Expr::Call(
                Box::new(Expr::parse(&func)?),
                tokenize(&args, ",")?
                    .iter()
                    .map(|x| Expr::parse(x))
                    .collect::<Result<Vec<_>, String>>()?,
            ))
        } else if let Ok((arr, idx)) = surround!(x, "[", "]") {
            let offset = Expr::Mul(Box::new(Expr::parse(&idx)?), Box::new(Expr::Integer(8)));
            Ok(Expr::Derefer(Box::new(Expr::Add(
                Box::new(Expr::parse(&arr)?),
                Box::new(offset),
            ))))
        } else if let Ok(literal) = x.parse::<i64>() {
            Ok(Expr::Integer(literal))
        } else if let Ok(literal) = x.parse::<bool>() {
            Ok(Expr::Integer(if literal { 1 } else { 0 }))
        } else {
            Ok(Expr::Variable(Name::new(x)?))
        }
    }
}
