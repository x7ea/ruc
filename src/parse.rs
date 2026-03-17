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
                    .iter().map(|x| Name::new(&x))
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
        let token = source.trim();
        if let Some(token) = token.strip_prefix("let ") {
            if let Ok((name, value)) = once!(token, "=") {
                Ok(Expr::Let(Box::new(Expr::parse(&name)?), Box::new(Expr::parse(&value)?)))
            } else {
                Ok(Expr::Let(Box::new(Expr::parse(token)?), Box::new(Expr::Undefined)))
            }
        } else if let Some(token) = token.strip_prefix("if ") {
            if let Ok((cond, body)) = once!(token, "then") {
                if let Ok((then, els)) = once!(&body, "else") {
                    Ok(Expr::If(Box::new(Expr::parse(&cond)?), Box::new(Expr::parse(&then)?), Some(Box::new(Expr::parse(&els)?))))
                } else {
                    Ok(Expr::If(Box::new(Expr::parse(&cond)?), Box::new(Expr::parse(&body)?), None))
                }
            } else {
                Err(format!("parse \"if\" but \"then\" not found: {source}"))
            }
        } else if let Some(token) = token.strip_prefix("while ") {
            if let Ok((cond, body)) = once!(token, "do") {
                Ok(Expr::While(Box::new(Expr::parse(&cond)?), Box::new(Expr::parse(&body)?)))
            } else {
                Err(format!("parse \"while\" but \"do\" not found: {source}"))
            }
        } else if let Some(token) = token.strip_prefix("{").and_then(|token| token.strip_suffix("}")) {
            let mut block = vec![];
            for line in tokenize(token, "\n")? {
                let (line, _) = once!(&line, ";").unwrap_or((line, String::new()));
                if !line.trim().is_empty() { block.push(Expr::parse(&line)?); }
            }
            Ok(Expr::Block(block))
        } 
        else if token == "break" { Ok(Expr::Break(Box::new(Expr::Undefined))) }
        else if token == "return" { Ok(Expr::Return(Box::new(Expr::Undefined))) } 
        else if let Some(token) = token.strip_prefix("break ") { Ok(Expr::Break(Box::new(Expr::parse(&token)?))) } 
        else if let Some(token) = token.strip_prefix("return ") { Ok(Expr::Return(Box::new(Expr::parse(&token)?))) } 
        
        else if let Ok(operator) = parse_oprator(token) { Ok(operator) }
        else if let Some(ptr) = token.strip_prefix("*") { Ok(Expr::Derefer(Box::new(Expr::parse(ptr)?))) } 
        else if let Some(ptr) = token.strip_prefix("&") {
            if let Ok(name) = Name::new(ptr) { Ok(Expr::Pointer(name)) } 
            else if let Ok(Expr::Derefer(ptr)) = Expr::parse(ptr) { Ok(*ptr.clone()) } 
            else { Err(format!("invalid reference: {ptr}")) }
        } 
        else if token.starts_with("\"") && token.ends_with("\"") { Ok(Expr::String(token.to_owned())) } 
        else if let Some(expr) = token.strip_prefix("(").and_then(|x| x.strip_suffix(")")) { Expr::parse(expr) } 
        else if let (true, Some(expr)) = (token.contains("("), token.strip_suffix(")")) {
            let (name, args) = ok!(expr.split_once("("))?;
            let args = tokenize(&args, ",")?.iter().map(|x| Expr::parse(x));
            Ok(Expr::Call(Box::new(Expr::parse(&name)?), args.collect::<Result<Vec<_>, String>>()?))
        } 
        else if let (true, Some(expr)) = (token.contains("["), token.strip_suffix("]")) {
            let (arr, idx) = ok!(expr.rsplit_once("["))?;
            let offset = Expr::Mul(Box::new(Expr::parse(idx)?), Box::new(Expr::Integer(8)));
            Ok(Expr::Derefer(Box::new(Expr::Add(Box::new(Expr::parse(arr)?), Box::new(offset)))))
        } 
        else if let Ok(literal) = token.parse::<i64>() { Ok(Expr::Integer(literal)) }
        else if let Ok(literal) = token.parse::<bool>() { Ok(Expr::Integer(if literal { 1 } else { 0 })) } 
        else { Ok(Expr::Variable(Name::new(token)?)) }
    }
}

fn parse_oprator(source: &str) -> Result<Expr, String> {
    let tokens: Vec<String> = tokenize(source, SPACE)?;

    let op = ok!(tokens.len().checked_sub(2))?;
    let lhs_term = &ok!(tokens.get(..op))?.join(SPACE);
    let rhs_term = &ok!(tokens.get(op + 1..))?.join(SPACE);

    let op = ok!(tokens.get(op))?.as_str();
    let lhs = Box::new(Expr::parse(lhs_term)?);
    let rhs = Box::new(Expr::parse(rhs_term)?);

    Ok(match op {
        "+" => Expr::Add(lhs, rhs),
        "-" => Expr::Sub(lhs, rhs),
        "*" => Expr::Mul(lhs, rhs),
        "/" => Expr::Div(lhs, rhs),
        "%" => Expr::Mod(lhs, rhs),
        "==" => Expr::Eql(lhs, rhs),
        "!=" => Expr::NotEq(lhs, rhs),
        ">" => Expr::Gt(lhs, rhs),
        "<" => Expr::Lt(lhs, rhs),
        ">=" => Expr::GtEq(lhs, rhs),
        "<=" => Expr::LtEq(lhs, rhs),
        "&" => Expr::And(lhs, rhs),
        "|" => Expr::Or(lhs, rhs),
        "^" => Expr::Xor(lhs, rhs),
        op => return Err(format!("unknown operator: {op}")),
    })
}
