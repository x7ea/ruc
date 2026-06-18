use crate::*;
use std::{
    fmt::{self, Display},
    fs::read_to_string,
};

pub const SPACE: &str = " ";

impl Define {
    pub fn parse(src: &str) -> Result<Vec<Define>, String> {
        let src = src.trim().replace("'\n", SPACE);
        let mut result = Vec::new();
        for line in lexer(&src, "\n")? {
            macro_rules! args {
                ($args: expr) => {{
                    let mut map = IndexMap::new();
                    for arg in lexer($args, ",")? {
                        let (name, typ) = once!(&arg, ":")?;
                        map.insert(Name::new(&name)?, Type::parse(&typ)?);
                    }
                    map
                }};
            }
            if let Some(file) = line.strip_prefix("use ") {
                let file = file.trim().to_string();
                let Ok(file) = read_to_string(format!("./lib/{file}.rca")) else {
                    return Err(format!("undefined library: {file}"));
                };
                result.append(&mut Define::parse(&file)?);
            } else if let Some(func) = line.strip_prefix("extern fn ") {
                let (head, body) = once!(func, ":")?;
                let (name, args) = surround!(&head, "(", ")")?;
                result.push(Define::Declare(
                    (Generics::parse(&name)?, args!(&args)),
                    Type::parse(&body)?,
                ));
            } else if let Some(func) = line.strip_prefix("fn ")
                && let Ok((head, body)) = once!(func, ":")
            {
                let (typ, body) = once!(&body, SPACE)?;
                let (name, args) = surround!(&head, "(", ")")?;
                result.push(Define::Function(
                    (Generics::parse(&name)?, args!(&args)),
                    (Expr::Block(vec![Expr::parse(&body)?]), Type::parse(&typ)?),
                ));
            } else if let Some(func) = line.strip_prefix("fn ") {
                let (head, body) = once!(&func, SPACE)?;
                let (name, args) = surround!(&head, "(", ")")?;
                result.push(Define::Function(
                    (Generics::parse(&name)?, args!(&args)),
                    (Expr::Block(vec![Expr::parse(&body)?]), Type::Void),
                ));
            } else if let Some(head) = line.strip_prefix("struct ") {
                let (name, args) = surround!(&head, "{", "}")?;
                result.push(Define::Class(
                    Generics::parse(&name)?,
                    Object::Struct(args!(&args)),
                ));
            } else if let Some(head) = line.strip_prefix("enum ") {
                let (name, args) = surround!(&head, "{", "}")?;
                result.push(Define::Class(
                    Generics::parse(&name)?,
                    Object::Enum(args!(&args)),
                ));
            }
        }
        Ok(result)
    }
}

impl Expr {
    fn parse(src: &str) -> Result<Expr, String> {
        let src = src.trim();
        fn is_operator(src: &str) -> Result<(String, String, String), String> {
            let tokens = lexer(src, SPACE)?;
            if tokens.len() < 3 {
                return Err(String::new());
            }
            let pos: usize = tokens.len() - 2;
            Ok((
                tokens[..pos].join(SPACE),
                tokens[pos].to_string(),
                tokens[pos + 1].to_string(),
            ))
        }
        if let Some(src) = src.strip_prefix("print ") {
            Ok(Expr::Print(true, serial!(src, Expr::parse)))
        } else if let Some(src) = src.strip_prefix("format ") {
            Ok(Expr::Print(false, serial!(src, Expr::parse)))
        } else if let Some(src) = src.strip_prefix("let ") {
            if let Ok((name, val)) = once!(src, "=") {
                return Ok(Expr::Let(
                    Box::new(Expr::parse(&name)?),
                    Box::new(Expr::parse(&val)?),
                ));
            }
            let (name, typ) = once!(src, ":")?;
            Ok(Expr::Let(
                Box::new(Expr::parse(&name)?),
                Box::new(Expr::Null(Type::parse(&typ)?)),
            ))
        } else if let Some(src) = src.strip_prefix("if ") {
            let (cond, body) = once!(src, "then")?;
            let cond = Box::new(Expr::parse(&cond)?);
            if let Ok((then, els)) = once!(&body, "else") {
                let els = Some(Box::new(Expr::parse(&els)?));
                Ok(Expr::If(cond, Box::new(Expr::parse(&then)?), els))
            } else {
                Ok(Expr::If(cond, Box::new(Expr::parse(&body)?), None))
            }
        } else if let Some(src) = src.strip_prefix("match ") {
            let (expr, pats) = surround!(src, "{", "}")?;
            let pats = serial!(&pats, |src| {
                let (head, ret) = once!(src, "=")?;
                let ret = Expr::parse(&ret)?;
                if let Ok((key, bind)) = once!(&head.trim(), SPACE) {
                    return Ok((Name::new(&key)?, Some(Expr::parse(&bind)?), ret));
                }
                Ok((Name::new(&head)?, None, ret))
            });
            Ok(Expr::Match(Box::new(Expr::parse(&expr)?), pats))
        } else if let Some(src) = src.strip_prefix("while ") {
            let (cond, body) = once!(src, "do")?;
            let (cond, body) = (Box::new(Expr::parse(&cond)?), Box::new(Expr::parse(&body)?));
            Ok(Expr::While(cond, body))
        } else if let Some(src) = src.strip_prefix("for ") {
            let (head, body) = once!(src, "do")?;
            let (cnt, arr) = once!(&head, "=")?;
            Ok(Expr::For(
                Box::new(Expr::parse(&cnt)?),
                Box::new(Expr::parse(&arr)?),
                Box::new(Expr::parse(&body)?),
            ))
        } else if let Some(class) = src.strip_prefix("new ") {
            Ok(Expr::New(Type::parse(class)?))
        } else if let Some(expr) = src.strip_prefix("clone ") {
            Ok(Expr::Clone(Box::new(Expr::parse(expr)?)))
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
            macro_rules! op {
                ($($op: pat => $expr: ident ,)*) => {
                    match op.as_str() {
                        $($op => Ok(Expr::$expr(lhs, rhs)),)*
                         _  => Err(format!("unknown operator: {op}"))
                    }
                };
            }
            op!("+"  => Add, "-"  => Sub, "*" => Mul, "/" => Div, "%"  => Mod,
                "==" => Eq,  "!=" => Ne,  ">" => Gt,  "<" => Lt,  ">=" => Ge, "<=" =>Le,
                "&"  => And, "|"  => Or,  "^" => Xor,)
        } else if src == "()" {
            Ok(Expr::Null(Type::Void))
        } else if let Some(text) = surround!("\"", src, "\"") {
            Ok(Expr::String(text.to_owned()))
        } else if let Some(expr) = surround!("(", src, ")") {
            Expr::parse(expr)
        } else if let Some(arr) = surround!("[", src, "]") {
            if let Ok((typ, len)) = once!(arr, ";") {
                let Ok(len) = len.trim().parse::<usize>() else {
                    return Err(format!("not length: {len}"));
                };
                return Ok(Expr::Init(Type::parse(&typ)?, len));
            }
            if let Ok(arr) = serial!(arr, Expr::parse).try_into() {
                Ok(Expr::Sequence(arr))
            } else {
                Err(format!("empty array: {src}"))
            }
        } else if let Ok(i) = src.parse::<i64>() {
            Ok(Expr::Integer(i))
        } else if let Ok(f) = src.parse::<f64>() {
            Ok(Expr::Float(Float(f)))
        } else if let Some(class) = src.strip_suffix("?") {
            Ok(Expr::Check(Box::new(Expr::parse(class)?)))
        } else if let Ok((obj, key)) = once!(src, ".", right) {
            let obj = Expr::parse(&obj)?;
            if let Ok(Expr::Call(callee, arg)) = Expr::parse(&key) {
                return Ok(Expr::Call(callee.clone(), [vec![obj], arg].concat()));
            }
            Ok(Expr::Member(Box::new(obj), Name::new(&key)?))
        } else if let Ok((typ, key)) = once!(src, "::") {
            let typ = Type::parse(&typ)?;
            if let Ok((key, val)) = surround!(&key, "(", ")") {
                let name = Name::new(&key)?;
                Ok(Expr::Enum(typ, name, Box::new(Expr::parse(&val)?)))
            } else {
                let name = Name::new(&key)?;
                Ok(Expr::Enum(typ, name, Box::new(Expr::Null(Type::Void))))
            }
        } else if let Ok((func, args)) = surround!(src, "(", ")") {
            Ok(Expr::Call(
                Box::new(Expr::parse(&func)?),
                serial!(&args, Expr::parse),
            ))
        } else if let Ok((arr, idx)) = surround!(src, "[", "]") {
            Ok(Expr::Index(
                Box::new(Expr::parse(&arr)?),
                Box::new(Expr::parse(&idx)?),
            ))
        } else if let Ok(b) = src.parse::<bool>() {
            Ok(Expr::Boolean(b))
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
            "Bool" => Ok(Type::Boolean),
            "Float" => Ok(Type::Float),
            "()" => Ok(Type::Void),
            x => {
                if let Ok((ret, args)) = surround!(x, "(", ")") {
                    Ok(Type::Function(Lambda(
                        Vec::new(),
                        Box::new(Type::parse(&ret)?),
                        Some(serial!(&args, Type::parse)),
                    )))
                } else if let Some(typ) = surround!("[", x, "]") {
                    Ok(Type::Array(Box::new(Type::parse(typ)?)))
                } else {
                    Ok(Type::Class(Generics::parse(x)?))
                }
            }
        }
    }
}

impl Generics {
    pub fn parse(src: &str) -> Result<Generics, String> {
        let x = src.trim();
        if let Some((var, args)) = surround!("<", ">", x) {
            Ok(Generics(Name::new(var)?, serial!(args, Type::parse)))
        } else {
            Ok(Generics(Name::new(x)?, vec![]))
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn comma(x: &[Type]) -> String {
            map!(x, |x: &Type| x.to_string()).join(", ")
        }
        match self {
            Type::Integer => write!(f, "Int"),
            Type::String => write!(f, "Str"),
            Type::Float => write!(f, "Float"),
            Type::Boolean => write!(f, "Bool"),
            Type::Void => write!(f, "()"),
            Type::Array(typ) => write!(f, "[{typ}]"),
            Type::Class(Generics(name, args)) if args.is_empty() => write!(f, "{name}"),
            Type::Class(Generics(name, args)) => write!(f, "{name}<{}>", comma(args)),
            Type::Function(Lambda(_, ret, Some(args))) => write!(f, "{ret}({})", comma(args)),
            Type::Function(Lambda(_, ret, None)) => write!(f, "{ret}()"),
        }
    }
}
