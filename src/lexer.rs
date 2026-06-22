use crate::*;

pub fn lexer(src: &str, del: &str) -> Result<Vec<String>, String> {
    let (mut level, mut idx) = (0, 0);
    let (mut quote, mut esc) = (false, false);

    let (mut tokens, mut current) = (Vec::new(), String::new());
    let chars = src.chars().collect::<Vec<char>>();

    while idx < chars.len() {
        let c = chars[idx];
        if esc {
            current.push(c);
            esc = false;
            idx += 1;
            continue;
        }
        if let Some(op) = src.get(idx..idx + 3)
            && [" < ", " > ", " <=", " >="].contains(&op)
        {
            if del == SPACE {
                tokens.append(&mut vec![current.clone(), op.trim().to_string()]);
                current.clear();
            } else {
                current += op;
            }
            idx += 3;
            continue;
        }
        match c {
            '<' | '(' | '{' | '[' if !quote => {
                if c.to_string() == del && level == 0 {
                    tokens.push(current.clone());
                    current.clear();
                }
                current.push(c);
                level += 1;
            }
            '>' | ')' | '}' | ']' if !quote => {
                current.push(c);
                level -= 1;
            }
            '"' => {
                quote = !quote;
                current.push(c);
            }
            '\\' if quote => {
                current.push(c);
                esc = true;
            }
            _ => {
                if src.get(idx..idx + del.len()) == Some(del) {
                    if level != 0 || quote || esc {
                        current += del;
                    } else if !current.is_empty() {
                        tokens.push(current.clone());
                        current.clear();
                    }
                    idx += del.len();
                    continue;
                } else {
                    current.push(c);
                }
            }
        }
        idx += 1
    }
    if esc || quote || level != 0 {
        return Err(format!("not closed: {current}"));
    }
    if !current.is_empty() {
        tokens.push(current.clone());
        current.clear();
    }
    Ok(tokens)
}

pub mod name {
    use crate::*;
    use std::fmt;

    const RESERVED: [&str; 12] = [
        "print", "format", "let", "new", "clone", "if", "then", "else", "for", "while", "do",
        "match",
    ];

    #[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
    pub struct Name(String);

    impl Name {
        pub fn new(name: &str) -> Result<Name, String> {
            let name = name.trim();
            if name.is_empty() {
                return Err(format!("empty: {name}"));
            }
            let name = name.replace(".", "__");
            fn validate(x: char) -> bool {
                x == '_' || x.is_ascii_alphabetic() || x.is_ascii_digit()
            }
            if !name.chars().all(validate) {
                return Err(format!("invalid: {name}"));
            }
            if RESERVED.contains(&name.as_str()) {
                return Err(format!("reserved: {name}"));
            }
            Ok(Name(name.to_lowercase()))
        }
    }

    impl fmt::Display for Name {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl fmt::Display for Generic {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let Generic(name, args) = self.clone();
            let args = map!(args, |x| x.to_string()).join(", ");
            if args.is_empty() {
                write!(f, "{name}")
            } else {
                write!(f, "{name}<{args}>")
            }
        }
    }

    impl Generic {
        pub fn generics(&self) -> Name {
            let Generic(name, typ) = self;
            if typ.is_empty() {
                return name.clone();
            }
            Name(format!("{name}.{:x}", hash!(typ)))
        }
    }
}

#[macro_export]
macro_rules! surround {
    ($ls: literal, $x: expr, $rs: literal) => {
        $x.trim()
            .strip_prefix($ls)
            .and_then(|x| x.strip_suffix($rs))
    };
    ($ls: literal, $rs: literal,$x: expr) => {
        $x.trim().strip_suffix($rs).and_then(|x| x.split_once($ls))
    };
    ($src: expr, $ls: literal, $rs: literal) => {
        lexer($src.trim(), &$ls).and_then(|src| {
            if src.len() < 2 {
                return Err(String::new());
            }
            let func = src[..src.len() - 1].concat();
            let args = src[src.len() - 1].to_string();
            let args = args[1..args.len() - 1].to_string();
            Ok((func, args))
        })
    };
}

#[macro_export]
macro_rules! hash {
    ($val: expr) => {{
        use std::hash::{DefaultHasher, Hasher};
        let mut state = DefaultHasher::new();
        $val.hash(&mut state);
        state.finish()
    }};
}

#[macro_export]
macro_rules! serial {
    ($arr: expr, $lambda: expr) => {
        lexer($arr, ",")?
            .iter()
            .map(|x| $lambda(&x))
            .collect::<Result<Vec<_>, String>>()?
    };
}

#[macro_export]
macro_rules! once {
    ($v: expr, $del: expr) => {{
        let v = lexer($v, $del)?;
        if v.len() >= 2 {
            Ok((v[0].clone(), v[1..].join($del)))
        } else {
            Err(format!("expected: {}", $del))
        }
    }};
    ($v: expr,$del: literal, right) => {{
        let v = lexer($v, $del)?;
        if v.len() >= 2 {
            let last = v.len() - 1;
            Ok((v[..last].join($del), v[last].clone()))
        } else {
            Err(format!("expected: {}", $del))
        }
    }};
}

#[macro_export]
macro_rules! map {
    ($arr: expr, $lambda: expr) => {{ $arr.iter().map($lambda).collect::<Vec<_>>() }};
    ($arr: expr, $lambda: expr, ok) => {{ $arr.iter().map($lambda).collect::<Result<Vec<_>, String>>() }};
}

#[macro_export]
macro_rules! var {
    ($name: expr) => {{ Expr::Variable(Generic(Name::new(&$name)?, Vec::new())) }};
    ($name: expr, $typ: expr) => {{ Expr::Variable(Generic(Name::new(&$name)?, vec![$typ])) }};
}

#[macro_export]
macro_rules! new {
    ($layout: expr) => {
        Expr::Call(
            Box::new(var!("calloc")),
            vec![Expr::Integer($layout as i64), Expr::Integer(8)],
        )
    };
}

#[macro_export]
macro_rules! len {
    ($arr: expr) => {
        Box::new(Expr::Member($arr.clone(), Name::new("len")?))
    };
}

#[macro_export]
macro_rules! array {
    ($arr: expr, $idx: expr) => {
        Box::new(Expr::Add(
            Box::new(Expr::Mod($idx.clone(), len!($arr))),
            Box::new(Expr::Integer(1)),
        ))
    };
}
