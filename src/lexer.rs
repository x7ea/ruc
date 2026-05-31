use crate::{Context, parse::SPACE};

pub mod name {
    use crate::*;
    use std::fmt;

    const RESERVED: [&str; 12] = [
        "print", "format", "let", "new", "clone", "if", "then", "else", "while", "do", "match",
        "for",
    ];

    #[derive(Clone, Debug, PartialEq, Hash, Eq)]
    pub struct Name(String);

    impl Name {
        pub fn new(name: &str) -> Result<Name, String> {
            let name = name.trim();
            if name.is_empty() {
                return Err("empty".to_string());
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
            Ok(Name(name.to_owned()))
        }
    }

    impl fmt::Display for Name {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl fmt::Display for Generics {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let args = map!(self.1, |x| x.to_string()).join(", ");
            if args.is_empty() {
                write!(f, "{}", self.0)
            } else {
                write!(f, "{}<{}>", self.0, args)
            }
        }
    }

    impl Generics {
        pub fn generics(&self) -> Name {
            if self.1.is_empty() {
                return self.0.clone();
            }
            fn mangle(typ: &Type) -> String {
                match typ {
                    Type::Integer => "I".to_string(),
                    Type::String => "S".to_string(),
                    Type::Float => "F".to_string(),
                    Type::Bool => "B".to_string(),
                    Type::None => "N".to_string(),
                    Type::Array(typ) => format!("A{}", mangle(typ)),
                    Type::Class(Generics(name, _)) => format!("C{name}"),
                    Type::Function(_, ret, Some(args)) => {
                        let args = map!(args, mangle).concat();
                        format!("L{}{args}", mangle(ret))
                    }
                    Type::Function(_, ret, None) => format!("L{}", mangle(ret)),
                    Type::Any => panic!(),
                }
            }
            let typ = map!(self.1, mangle).concat();
            Name(format!("{}.{typ}", self.0))
        }
    }
}

pub fn lexer(src: &str, del: &str) -> Result<Vec<String>, String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current = String::new();

    let mut level: usize = 0;
    let mut quote = false;
    let mut esc = false;

    let chars = src.chars().collect::<Vec<char>>();
    let mut idx = 0;

    while idx < chars.len() {
        let c = chars[idx];
        if esc {
            current.push(c);
            esc = false;
            idx += 1;
            continue;
        }
        if let Some(op) = src.get(idx..idx + 3)
            && [" < ", " > "].contains(&op)
        {
            if del == SPACE {
                tokens.push(current.clone());
                tokens.push(op.trim().to_string());
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
                level = level.saturating_sub(1);
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

impl Context {
    pub fn label(&mut self) -> String {
        let id = self.global.idx;
        self.global.idx += 1;
        id.to_string()
    }
}

#[macro_export]
macro_rules! surround {
    ($ls: literal, $x: expr, $rs: literal) => {
        $x.strip_prefix($ls).and_then(|x| x.strip_suffix($rs))
    };
    ($ls: literal, $rs: literal,$x: expr) => {
        $x.strip_suffix($rs).and_then(|x| x.split_once($ls))
    };
    ($x: expr, $ls: literal, $rs: literal) => {
        lexer($x, &$ls).and_then(|x| {
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

#[macro_export]
macro_rules! ok {
    ($v: expr) => {
        if let Some(v) = $v {
            Ok(v)
        } else {
            Err(String::new())
        }
    };
}

#[macro_export]
macro_rules! once {
    ($v: expr, $del: expr) => {{
        let v = lexer($v, $del)?;
        if v.len() >= 2 {
            Ok((v[0].clone(), v[1..].join($del)))
        } else {
            Err(format!("expected {}", $del))
        }
    }};
    ($v: expr,$del: literal, right) => {{
        let v = lexer($v, $del)?;
        if v.len() >= 2 {
            let last = v.len() - 1;
            Ok((v[..last].join($del), v[last].clone()))
        } else {
            Err(format!("expected {}", $del))
        }
    }};
}

#[macro_export]
macro_rules! hash {
    ($self: expr) => {{
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;
        let mut hasher = DefaultHasher::new();
        $self.hash(&mut hasher);
        hasher.finish()
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
macro_rules! map {
    ($arr: expr, $lambda: expr) => {
        $arr.iter().map($lambda).collect::<Vec<_>>()
    };
}
