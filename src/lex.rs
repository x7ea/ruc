use crate::parse::SPACE;

pub mod name {
    use crate::*;
    use std::fmt;

    const RESERVED: [&str; 7] = ["let", "if", "then", "else", "while", "do", "new"];

    #[derive(Clone, Debug, PartialEq, Hash, Eq)]
    pub struct Name(String);

    impl Name {
        pub fn new(name: &str) -> Result<Name, String> {
            let name = name.trim();
            if name.is_empty() {
                return Err("empty".to_string());
            }
            fn validate(x: char) -> bool {
                x == '_' || x.is_ascii_alphabetic() || x.is_ascii_digit()
            }
            if !name.chars().all(validate) {
                return Err(format!("invalid: {name}"));
            }
            if RESERVED.contains(&name) {
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
            write!(f, "{}<{}>", self.0, args)
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
                }
            }
            let typ = map!(self.1, mangle).concat();
            Name(format!("{}.{typ}", self.0))
        }
    }
}

pub fn tokenize(input: &str, delimiter: &str) -> Result<Vec<String>, String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current = String::new();

    let mut level: usize = 0;
    let mut in_quote = false;
    let mut is_escape = false;

    let chars = input.chars().collect::<Vec<char>>();
    let mut index = 0;

    while index < chars.len() {
        let c = chars[index];
        if is_escape {
            current.push(c);
            is_escape = false;
            index += 1;
            continue;
        }
        if let Some(op) = input.get(index..index + 3)
            && [" < ", " > "].contains(&op)
        {
            if delimiter == SPACE {
                tokens.push(current.clone());
                tokens.push(op.trim().to_string());
                current.clear();
            } else {
                current += op;
            }
            index += 3;
            continue;
        }
        match c {
            '<' | '(' | '{' | '[' if !in_quote => {
                if c.to_string() == delimiter && level == 0 {
                    tokens.push(current.clone());
                    current.clear();
                }
                current.push(c);
                level += 1;
            }
            '>' | ')' | '}' | ']' if !in_quote => {
                current.push(c);
                level = level.saturating_sub(1);
            }
            '"' => {
                in_quote = !in_quote;
                current.push(c);
            }
            '\\' if in_quote => {
                current.push(c);
                is_escape = true;
            }
            _ => {
                if input.get(index..index + delimiter.len()) == Some(delimiter) {
                    if level != 0 || in_quote || is_escape {
                        current += delimiter;
                    } else if !current.is_empty() {
                        tokens.push(current.clone());
                        current.clear();
                    }
                    index += delimiter.len();
                    continue;
                } else {
                    current.push(c);
                }
            }
        }
        index += 1
    }

    if is_escape || in_quote || level != 0 {
        return Err(format!("not closed: {current}"));
    }
    if !current.is_empty() {
        tokens.push(current.clone());
        current.clear();
    }

    Ok(tokens)
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
    ($v: expr, $delimiter: expr) => {{
        let v = tokenize($v, $delimiter)?;
        if v.len() >= 2 {
            Ok((v[0].clone(), v[1..].join($delimiter)))
        } else {
            Err(format!("expected: {}", $delimiter))
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
macro_rules! map {
    ($arr: expr => $lambda: expr) => {
        $arr.iter()
            .map($lambda)
            .collect::<Result<Vec<_>, String>>()?
    };
    ($arr: expr, $lambda: expr) => {
        $arr.iter().map($lambda).collect::<Vec<_>>()
    };
}
