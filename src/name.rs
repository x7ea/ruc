use crate::*;
use std::fmt;

const RESERVED: [&str; 12] = [
    "print", "format", "let", "new", "clone", "if", "then", "else", "for", "while", "do", "match",
];

#[derive(Clone, Default, PartialEq, Hash, Eq)]
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
                Type::Boolean => "B".to_string(),
                Type::Void => "N".to_string(),
                Type::Float => "F".to_string(),
                Type::Array(typ) => format!("A{}", mangle(typ)),
                Type::Function(_, ret, None) => format!("L{}", mangle(ret)),
                Type::Function(_, ret, Some(args)) => {
                    format!("L{}{}", mangle(ret), map!(args, mangle).concat())
                }
                Type::Class(Generics(name, _)) => {
                    format!("C{}", name.to_string().to_lowercase())
                }
            }
        }
        let typ = map!(self.1, mangle).concat();
        Name(format!("{}.{typ}", self.0))
    }
}
