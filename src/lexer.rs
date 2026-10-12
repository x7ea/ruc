use crate::*;

pub fn lexer(src: &str, del: &str) -> Result<Vec<String>, String> {
    let chars = src.chars().collect::<Vec<char>>();
    let (mut level, mut idx, mut quote, mut esc) = (0, 0, false, false);
    let (mut tokens, mut current) = (Vec::new(), String::new());
    while idx < chars.len() {
        let c = chars[idx];
        if esc {
            current.push(c);
            esc = false;
            idx += 1;
            continue;
        }
        if let Some(op) = src.get(idx..idx + 3)
            && [" < ", " > ", "<= ", " >="].contains(&op)
        {
            if del == SPACE && level == 0 {
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

    const RESERVED: [&str; 10] = [
        "let", "new", "clone", "if", "then", "else", "for", "while", "do", "match",
    ];

    #[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
    pub struct Name(pub String);

    impl Name {
        pub fn new(name: &str) -> Result<Name, String> {
            let name = name.trim();
            if name.is_empty() {
                return Err(format!("empty: {name}"));
            }
            if let Some((class, name)) = name.split_once(".") {
                return Ok(Name::new(name)?.class(&Type::parse(class)?));
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

        pub fn class(&self, typ: &Type) -> Name {
            Name(format!("{typ:?}.{self}"))
        }
        pub fn generic(&self, typ: &Type) -> Name {
            Name(format!("{self}@{typ:?}"))
        }
        pub fn is_generic(&self) -> bool {
            self.0.contains("@")
        }
    }

    impl Display for Name {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}

#[macro_export]
macro_rules! surround {
    ($ls: literal, $x: expr, $rs: literal) => {{ $x.strip_prefix($ls).and_then(|x| x.strip_suffix($rs)) }};
    ($ls: literal, $rs: literal, $x: expr) => {{ $x.trim().strip_suffix($rs).and_then(|x| x.split_once($ls)) }};
    ($src: expr, $ls: literal, $rs: literal) => {{ rsplit!($src, &$ls).and_then(|(f, args)| Ok((f, args[1..args.len() - 1].to_string()))) }};
}

#[macro_export]
macro_rules! serial {
    ($arr: expr, $lambda: expr) => {{ map!({ lexer($arr, ",")? }, |x| $lambda(&x))? }};
}

#[macro_export]
macro_rules! __split_internal {
    ($token: expr, $del: expr, $f: expr) => {{
        let token = lexer($token, $del)?;
        match token.len() {
            len if len >= 2 => Ok($f(&token, $del.to_string())),
            _ => Err(format!("expected: {}", $del)),
        }
    }};
}

pub fn left(tok: &Vec<String>, del: String) -> (String, String) {
    (tok[0].clone(), tok[1..].join(&del))
}
pub fn right(tok: &Vec<String>, del: String) -> (String, String) {
    (tok[..tok.len() - 1].join(&del), tok[tok.len() - 1].clone())
}

#[macro_export]
macro_rules! split {
    ($token: expr, $del: expr) => {{ __split_internal!($token, $del, crate::lexer::left) }};
}
#[macro_export]
macro_rules! rsplit {
    ($token: expr, $del: expr) => {{ __split_internal!($token, $del, crate::lexer::right) }};
}
