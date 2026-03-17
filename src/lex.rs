pub mod name {
    use std::fmt;

    const RESERVED: [&str; 8] = ["let", "if", "then", "else", "while", "do", "break", "return"];

    #[derive(Clone, PartialEq, Hash, Eq)]
    pub struct Name(String);

    impl Name {
        pub fn new(name: &str) -> Result<Name, String> {
            let name = name.trim();
            if name.is_empty() {
                return Err(format!("empty name"));
            }
            fn validate(x: char) -> bool {
                x == '_' || x.is_ascii_alphabetic() || x.is_digit(10)
            }
            if !name.chars().all(validate) {
                return Err(format!("invalid name: {name}"));
            }
            if RESERVED.contains(&name) {
                return Err(format!("reserved name: {name}"));
            }
            Ok(Name(name.to_owned()))
        }
    }

    impl fmt::Display for Name {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}

pub fn tokenize(input: &str, delimiter: &str) -> Result<Vec<String>, String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();

    let mut in_parentheses: usize = 0;
    let mut in_quote = false;
    let mut is_escape = false;

    let chars = input.chars().collect::<Vec<char>>();
    let mut index = 0;

    while index < chars.len() {
        let c = chars[index];
        if is_escape {
            current_token.push(c);
            is_escape = false;
        } else {
            match c {
                '(' | '{' | '[' if !in_quote => {
                    if c.to_string() == delimiter.to_string() && in_parentheses == 0 {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                    current_token.push(c);
                    in_parentheses += 1;
                }
                ')' | '}' | ']' if !in_quote => {
                    current_token.push(c);
                    in_parentheses.checked_sub(1).map(|x| in_parentheses = x);
                }
                '"' => {
                    in_quote = !in_quote;
                    current_token.push(c);
                }
                '\\' if in_quote => {
                    current_token.push(c);
                    is_escape = true;
                }
                _ => {
                    if input.get(index..index + delimiter.len()) == Some(delimiter) {
                        if in_parentheses != 0 || in_quote || is_escape {
                            current_token += delimiter;
                        } else if !current_token.is_empty() {
                            tokens.push(current_token.clone());
                            current_token.clear();
                        }
                        index += delimiter.len();
                        continue;
                    } else {
                        current_token.push(c);
                    }
                }
            }
        }
        index += 1
    }

    // Syntax error check
    if is_escape || in_quote || in_parentheses != 0 {
        return Err(format!("not closed: {current_token}"));
    }
    if !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Ok(tokens)
}

#[macro_export]
macro_rules! ok {
    ($v: expr) => {
        if let Some(v) = $v {
            Ok(v)
        } else {
            Err(format!("invalid token"))
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
            Err(format!("expected `{}` but not found", $delimiter))
        }
    }};
}
