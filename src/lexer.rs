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
