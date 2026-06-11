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
    ($x: expr, $ls: literal, $rs: literal) => {
        lexer($x.trim(), &$ls).and_then(|x| {
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
macro_rules! new {
    ($layout: expr) => {
        Expr::Call(
            Box::new(Expr::Variable(Generics(Name::new("calloc")?, vec![]))),
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
