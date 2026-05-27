pub mod emit;
pub mod lex;
pub mod parse;
pub mod typ;

use indexmap::{IndexMap, IndexSet, indexmap};
use lex::name::Name;
use lex::tokenize;
use ordered_float::OrderedFloat as Float;

use std::hash::Hash;
use std::io::{Read, Write, stdin, stdout};
use std::process::exit;

fn main() {
    macro_rules! error {
        ($value: expr) => {
            match $value {
                Ok(value) => value,
                Err(err) => {
                    eprintln!("Error! {err}");
                    exit(1)
                }
            }
        };
    }
    let code = {
        let mut buffer = String::new();
        error!(stdin().read_to_string(&mut buffer));
        buffer.trim().to_owned()
    };
    let mut ast = error!(Define::parse(&code));
    let output = error!(Define::compile(&mut ast));
    error!(stdout().write_all(output.as_bytes()));
}

impl Define {
    const LIB: [&str; 3] = ["calloc", "printf", "free"];

    pub fn compile(defines: &mut Vec<Self>) -> Result<String, String> {
        let mut text = String::new();
        let ctx = &mut Context::default();

        macro_rules! name {
            ($define: expr) => {
                if let Define::Function(Generics(func, _), _, _) = $define.clone() {
                    Some(func.clone())
                } else if let Define::Class(Generics(class, _), _) = $define.clone() {
                    Some(class.clone())
                } else {
                    None
                }
            };
        }
        ctx.global.def = {
            let mut map = IndexMap::new();
            for define in defines.clone() {
                if let Some(name) = &name!(define) {
                    map.insert(name.clone(), define.clone());
                }
            }
            map
        };
        ctx.global.lib = {
            let mut map = IndexMap::new();
            for line in Self::LIB {
                let signature = Type::Function(vec![], Box::new(Type::None), None);
                map.insert(Name::new(line)?, signature);
            }
            map
        };
        for (_, define) in ctx.global.def.clone() {
            define.infer(ctx)?;
        }
        for (_, define) in ctx.global.def.clone() {
            if Some(Name::new("main")?) == name!(define) {
                text = define.emit(ctx)? + &text;
            } else {
                text += &define.emit(ctx)?;
            }
        }
        let data = ctx.global.data.clone();
        for (_, define) in ctx.global.def.clone() {
            if let Some(func) = name!(define) {
                ctx.global.lib.shift_remove(&func);
            }
        }
        let mut lib = String::from("\nsection .text\n\tglobal main\n");
        for symbol in ctx.global.lib.keys() {
            lib += &format!("\textern {symbol}\n");
        }
        Ok(format!("section .data\n{data}{lib}{text}\n"))
    }
}

// Abstract Syntax Tree (AST)

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Struct(IndexMap<Name, Type>),
    Enum(IndexMap<Name, Type>),
}

#[derive(Clone, PartialEq)]
pub enum Define {
    Function(Generics, IndexMap<Name, Type>, Expr),
    Class(Generics, Object),
}

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct Generics(Name, Vec<Type>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    Integer(i64),
    Float(Float<f64>),
    Bool(bool),
    String(String),
    Null(Type),
    Variable(Generics),
    Let(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Read(Box<Expr>, Type, Box<Expr>),
    Write(Box<Expr>, Box<Expr>, Box<Expr>),
    Array(Type, usize),
    Index(Box<Expr>, Box<Expr>),
    New(Type),
    Member(Box<Expr>, Name),
    Check(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    Print(Vec<Expr>),
    Block(Vec<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Eql(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    GtEq(Box<Expr>, Box<Expr>),
    LtEq(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Type {
    String,
    Integer,
    Bool,
    Float,
    Array(Box<Type>),
    Class(Generics),
    Function(Vec<Type>, Box<Type>, Option<Vec<Type>>),
    None,
}

#[derive(Default)]
pub struct Context {
    global: Global,
    local: Function,
    table: IndexMap<Name, Function>,
}

#[derive(Default)]
pub struct Global {
    idx: usize,
    data: String,
    lib: IndexMap<Name, Type>,
    def: IndexMap<Name, Define>,
    table: IndexMap<Name, (Vec<Type>, Object)>,
    meta: IndexSet<Name>,
    alias: IndexMap<Type, Type>,
}

#[derive(Default, Debug, Clone)]
pub struct Function {
    var: IndexMap<Name, Type>,
    scope: IndexMap<Name, Type>,
    typed: IndexMap<Expr, Type>,
    expand: IndexMap<Expr, Expr>,
}
