pub mod emit;
pub mod lexer;
pub mod parse;
pub mod typ;

use derive_more::Unwrap;
use indexmap::{IndexMap, IndexSet};
use ordered_float::OrderedFloat as Float;
use vec1::Vec1;
use {
    lexer::{lexer, name::Name},
    parse::SPACE,
};

use std::hash::Hash;
use std::io::{Read, Write, stdin, stdout};
use std::process::exit;

fn main() {
    macro_rules! error {
        ($val: expr) => {
            match $val {
                Ok(val) => val,
                Err(err) => {
                    eprintln!("Error! {err}");
                    exit(1)
                }
            }
        };
    }
    use std::thread::Builder;
    let build = || {
        let code = {
            let mut buffer = String::new();
            error!(stdin().read_to_string(&mut buffer));
            buffer.trim().to_owned()
        };
        let ast = error!(Define::parse(&code));
        let output = error!(Define::compile(&ast));
        error!(stdout().write_all(output.as_bytes()));
    };
    let thread = Builder::new().stack_size(8 * 1024 * 1024);
    let thread = thread.spawn(build).unwrap();
    thread.join().unwrap();
}

// Abstract Syntax Tree (AST)

#[derive(Clone, PartialEq)]
pub enum Define {
    Function(Generics, IndexMap<Name, Type>, (Option<Expr>, Option<Type>)),
    Class(Generics, Object),
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    // Literal
    Integer(i64),
    Float(Float<f64>),
    Boolean(bool),
    String(String),
    Null(Type),
    // Reference
    Variable(Generics),
    Let(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    // Array
    Sequence(Vec1<Expr>),
    Index(Box<Expr>, Box<Expr>),
    // Object
    New(Type),
    Enum(Type, Name, Box<Expr>),
    Member(Box<Expr>, Name),
    Check(Box<Expr>),
    // Control
    Print(bool, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Match(Box<Expr>, Vec1<(Name, Option<Expr>, Expr)>),
    For(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    // Operator
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    // Compare
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    // Logial
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    // Low-layer IR
    Read(Box<Expr>, Type, Box<Expr>),
    Write(Box<Expr>, Box<Expr>, Box<Expr>),
    Clone(Box<Expr>),
    Init(Type, usize),
}

#[derive(Clone, PartialEq, Unwrap, Eq, Hash)]
pub enum Type {
    String,
    Integer,
    Boolean,
    Float,
    Array(Box<Type>),
    Class(Generics),
    Function(Lambda),
    Void,
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct Lambda(Vec<Type>, Box<Type>, Option<Vec<Type>>);

#[derive(Clone, Hash, Default, PartialEq, Eq)]
pub struct Generics(Name, Vec<Type>);

#[derive(Clone, PartialEq)]
pub enum Object {
    Struct(IndexMap<Name, Type>),
    Enum(IndexMap<Name, Type>),
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
    alias: IndexMap<Type, Type>,
    extrn: IndexSet<Name>,
}

#[derive(Default, Clone)]
pub struct Function {
    var: IndexMap<Name, Type>,
    scope: IndexMap<Name, Type>,
    typed: IndexMap<Expr, Type>,
    expand: IndexMap<Expr, Expr>,
    class: Option<Name>,
}
