pub mod emit;
pub mod lexer;
pub mod parse;
pub mod r#type;
pub mod infer;

use lexer::lexer;
use lexer::name::Name;
use parse::SPACE;

use indexmap::{IndexMap, IndexSet};
use ordered_float::OrderedFloat as Float;
use vec1::Vec1;

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
        let mut ast = error!(Define::parse(&code));
        let output = error!(Define::compile(&mut ast));
        error!(stdout().write_all(output.as_bytes()));
    };
    let thread = Builder::new().stack_size(8 * 1024 * 1024);
    let thread = thread.spawn(build).unwrap();
    thread.join().unwrap();
}

impl Define {
    const CORE: [&str; 5] = ["calloc", "printf", "g_strdup_printf", "free", "memcpy"];

    pub fn compile(defines: &mut [Self]) -> Result<String, String> {
        let mut text = String::new();
        let ctx = &mut Context::default();
        macro_rules! name {
            ($define: expr) => {
                match $define.clone() {
                    Define::Function(Generics(func, _), _, (Some(_), _)) => Some(func),
                    Define::Class(Generics(class, _), _) => Some(class),
                    _ => None,
                }
                .clone()
            };
            (all, $define: expr) => {
                match $define.clone() {
                    Define::Function(Generics(func, _), _, _) => func,
                    Define::Class(Generics(class, _), _) => class,
                }
                .clone()
            };
        }
        ctx.global.def = {
            let mut map = IndexMap::new();
            for define in defines {
                map.insert(name!(all, define), define.clone());
            }
            map
        };
        ctx.global.lib = {
            let mut map = IndexMap::new();
            for line in Self::CORE {
                let signature = Type::Function(vec![], Box::new(Type::Void), None);
                map.insert(Name::new(line)?, signature);
            }
            map
        };
        for (_, define) in ctx.global.def.clone() {
            define.infer(ctx)?;
        }
        for (_, define) in ctx.global.def.clone() {
            text += &define.emit(ctx)?;
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
#[derive(Clone, PartialEq)]
pub enum Object {
    Struct(IndexMap<Name, Type>),
    Enum(IndexMap<Name, Type>),
}
#[derive(Clone, PartialEq)]
pub enum Define {
    Function(Generics, IndexMap<Name, Type>, (Option<Expr>, Option<Type>)),
    Class(Generics, Object),
}
#[derive(Clone, Hash, Default, PartialEq, Eq)]
pub struct Generics(Name, Vec<Type>);

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    // Literal
    Integer(i64),
    Float(Float<f64>),
    Bool(bool),
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
    Eql(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    GtEq(Box<Expr>, Box<Expr>),
    LtEq(Box<Expr>, Box<Expr>),
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
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    String,
    Integer,
    Boolean,
    Float,
    Array(Box<Type>),
    Class(Generics),
    Function(Vec<Type>, Box<Type>, Option<Vec<Type>>),
    Void,
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