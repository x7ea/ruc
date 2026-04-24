pub mod emit;
pub mod lex;
mod parse;

use indexmap::IndexSet;
use lex::name::Name;
use lex::tokenize;
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
    let output = error!(Define::compile(error!(Define::parse(&code))));
    error!(stdout().write_all(output.as_bytes()));
}

#[derive(Clone)]
pub struct Define(pub Name, pub IndexSet<Name>, pub Expr);

// Abstract Syntax Tree (AST)
#[derive(Clone, PartialEq)]
pub enum Expr {
    // Literal
    Integer(i64),
    String(String),
    Undefined,
    // Reference
    Variable(Name),
    Pointer(Name),
    Derefer(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    // Structure
    Block(Vec<Expr>),
    Let(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    Break(Box<Expr>),
    Return(Box<Expr>),
    // Operator
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

// Contexts

#[derive(Default)]
struct Context {
    global: Global,
    local: Function,
}

#[derive(Default)]
struct Global {
    idx: usize,
    data: String,
    func: IndexSet<Name>,
}

#[derive(Default)]
struct Function {
    var: IndexSet<Name>,
    jmp: Vec<String>,
}
