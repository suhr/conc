use super::parser::*;
use super::parser::lexer::Position;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
struct Function {
    scheme: (),
    input: (),
    output: (),
}

#[derive(Debug, Clone, PartialEq)]
struct Entry {
    code: FullExpr,
    refer_to: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Simple {
    Word(String, Position),
    String(String, Position),
    Char(char, Position),
    Integer(String, Position),
    Float(String, Position)
}

#[derive(Debug, Clone, PartialEq)]
enum Literal {
    String(String, Position),
    Char(char, Position),
    Integer(String, Position),
    Float(String, Position)
}

#[derive(Debug, Clone, PartialEq)]
struct Word {
    word: String,
    position: Position,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Soundness {
    Ok,
    Nonsense,
}

#[derive(Debug, Clone, PartialEq)]
struct ExprCtx {
    soundness: Soundness,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Branch(Vec<Pattern>, FullExpr);

#[derive(Debug, Clone, PartialEq)]
enum FullExpr {
    Comp(Vec<FullExpr>, ExprCtx),
    Conc(Vec<FullExpr>, ExprCtx),
    Quote(Box<FullExpr>, ExprCtx),
    Lambda(Box<FullExpr>, ExprCtx),
    Case(usize, Vec<Branch>),
    If(Box<FullExpr>, Option<Box<FullExpr>>, ExprCtx),
    Literal(Literal, ExprCtx),
    Word(Word),
    Empty
}

#[derive(Debug, Clone, PartialEq)]
enum Pattern {
    Comp(Vec<Pattern>),
    Conc(Vec<Pattern>),
    Placeholder,
    Literal(Literal),
    Word(Word),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
enum TypeExpr<T> {
    Comp(Vec<T>),
    Conc(Vec<T>),
    Word(Word),
    Multiword(Word),
    Function(Function),
    Empty,
}
