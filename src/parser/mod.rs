use std::io;

use self::lexer::{Lexer, Token, Lexeme, LexerError, Position};

mod lexer;

#[derive(Debug)]
struct SyntaxTree {
    root: Vec<Decl>,
}

#[derive(Debug)]
enum Decl {
    FuncDef(Name, Option<StackEff>, FullExpr)
}

#[derive(Debug)]
struct Name(String, Position);

#[derive(Debug, Clone, PartialEq)]
enum FullExpr {
    Comp(Vec<FullExpr>),
    Conc(Vec<FullExpr>),
    Enclosed(Box<FullExpr>),
    Simple(Simple),
    Lambda(Pattern, Box<FullExpr>),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
struct Pattern {
    pos: Position,
}

#[derive(Debug, Clone, PartialEq)]
enum Simple {
    Word(String, Position),
    String(String, Position),
    Char(char, Position),
    Integer(String, Position),
    Float(String, Position)
}

fn into_simple(tok: Token) -> Result<Simple, Token> {
    match tok.lexeme {
        Lexeme::Word(w) => Ok(Simple::Word(w, tok.position)),
        Lexeme::Integer(i) => Ok(Simple::Integer(i, tok.position)),
        Lexeme::Float(f) => Ok(Simple::Float(f, tok.position)),
        Lexeme::String(s) =>Ok(Simple::String(s, tok.position)),
        Lexeme::Char(c) => Ok(Simple::Char(c, tok.position)),
        _ => Err(tok),
    }
}

#[derive(Debug)]
struct StackEff {
    input: Vec<Name>,
    output: Vec<Name>,
}

#[derive(Debug, Clone, PartialEq)]
enum Symbol {
    Token(Token),
    FullExpr(FullExpr),
}

impl Symbol {
    fn is_lexeme(&self, lex: Lexeme) -> bool {
        self.ref_lexeme() == Some(&lex)
    }

    fn ref_lexeme(&self) -> Option<&Lexeme> {
        match self {
            &Symbol::Token(ref tok) =>
                Some(&tok.lexeme),
            _ =>
                None,
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    ExpectedFound(Token, Token),
    Stuck,
}

impl ::std::convert::From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        ParserError::LexerError(err)
    }
}

#[derive(Debug)]
pub struct Parser<'a, R: 'a + io::Read, T> {
    lexer: &'a mut Lexer<R>,
    cursor: Token,
    input: Vec<(Symbol, usize)>,
    output: Vec<T>,
}

impl<'a, R: io::Read, T> Parser<'a, R, T> {
    pub fn new(lexer: &'a mut Lexer<R>) -> Result<Self, ParserError> {
        let tok = lexer.next_token()?;

        Ok(Parser {
            lexer,
            cursor: tok,
            input: vec![],
            output: vec![],
        })
    }

    fn shift_lexer(&mut self) -> Result<Token, ParserError> {
        let mut tok = self.lexer.next_token()?;
        ::std::mem::swap(&mut tok, &mut self.cursor);

        Ok(tok)
    }

    fn shift(&mut self) -> Result<(), ParserError> {
        let tok = self.shift_lexer()?;
        self.input.push((Symbol::Token(tok), self.output.len()));

        Ok(())
    }

    fn last_input_pos(&self) -> Option<usize> {
        Some(self.input.last()?.1)
    }

    fn terminate(&mut self) -> Option<T> {
        if self.input.is_empty() && self.output.len() == 1 {
            self.output.pop()
        } else {
            None
        }
    }
}

fn pop_conc<R: io::Read, T>(parser: &mut Parser<R, T>) -> Option<(T, T)> {
    if parser.output.len() < 2 { return None }

    let cond =
        parser.input.last()?.0.is_lexeme(Lexeme::Comma)
        && parser.input.last()?.1 < parser.output.len();
    if cond {
        drop(parser.input.pop());

        let e2 = parser.output.pop().unwrap();
        let e1 = parser.output.pop().unwrap();

        Some((e1, e2))
    } else {
        None
    }
}

fn pop_comp<R: io::Read, T>(parser: &mut Parser<R, T>) -> Option<(T, T)> {
    if parser.output.len() < 2 { return None }

    let cond = parser.last_input_pos().map(|p| p + 2 <= parser.output.len()).unwrap_or(true);
    if !cond { return None }

    match parser.cursor.lexeme {
        Lexeme::Comma | Lexeme::Operator(_) => {
            None
        },
        _ => {
            let e2 = parser.output.pop().unwrap();
            let e1 = parser.output.pop().unwrap();

            Some((e1, e2))
        },
    }
}

fn parse_fullexpr<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> Result<FullExpr, ParserError> {
    loop {
        // reduce
        let cond =
            reduce_simple_fe(parser)
            || reduce_conc_fe(parser)
            || reduce_comp_fe(parser);
        if cond { continue }

        // shift
        match parser.cursor.lexeme {
            Lexeme::Newline
            | Lexeme::Comment(_) =>
                drop(parser.shift_lexer()?),
            Lexeme::Eof =>
                break,
            _ => (),
        }

        let cond =
            check_basic(parser);
        if cond {
            parser.shift()?
        } else {
            break
        }
    }

    parser.terminate().ok_or(ParserError::Stuck)
}

fn check_basic<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> bool {
    use self::Lexeme;
    match parser.cursor.lexeme {
        Lexeme::Word(_)
        | Lexeme::Integer(_)
        | Lexeme::Float(_)
        | Lexeme::String(_)
        | Lexeme::Char(_)
        | Lexeme::Comma
        | Lexeme::Lparren => true,
        _ => false,
    }
}

fn reduce_conc_fe<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> bool {
    match pop_conc(parser) {
        Some((FullExpr::Conc(mut conc), e2)) => {
            conc.push(e2);
            parser.output.push(FullExpr::Conc(conc));
            true
        },
        Some((e1, e2)) => {
            parser.output.push(FullExpr::Conc(vec![e1, e2]));
            true
        }
        _ =>
            false,
    }
}

fn reduce_comp_fe<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> bool {
    match pop_comp(parser) {
        Some((FullExpr::Comp(mut comp), e2)) => {
            comp.push(e2);
            parser.output.push(FullExpr::Comp(comp));
            true
        },
        Some((e1,e2)) => {
            parser.output.push(FullExpr::Comp(vec![e1, e2]));
            true
        },
        None =>
            false,
    }
}

fn reduce_enclosed_fe<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> bool {
    unimplemented!()
}

fn reduce_simple_fe<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> bool {
    let sym = parser.input.pop();
    match sym {
        Some((Symbol::Token(tok), n)) => {
            match into_simple(tok) {
                Ok(simple) => {
                    parser.output.push(FullExpr::Simple(simple));
                    true
                },
                Err(tok) => {
                    parser.input.push((Symbol::Token(tok), n));
                    false
                },
            }
        },
        Some(sym) => {
            parser.input.push(sym);
            false
        }
        _ =>
            false,
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn format_indented(s: &str, indent: usize) -> String {
        let mut string = String::new();
        for _ in 0..indent {
            string.push(' ')
        }

        format!("{}{}\n", string, s)
    }

    fn format_fullexpr(expr: &FullExpr, indent: usize) -> String {
        match expr {
            &FullExpr::Simple(Simple::String(ref s, _)) => {
                let s = format!("String {:?}", s);
                format_indented(&s, indent)
            },
            &FullExpr::Simple(Simple::Char(ref c, _)) => {
                let s = format!("Char {:?}", c);
                format_indented(&s, indent)
            },
            &FullExpr::Simple(Simple::Float(ref f, _)) => {
                let s = format!("Float {:?}", f);
                format_indented(&s, indent)
            },
            &FullExpr::Simple(Simple::Integer(ref i, _)) => {
                let s = format!("Integer {:?}", i);
                format_indented(&s, indent)
            },
            &FullExpr::Simple(Simple::Word(ref w, _)) => {
                let s = format!("Word {:?}", w);
                format_indented(&s, indent)
            },
            &FullExpr::Comp(ref comp) => {
                let mut res = format_indented("Comp", indent);
                for e in comp {
                    let t = format_fullexpr(&e, indent + 2);
                    res.push_str(&t);
                }
                res
            },
            &FullExpr::Conc(ref conc) => {
                let mut res = format_indented("Conc", indent);
                for e in conc {
                    let t = format_fullexpr(&e, indent + 2);
                    res.push_str(&t);
                }
                res
            },
            &FullExpr::Enclosed(ref e) => {
                let mut res = format_indented("Enclosed", indent);
                let t = format_fullexpr(&e, indent);
                res.push_str(&t);
                res
            },
            &FullExpr::Empty => {
                format_indented("Empty", indent)
            },
            _ => unimplemented!(),
        }
    }

    #[test] fn abcdef_expr() {
        let expr = "a b,c d,e f\n".as_bytes();

        let mut lexer = lexer::Lexer::new(expr).unwrap();
        let mut parser = Parser::new(&mut lexer).unwrap();
        let expr = parse_fullexpr(&mut parser).unwrap();

        let pretty = format_fullexpr(&expr, 0);
        let gold =
    // Raw string litreals are painful
r#"Comp
  Word "a"
  Conc
    Word "b"
    Word "c"
  Conc
    Word "d"
    Word "e"
  Word "f"
"#;
// Seriously,
    // they are
        // very painful
        assert_eq!(gold, pretty);
    }

    // #[test] fn hello_expr() {
    //     let expr = concat!(
    //         "\"Hello world\" -> hello\n",
    //         "hello print_ln\n"
    //     ).as_bytes();

    //     let mut lexer = lexer::Lexer::new(expr).unwrap();
    //     let mut parser = Parser::new(&mut lexer).unwrap();
    //     let expr = parse_fullexpr(&mut parser).unwrap();

    //     assert_eq!(FullExpr::Empty, expr);
    // }
}