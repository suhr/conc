use std::io;

use self::lexer::{Lexer, Token, Lexeme, LexerError, Position};

mod lexer;

#[derive(Debug)]
struct SyntaxTree {
    root: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
enum Decl {
    FuncDef(Name, Option<StackEff>, FullExpr),
    FuncType(Name, TypeExpr)
}

#[derive(Debug, Clone, PartialEq)]
enum TypeExpr {
    Word(Name),
    Multiword(Name),
    Function(Box<TypeExpr>, Box<TypeExpr>),

    Comp(Vec<TypeExpr>),
    Conc(Vec<TypeExpr>),
    Enclosed(Box<TypeExpr>),
    Mexpr(Name, Box<FullExpr>),
    Tuple(Box<FullExpr>),

    Empty
}

#[derive(Debug, Clone, PartialEq)]
struct StackEff {
    input: Vec<Name>,
    output: Vec<Name>,
}

#[derive(Debug, Clone, PartialEq)]
struct Name(String, Position);

#[derive(Debug, Clone, PartialEq)]
enum FullExpr {
    Comp(Vec<FullExpr>),
    Conc(Vec<FullExpr>),

    Enclosed(Box<FullExpr>),
    Quote(Box<FullExpr>),
    Mexpr(Name, Box<FullExpr>),
    Tuple(Box<FullExpr>),
    List(Box<FullExpr>),

    Infix(Box<FullExpr>, Name, Box<FullExpr>),
    Simple(Simple),
    Lambda(Pattern, Box<FullExpr>),

    Empty,
}

#[derive(Debug, Clone, PartialEq)]
enum Pattern {
    Placeholder(Position),

    Comp(Vec<Pattern>),
    Conc(Vec<Pattern>),
    Enclosed(Box<Pattern>),
    Mexpr(Name, Box<FullExpr>),
    Tuple(Box<FullExpr>),

    Simple(Simple),

    Empty,
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

#[derive(Debug, Clone, PartialEq)]
enum Symbol {
    Token(Token),
    Pattern(Pattern),
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
    ExpectedFound(Vec<Lexeme>, Token),
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

    fn decay(self) -> Token {
        self.cursor
    }

    fn terminate_default(&mut self, fallback: T) -> Option<T> {
        match (self.input.is_empty(), self.output.len()) {
            (true, 0) => Some(fallback),
            (true, 1) => self.output.pop(),
            _ => None,
        }
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

fn pop_enclosed<R: io::Read, T>(parser: &mut Parser<R, T>) -> Option<Option<T>> {
    if parser.input.len() < 2 { return None }

    let (lp_n, rp_n) = {
        let &(ref lp, lp_n) = parser.input.get(parser.input.len() - 2)?;
        let &(ref rp, rp_n) = parser.input.get(parser.input.len() - 1)?;

        if !lp.is_lexeme(Lexeme::Lparren) || !rp.is_lexeme(Lexeme::Rparren) {
            return None
        }

        (lp_n, rp_n)
    };

    let out_len = parser.output.len();
    if lp_n + 1 == out_len && rp_n == out_len {
        drop(parser.input.pop());
        drop(parser.input.pop());
        Some(parser.output.pop())
    } else if lp_n == out_len && rp_n == out_len {
        drop(parser.input.pop());
        drop(parser.input.pop());
        Some(None)
    } else {
        None
    }

}

fn pop_lambda<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> Option<(Pattern, FullExpr)> {
    let pat = match parser.input.pop()? {
        (Symbol::Pattern(pat), n) => {
            if n + 1 == parser.output.len() {
                pat
            } else {
                parser.input.push((Symbol::Pattern(pat), n));
                return None
            }
        },
        sym_pair => {
            parser.input.push(sym_pair);
            return None
        },
    };

    let expr = parser.output.pop().unwrap();

    Some((pat, expr))
}

fn parse_pattern<R: io::Read>(parser: &mut Parser<R, Pattern>, multi_line: bool) -> Result<Pattern, ParserError> {
    loop {
        let cond =
            reduce_simple_pat(parser)
            || reduce_enclosed_pat(parser)
            || reduce_conc_pat(parser)
            || reduce_comp_pat(parser);
        if cond { continue }

        match parser.cursor.lexeme {
            Lexeme::Newline if multi_line =>
                drop(parser.shift_lexer()?),
            Lexeme::Comment(_) =>
                drop(parser.shift_lexer()?),
            Lexeme::Eof =>
                break,
            _ => (),
        }

        let cond =
            check_basic(parser)
            || check_rparren(parser);
        if cond {
            parser.shift()?
        } else {
            break
        }
    }

    parser.terminate_default(Pattern::Empty).ok_or(ParserError::Stuck)
}

fn reduce_simple_pat<R: io::Read>(parser: &mut Parser<R, Pattern>) -> bool {
    let sym = parser.input.pop();
    match sym {
        Some((Symbol::Token(tok), n)) => {
            match into_simple(tok) {
                Ok(simple) => {
                    parser.output.push(Pattern::Simple(simple));
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

fn reduce_conc_pat<R: io::Read>(parser: &mut Parser<R, Pattern>) -> bool {
    match pop_conc(parser) {
        Some((Pattern::Conc(mut conc), e2)) => {
            conc.push(e2);
            parser.output.push(Pattern::Conc(conc));
            true
        },
        Some((e1, e2)) => {
            parser.output.push(Pattern::Conc(vec![e1, e2]));
            true
        }
        _ =>
            false,
    }
}

fn reduce_comp_pat<R: io::Read>(parser: &mut Parser<R, Pattern>) -> bool {
    match pop_comp(parser) {
        Some((Pattern::Comp(mut comp), e2)) => {
            comp.push(e2);
            parser.output.push(Pattern::Comp(comp));
            true
        },
        Some((e1,e2)) => {
            parser.output.push(Pattern::Comp(vec![e1, e2]));
            true
        },
        None =>
            false,
    }
}

fn reduce_enclosed_pat<R: io::Read>(parser: &mut Parser<R, Pattern>) -> bool {
    match pop_enclosed(parser) {
        Some(Some(e)) => {
            parser.output.push(Pattern::Enclosed(Box::new(e)));
            true
        },
        Some(None) => {
            let e = Pattern::Empty;
            parser.output.push(Pattern::Enclosed(Box::new(e)));
            true
        }
        None =>
            false,
    }
}

fn parse_fullexpr<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> Result<FullExpr, ParserError> {
    loop {
        //println!("OUTPUT: {:?}", parser.output);
        // reduce
        let cond =
            reduce_simple_fe(parser)
            || reduce_enclosed_fe(parser)
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
            check_basic(parser)
            || check_rparren(parser)
            || check_lambda(parser)?;
        if cond {
            parser.shift()?
        } else {
            reduce_lambda(parser)
            || break;
        }
        //println!("INPUT: {:?}", parser.input);
    }

    parser.terminate_default(FullExpr::Empty).ok_or(ParserError::Stuck)
}

fn check_basic<R: io::Read, T>(parser: &mut Parser<R, T>) -> bool {
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

fn check_rparren<R: io::Read, T>(parser: &mut Parser<R, T>) -> bool {
    if parser.cursor.lexeme != Lexeme::Rparren {
        return false
    }

    match parser.input.last() {
        Some(&(Symbol::Token(ref tok), lp_n)) => {
            tok.lexeme == Lexeme::Lparren
            && lp_n + 1 >= parser.output.len()
        },
        _ =>
            false,
    }
}

fn check_lambda<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> Result<bool, ParserError> {
    if parser.cursor.lexeme != Lexeme::Rarrow {
        return Ok(false)
    }

    {
        let mut pat_parser = Parser::new(parser.lexer)?;
        let pattern = parse_pattern(&mut pat_parser, false)?;
        parser.input.push((Symbol::Pattern(pattern), parser.output.len()));
        parser.cursor = pat_parser.decay();
    }

    match parser.cursor.lexeme {
        Lexeme::Newline | Lexeme::Colon => {
            drop(parser.shift_lexer());
            Ok(true)
        },
        _ => Err(ParserError::ExpectedFound(
            vec![Lexeme::Newline, Lexeme::Colon],
            parser.cursor.clone()
        ))
    }
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
    match pop_enclosed(parser) {
        Some(Some(e)) => {
            parser.output.push(FullExpr::Enclosed(Box::new(e)));
            true
        },
        Some(None) => {
            let e = FullExpr::Empty;
            parser.output.push(FullExpr::Enclosed(Box::new(e)));
            true
        }
        None =>
            false,
    }
}

fn reduce_lambda<R: io::Read>(parser: &mut Parser<R, FullExpr>) -> bool {
    match pop_lambda(parser) {
        Some((pat, expr)) => {
            parser.output.push(FullExpr::Lambda(pat, Box::new(expr)));
            true
        },
        _ => {
            false
        },
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

    fn format_pattern(pat: &Pattern, indent: usize) -> String {
        match pat {
            &Pattern::Simple(Simple::String(ref s, _)) => {
                let s = format!("String {:?}", s);
                format_indented(&s, indent)
            },
            &Pattern::Simple(Simple::Char(ref c, _)) => {
                let s = format!("Char {:?}", c);
                format_indented(&s, indent)
            },
            &Pattern::Simple(Simple::Float(ref f, _)) => {
                let s = format!("Float {:?}", f);
                format_indented(&s, indent)
            },
            &Pattern::Simple(Simple::Integer(ref i, _)) => {
                let s = format!("Integer {:?}", i);
                format_indented(&s, indent)
            },
            &Pattern::Simple(Simple::Word(ref w, _)) => {
                let s = format!("Word {:?}", w);
                format_indented(&s, indent)
            },
            &Pattern::Comp(ref comp) => {
                let mut res = format_indented("Comp", indent);
                for e in comp {
                    let t = format_pattern(&e, indent + 2);
                    res.push_str(&t);
                }
                res
            },
            &Pattern::Conc(ref conc) => {
                let mut res = format_indented("Conc", indent);
                for e in conc {
                    let t = format_pattern(&e, indent + 2);
                    res.push_str(&t);
                }
                res
            },
            &Pattern::Enclosed(ref e) => {
                let mut res = format_indented("Enclosed", indent);
                let t = format_pattern(&e, indent + 2);
                res.push_str(&t);
                res
            },
            &Pattern::Empty => {
                format_indented("Empty", indent)
            },
            _ => unimplemented!(),
        }
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
                let t = format_fullexpr(&e, indent + 2);
                res.push_str(&t);
                res
            },
            &FullExpr::Empty => {
                format_indented("Empty", indent)
            },
            &FullExpr::Lambda(ref pat, ref e) => {
                let mut res = format_indented("Lambda", indent);

                res.push_str(&format_indented("Pattern", indent + 2));
                res.push_str(&format_pattern(pat, indent + 4));
                res.push_str(&format_indented("Expr", indent + 2));
                res.push_str(&format_fullexpr(e, indent + 4));

                res
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

    #[test] fn lisp_expr() {
        let expr = "(()(lisp)),()".as_bytes();

        let mut lexer = lexer::Lexer::new(expr).unwrap();
        let mut parser = Parser::new(&mut lexer).unwrap();
        let expr = parse_fullexpr(&mut parser).unwrap();

        let pretty = format_fullexpr(&expr, 0);
        let gold =
r#"Conc
  Enclosed
    Comp
      Enclosed
        Empty
      Enclosed
        Word "lisp"
  Enclosed
    Empty
"#;
        assert_eq!(gold, pretty);
    }

    #[test] fn hello_expr() {
        let expr = concat!(
            "\"Hello world\" -> hello\n",
            "hello print_ln\n"
        ).as_bytes();

        let mut lexer = lexer::Lexer::new(expr).unwrap();
        let mut parser = Parser::new(&mut lexer).unwrap();
        let expr = parse_fullexpr(&mut parser).unwrap();

        let pretty = format_fullexpr(&expr, 0);

        assert_eq!("", pretty);
    }
}