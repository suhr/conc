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
enum Expr<T> {
    Plain(T),
    Empty,

    Comp(Vec<Expr<T>>),
    Conc(Vec<Expr<T>>),
    Enclosed(Box<Expr<T>>),
    Mexpr(Name, Box<Expr<T>>),
    Tuple(Box<Expr<T>>),
}

impl<T> From<T> for Expr<T> {
    fn from(source: T) -> Expr<T> {
        Expr::Plain(source)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TypeExpr {
    Word(Name),
    Multiword(Name),
    Function(Box<Expr<TypeExpr>>, Box<Expr<TypeExpr>>),}

#[derive(Debug, Clone, PartialEq)]
struct StackEff {
    input: Vec<Name>,
    output: Vec<Name>,
}

#[derive(Debug, Clone, PartialEq)]
struct Name(String, Position);

#[derive(Debug, Clone, PartialEq)]
enum FullExpr {
    Quote(Box<Expr<FullExpr>>),
    List(Box<Expr<FullExpr>>),

    Infix(Box<Expr<FullExpr>>, Name, Box<Expr<FullExpr>>),
    Simple(Simple),
    Lambda(Expr<Pattern>, Box<Expr<FullExpr>>),
}

impl From<Simple> for Expr<FullExpr> {
    fn from(source: Simple) -> Expr<FullExpr> {
        FullExpr::Simple(source).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Pattern {
    Placeholder(Position),
    Simple(Simple),
}

impl From<Simple> for Expr<Pattern> {
    fn from(source: Simple) -> Expr<Pattern> {
        Pattern::Simple(source).into()
    }
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
    Pattern(Expr<Pattern>),
    FullExpr(Expr<FullExpr>),
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

fn pop_lambda<R: io::Read>(parser: &mut Parser<R, Expr<FullExpr>>) -> Option<(Expr<Pattern>, Expr<FullExpr>)> {
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

fn parse_pattern<R: io::Read>(parser: &mut Parser<R, Expr<Pattern>>, multi_line: bool) -> Result<Expr<Pattern>, ParserError> {
    loop {
        let cond =
            reduce_simple(parser)
            || reduce_enclosed(parser)
            || reduce_conc(parser)
            || reduce_comp(parser);
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

    parser.terminate_default(Expr::Empty.into()).ok_or(ParserError::Stuck)
}


fn parse_fullexpr<R: io::Read>(parser: &mut Parser<R, Expr<FullExpr>>) -> Result<Expr<FullExpr>, ParserError> {
    loop {
        //println!("OUTPUT: {:?}", parser.output);
        // reduce
        let cond =
            reduce_simple(parser)
            || reduce_enclosed(parser)
            || reduce_conc(parser)
            || reduce_comp(parser);
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

    parser.terminate_default(Expr::Empty).ok_or(ParserError::Stuck)
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

fn check_lambda<R: io::Read>(parser: &mut Parser<R, Expr<FullExpr>>) -> Result<bool, ParserError> {
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

fn reduce_simple<R: io::Read, T: From<Simple>>(parser: &mut Parser<R, T>) -> bool {
    let sym = parser.input.pop();
    match sym {
        Some((Symbol::Token(tok), n)) => {
            match into_simple(tok) {
                Ok(simple) => {
                    parser.output.push(simple.into());
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

fn reduce_conc<R: io::Read, T>(parser: &mut Parser<R, Expr<T>>) -> bool {
    match pop_conc(parser) {
        Some((Expr::Conc(mut conc), e2)) => {
            conc.push(e2);
            parser.output.push(Expr::Conc(conc));
            true
        },
        Some((e1, e2)) => {
            parser.output.push(Expr::Conc(vec![e1, e2]));
            true
        }
        _ =>
            false,
    }
}

fn reduce_comp<R: io::Read, T>(parser: &mut Parser<R, Expr<T>>) -> bool {
    match pop_comp(parser) {
        Some((Expr::Comp(mut comp), e2)) => {
            comp.push(e2);
            parser.output.push(Expr::Comp(comp));
            true
        },
        Some((e1,e2)) => {
            parser.output.push(Expr::Comp(vec![e1, e2]));
            true
        },
        None =>
            false,
    }
}

fn reduce_enclosed<R: io::Read, T>(parser: &mut Parser<R, Expr<T>>) -> bool {
    match pop_enclosed(parser) {
        Some(Some(e)) => {
            parser.output.push(Expr::Enclosed(Box::new(e)));
            true
        },
        Some(None) => {
            let e = Expr::Empty;
            parser.output.push(Expr::Enclosed(Box::new(e)));
            true
        }
        None =>
            false,
    }
}

fn reduce_lambda<R: io::Read>(parser: &mut Parser<R, Expr<FullExpr>>) -> bool {
    match pop_lambda(parser) {
        Some((pat, expr)) => {
            parser.output.push(FullExpr::Lambda(pat, Box::new(expr)).into());
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

    trait Format {
        fn format(&self, indent: usize) -> String;
    }

    fn format_indented(s: &str, indent: usize) -> String {
        let mut string = String::new();
        for _ in 0..indent {
            string.push(' ')
        }

        format!("{}{}\n", string, s)
    }

    impl Format for Simple {
        fn format(&self, indent: usize) -> String {
            match self {
                &Simple::String(ref s, _) => {
                    let s = format!("String {:?}", s);
                    format_indented(&s, indent)
                },
                &Simple::Char(ref c, _) => {
                    let s = format!("Char {:?}", c);
                    format_indented(&s, indent)
                },
                &Simple::Float(ref f, _) => {
                    let s = format!("Float {:?}", f);
                    format_indented(&s, indent)
                },
                &Simple::Integer(ref i, _) => {
                    let s = format!("Integer {:?}", i);
                    format_indented(&s, indent)
                },
                &Simple::Word(ref w, _) => {
                    let s = format!("Word {:?}", w);
                    format_indented(&s, indent)
                },
            }
        }
    }

    impl<T: Format> Format for Expr<T> {
        fn format(&self, indent: usize) -> String {
            match self {
                &Expr::Plain(ref v) => v.format(indent),
                &Expr::Comp(ref comp) => {
                    let mut res = format_indented("Comp", indent);
                    for e in comp {
                        res.push_str(&e.format(indent + 2));
                    }
                    res
                },
                &Expr::Conc(ref conc) => {
                    let mut res = format_indented("Conc", indent);
                    for e in conc {
                        res.push_str(&e.format(indent + 2));
                    }
                    res
                },
                &Expr::Enclosed(ref e) => {
                    let mut res = format_indented("Enclosed", indent);
                    res.push_str(&e.format(indent + 2));
                    res
                },
                &Expr::Mexpr(ref w, ref e) => {
                    let mut res = format_indented("MExpr", indent);
                    res.push_str(&format_indented(&format!("Word {}", w.0), indent + 2));
                    res.push_str(&e.format(indent + 2));
                    res
                },
                &Expr::Tuple(ref e) => {
                    let mut res = format_indented("Enclosed", indent);
                    res.push_str(&e.format(indent + 2));
                    res
                },
                &Expr::Empty => {
                    format_indented("Empty", indent)
                },
            }
        }
    }

    impl Format for Pattern {
        fn format(&self, indent: usize) -> String {
            match self {
                &Pattern::Placeholder(_) => {
                    format_indented("Placeholder", indent)
                }
                &Pattern::Simple(ref s) =>
                    s.format(indent)
            }
        }
    }

    impl Format for FullExpr {
        fn format(&self, indent: usize) -> String {
            match self {
                &FullExpr::Simple(ref s) =>
                    s.format(indent),
                &FullExpr::Lambda(ref pat, ref e) => {
                    let mut res = format_indented("Lambda", indent);

                    res.push_str(&format_indented("Pattern", indent + 2));
                    res.push_str(&pat.format(indent + 4));
                    res.push_str(&format_indented("Expr", indent + 2));
                    res.push_str(&e.format(indent + 4));

                    res
                },
                _ => unimplemented!(),
            }
        }
    }

    #[test] fn abcdef_expr() {
        let expr = "a b,c d,e f\n".as_bytes();

        let mut lexer = lexer::Lexer::new(expr).unwrap();
        let mut parser = Parser::new(&mut lexer).unwrap();
        let expr = parse_fullexpr(&mut parser).unwrap();

        let pretty = expr.format(0);
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

        let pretty = expr.format(0);
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

        let pretty = expr.format(0);
        let gold =
r#"Comp
  String "Hello world"
  Lambda
    Pattern
      Word "hello"
    Expr
      Comp
        Word "hello"
        Word "print_ln"
"#;
        assert_eq!(gold, pretty);
    }
}
