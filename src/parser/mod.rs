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
pub struct Parser<'a, R: 'a + io::Read> {
    lexer: &'a mut Lexer<R>,
    stack: Vec<Symbol>,
    cursor: (Token, Token),
}

impl<'a, R: io::Read> Parser<'a, R> {
    pub fn new(lexer: &'a mut Lexer<R>) -> Result<Self, ParserError> {
        let tok = lexer.next_token()?;
        let la = lexer.next_token()?;

        Ok(Parser {
            lexer,
            stack: vec![],
            cursor: (tok, la),
        })
    }

    fn shift_lexer(&mut self) -> Result<Token, ParserError> {
        let mut tok = self.lexer.next_token()?;
        ::std::mem::swap(&mut tok, &mut self.cursor.1);
        ::std::mem::swap(&mut tok, &mut self.cursor.0);

        Ok(tok)
    }

    fn shift(&mut self) -> Result<(), ParserError> {
        let tok = self.shift_lexer()?;
        self.stack.push(Symbol::Token(tok));

        Ok(())
    }

    fn terminate(&mut self) -> Option<Symbol> {
        if self.stack.len() == 1 {
            self.stack.pop()
        } else {
            None
        }
    }
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

fn symbol_token_lexeme(sym: &Symbol, lex: Lexeme) -> bool {
    match sym {
        &Symbol::Token(ref t) =>
            t.lexeme == lex,
        _ => false,
    }
}

fn pop_token(stack: &mut Vec<Symbol>) -> Option<Token> {
    let v = stack.pop();

    match v {
        Some(Symbol::Token(tok)) => Some(tok),
        Some(sym) => {
            stack.push(sym);
            None
        }
        _ => None,
    }
}

fn pop_simple(stack: &mut Vec<Symbol>) -> Option<Simple> {
    let v = stack.pop();

    match v {
        Some(Symbol::FullExpr(FullExpr::Simple(s))) => Some(s),
        Some(sym) => {
            stack.push(sym);
            None
        }
        _ => None,
    }
}

fn pop_fullexpr(stack: &mut Vec<Symbol>) -> Option<FullExpr> {
    let v = stack.pop();

    match v {
        Some(Symbol::FullExpr(e)) => Some(e),
        Some(sym) => {
            stack.push(sym);
            None
        }
        _ => None,
    }
}

fn pop_lexeme(stack: &mut Vec<Symbol>, lex: Lexeme) -> Option<Token> {
    let tok = pop_token(stack)?;

    if tok.lexeme == lex {
        Some(tok)
    } else {
        stack.push(Symbol::Token(tok));
        None
    }
}

fn pop_conc(stack: &mut Vec<Symbol>) -> Option<(FullExpr, FullExpr)> {
    let e2 = pop_fullexpr(stack)?;

    let comma = match pop_lexeme(stack, Lexeme::Comma) {
        Some(c) => c,
        None => {
            stack.push(Symbol::FullExpr(e2));
            return None
        },
    };

    let e1 = match pop_fullexpr(stack) {
        Some(e) => e,
        None => {
            stack.push(Symbol::Token(comma));
            stack.push(Symbol::FullExpr(e2));
            return None
        },
    };

    Some((e1, e2))
}

fn pop_comp(stack: &mut Vec<Symbol>) -> Option<(FullExpr, FullExpr)> {
    let e2 = pop_fullexpr(stack)?;

    let e1 = match pop_fullexpr(stack) {
        Some(e) => e,
        None => {
            stack.push(Symbol::FullExpr(e2));
            return None
        },
    };

    Some((e1, e2))
}

fn parse_fullexpr<R: io::Read>(parser: &mut Parser<R>) -> Result<FullExpr, ParserError> {
    loop {
        // reduce
        let cond =
            reduce_simple(parser)
            || reduce_conc(parser)
            || reduce_comp(parser);
        if cond { continue }

        // shift
        match parser.cursor.0.lexeme {
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

    match parser.terminate() {
        Some(Symbol::FullExpr(fe)) => Ok(fe),
        Some(s) => {
            parser.stack.push(s);
            Err(ParserError::Stuck)
        },
        None => Err(ParserError::Stuck),
    }
}

fn check_basic<R: io::Read>(parser: &mut Parser<R>) -> bool {
    use self::Lexeme;
    match parser.cursor.0.lexeme {
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

fn reduce_conc<R: io::Read>(parser: &mut Parser<R>) -> bool {
    if let Lexeme::Operator(_) = parser.cursor.0.lexeme {
        return false
    }

    match pop_conc(&mut parser.stack) {
        Some((FullExpr::Conc(mut conc), expr)) => {
            conc.push(expr);
            parser.stack.push(Symbol::FullExpr(FullExpr::Conc(conc)));
            return true
        },
        Some((e1, e2)) => {
            let conc = vec![e1, e2];
            parser.stack.push(Symbol::FullExpr(FullExpr::Conc(conc)));
            return true
        },
        _ => return false,
    }
}

fn reduce_comp<R: io::Read>(parser: &mut Parser<R>) -> bool {
    match parser.cursor.0.lexeme {
        Lexeme::Operator(_)
        | Lexeme::Comma =>
            return false,
        _ => (),
    }

    match pop_comp(&mut parser.stack) {
        Some((FullExpr::Comp(mut comp), expr)) => {
            comp.push(expr);
            parser.stack.push(Symbol::FullExpr(FullExpr::Comp(comp)));
            true
        },
        Some((e1, e2)) => {
            let comp = vec![e1, e2];
            parser.stack.push(Symbol::FullExpr(FullExpr::Comp(comp)));
            true
        },
        _ => false
    }
}

fn reduce_simple<R: io::Read>(parser: &mut Parser<R>) -> bool {
    if let Some(tok) = pop_token(&mut parser.stack) {
        match into_simple(tok) {
            Ok(s) => {
                parser.stack.push(Symbol::FullExpr(FullExpr::Simple(s)));
                true
            }
            Err(tok) => {
                parser.stack.push(Symbol::Token(tok));
                false
            }
        }
    } else {
        false
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::lexer;

    #[test] fn abcdef_expr() {
        let expr = "a b,c d,e f\n".as_bytes();

        let mut lexer = lexer::Lexer::new(expr).unwrap();
        let mut parser = Parser::new(&mut lexer).unwrap();
        let expr = parse_fullexpr(&mut parser).unwrap();

        assert_eq!(FullExpr::Empty, expr);
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