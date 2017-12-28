use std::io;

use self::lexer::{Lexer, Token, Lexeme, LexerError, Position};

pub mod lexer;

#[derive(Debug, Default)]
pub struct SyntaxTree {
    pub root: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    //DataDef(Expr<TypeExpr>, Vec<TypeDecl>),
    FuncDef(Name, Option<StackEff>, Expr<FullExpr>, Option<Vec<Decl>>),
    FuncType(Name, Expr<TypeExpr>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T> {
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
pub enum TypeExpr {
    Word(Name),
    Multiword(Name),
    Function(Box<Expr<TypeExpr>>, Box<Expr<TypeExpr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StackEff {
    pub input: Vec<Name>,
    pub output: Vec<Name>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name(pub String, pub Position);

#[derive(Debug, Clone, PartialEq)]
pub struct Branch(Vec<Expr<Pattern>>, Expr<FullExpr>);

#[derive(Debug, Clone, PartialEq)]
pub enum FullExpr {
    Quote(Box<Expr<FullExpr>>),
    List(Box<Expr<FullExpr>>),

    Infix(Box<Expr<FullExpr>>, Name, Box<Expr<FullExpr>>),
    Simple(Simple),
    Lambda(Expr<Pattern>, Box<Expr<FullExpr>>),
    Block(Box<Expr<FullExpr>>),

    Case(Box<Expr<FullExpr>>, Vec<Branch>),
    If(Box<Expr<FullExpr>>, Box<Expr<FullExpr>>, Option<Box<Expr<FullExpr>>>),
}

impl From<Simple> for Expr<FullExpr> {
    fn from(source: Simple) -> Expr<FullExpr> {
        FullExpr::Simple(source).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Placeholder(Position),
    Simple(Simple),
}

impl From<Simple> for Expr<Pattern> {
    fn from(source: Simple) -> Expr<Pattern> {
        Pattern::Simple(source).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Simple {
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
    Decl(Decl),
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
    ExpectedIndentFound(Token),
    ExpectedUnindentFound(Token),
    ExpectedWordFound(Token),
    UnclosedIndent(Position),
    Stuck(Position),
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

    fn insert_symbol(&mut self, symbol: Symbol) {
        self.input.push((symbol, self.output.len()))
    }

    fn shift_lexer(&mut self) -> Result<Token, ParserError> {
        let mut tok = self.lexer.next_token()?;
        ::std::mem::swap(&mut tok, &mut self.cursor);

        Ok(tok)
    }

    fn shift(&mut self) -> Result<(), ParserError> {
        let tok = self.shift_lexer()?;
        self.insert_symbol(Symbol::Token(tok));

        Ok(())
    }

    fn last_input_pos(&self) -> Option<usize> {
        Some(self.input.last()?.1)
    }

    fn expect(&self, lexeme: Lexeme) -> Result<(), ParserError> {
        if self.cursor.lexeme == lexeme {
            Ok(())
        } else {
            Err(ParserError::ExpectedFound(vec![lexeme], self.cursor.clone()))
        }
    }

    fn expect_shift_lexer(&mut self, lexeme: Lexeme) -> Result<Token, ParserError> {
        self.expect(lexeme)?;
        self.shift_lexer()
    }

    fn decay(self) -> Token {
        self.cursor
    }

    fn terminate_default(&mut self, fallback: T) -> Result<T, ParserError> {
        match (self.input.is_empty(), self.output.len()) {
            (true, 0) =>
                Ok(fallback),
            (true, 1) =>
                self.output.pop()
                .ok_or(ParserError::Stuck(self.cursor.position)),
            _ =>
                Err(ParserError::Stuck(self.cursor.position)),
        }
    }

    fn terminate(&mut self) -> Result<T, ParserError> {
        if self.input.is_empty() && self.output.len() == 1 {
            self.output.pop()
            .ok_or(ParserError::Stuck(self.cursor.position))
        } else {
            Err(ParserError::Stuck(self.cursor.position))
        }
    }
}


fn parse_root<R: io::Read>(parser: &mut Parser<R, SyntaxTree>) -> Result<SyntaxTree, ParserError> {
    parser.output.push(Default::default());

    loop {
        println!("OUTPUT: {:?}", parser.output);
        let cond =
            reduce_decl(parser);
        if cond { continue }

        match parser.cursor.lexeme {
            Lexeme::Newline | Lexeme::Comment(_) =>
                drop(parser.shift_lexer()?),
            Lexeme::Eof =>
                break,
            _ => (),
        }

        let cond =
            check_word(parser)?;
        if cond {
            //parser.shift()?
        } else {
            break
        }
        println!("INPUT: {:?}", parser.input);
    }

    parser.terminate()
}

fn parse_in_block<R: io::Read, T1, T2, F>(parser: &mut Parser<R, T1>, func: F) -> Result<T2, ParserError>
where F: Fn(&mut Parser<R, T2>) -> Result<T2, ParserError>
{
    let indent = match parser.cursor.lexeme {
        Lexeme::Indent(n) => n,
        _ => return Err(ParserError::ExpectedIndentFound(parser.cursor.clone()))
    };

    let res = {
        let mut inner = Parser::new(parser.lexer)?;
        let res = func(&mut inner)?;
        parser.cursor = inner.decay();

        res
    };

    let unindent = match parser.cursor.lexeme {
        Lexeme::Unindent(n) => n,
        _ => return Err(ParserError::ExpectedUnindentFound(parser.cursor.clone()))
    };

    if unindent < indent {
        return Err(ParserError::UnclosedIndent(parser.cursor.position))
    } else if unindent == indent {
        parser.shift_lexer()?;
    } else {
        parser.cursor.lexeme = Lexeme::Unindent(unindent - indent)
    }

    Ok(res)
}

fn parse_stackeff<R: io::Read, T>(parser: &mut Parser<R, T>) -> Result<StackEff, ParserError> {
    let mut input = vec![];
    let mut output = vec![];

    let mut is_output = false;

    loop {
        match parser.cursor.lexeme {
            Lexeme::Word(ref word) if is_output =>
                output.push(Name(word.clone(), parser.cursor.position)),
            Lexeme::Word(ref word) =>
                input.push(Name(word.clone(), parser.cursor.position)),
            Lexeme::Operator(ref tilda) if &*tilda == "~" && !is_output =>
                is_output = true,
            _ =>
                break,
        }

        parser.shift_lexer()?;
    }

    Ok(StackEff {
        input, output
    })
}

fn parse_funcdef<R: io::Read, T>(parser: &mut Parser<R, T>, name: Name) -> Result<Decl, ParserError> {
    let eff =
        if parser.cursor.lexeme == Lexeme::Lparren {
            parser.shift_lexer()?;
            let eff = Some(parse_stackeff(parser)?);
            parser.expect_shift_lexer(Lexeme::Rparren)?;
            parser.expect_shift_lexer(Lexeme::Equals)?;

            eff
        } else {
            None
        };

    parser.shift_lexer()?;
    let expr = parse_in_block(parser, parse_fullexpr)?;

    Ok(Decl::FuncDef(name, eff, expr, None))
}

fn check_word<R: io::Read, T>(parser: &mut Parser<R, T>) -> Result<bool, ParserError> {
    if let Lexeme::Word(word) = parser.cursor.lexeme.clone() {
        let name = Name(word.clone(), parser.cursor.position);
        parser.shift_lexer()?;

        match parser.cursor.lexeme.clone() {
            Lexeme::Lparren | Lexeme::Equals => {
                let decl = parse_funcdef(parser, name)?;
                parser.insert_symbol(Symbol::Decl(decl));
            },
            Lexeme::Colon => {
                unimplemented!()
            },
            _ =>
                return Err(ParserError::ExpectedFound(
                    vec![Lexeme::Equals, Lexeme::Colon],
                    parser.cursor.clone()
                ))
        }

        Ok(true)
    } else {
        Ok(false)
    }
}

fn reduce_decl<R: io::Read>(parser: &mut Parser<R, SyntaxTree>) -> bool {
    match parser.input.pop() {
        Some((Symbol::Decl(decl), _)) => {
            parser.output.last_mut().unwrap().root.push(decl);
            true
        }
        Some(sym_n) => {
            parser.input.push(sym_n);
            false
        },
        None =>
            false,
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

    parser.terminate_default(Expr::Empty.into())
}


fn parse_fullexpr<R: io::Read>(parser: &mut Parser<R, Expr<FullExpr>>) -> Result<Expr<FullExpr>, ParserError> {
    loop {
        // println!("OUTPUT: {:?}", parser.output);
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
        // println!("INPUT: {:?}", parser.input);
    }

    parser.terminate_default(Expr::Empty)
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


//#[cfg(test)]
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

    impl Format for Name {
        fn format(&self, indent: usize) -> String {
            let s = format!("Name {:?}", self.0);
            format_indented(&s, indent)
        }
    }

    impl Format for StackEff {
        fn format(&self, indent: usize) -> String {
            let mut res = format_indented("Stackeff", indent);

            res.push_str(&format_indented("In", indent + 2));
            for w in &self.input {
                res.push_str(&w.format(indent + 4));
            }

            res.push_str(&format_indented("Out", indent + 2));
            for w in &self.output {
                res.push_str(&w.format(indent + 4));
            }

            res
        }
    }

    impl Format for Decl {
        fn format(&self, indent: usize) -> String {
            match self {
                &Decl::FuncDef(ref name, ref stack_eff, ref expr, _) => {
                    let mut res = format_indented("Funcdef", indent);

                    res.push_str(&name.format(indent + 2));
                    if let Some(ref eff) = *stack_eff {
                        res.push_str(&eff.format(indent + 2));
                    }
                    res.push_str(&format_indented("Expr", indent + 2));
                    res.push_str(&expr.format(indent + 4));

                    res
                },
                _ => unimplemented!()
            }
        }
    }

    impl Format for SyntaxTree {
        fn format(&self, indent: usize) -> String {
            let mut res = format_indented("Root", indent);
            for decl in &self.root {
                res.push_str(&decl.format(indent + 2));
            }

            res
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

    #[test] fn nop_program() {
        let expr = concat!(
            "nop ( ~ ) =\n",
            "    -- Just nothing.\n"
        ).as_bytes();

        let mut lexer = lexer::Lexer::new(expr).unwrap();
        let mut parser = Parser::new(&mut lexer).unwrap();
        let st = parse_root(&mut parser).unwrap();

        let pretty = st.format(0);
        let gold =
r#"Root
  Funcdef
    Name "nop"
    Stackeff
      In
      Out
    Expr
      Empty
"#;
        assert_eq!(gold, pretty);
    }
}
