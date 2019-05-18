#[derive(Debug, Clone)]
struct Cursor<'a> {
    source: &'a str,
    cursor: (Option<char>, Option<char>),
    iter: std::str::Chars<'a>,
    pos: usize,
}

impl<'a> Cursor<'a> {
    fn new(source: &'a str) -> Self {
        let mut iter = source.chars();
        let cursor = (iter.next(), iter.next());

        Cursor {
            source,
            cursor,
            iter,
            pos: 0,
        }
    }

    fn tail(&self, len: usize) -> &'a str {
        self.source.get(self.pos + 1 - len..self.pos + 1).unwrap()
    }

    fn from_pos(&self, pos: usize) -> &'a str {
        self.source.get(pos..=self.pos).unwrap()
    }

    fn shift(&mut self) {
        let (_, la) = self.cursor;
        let next = self.iter.next();

        if la.is_some() { self.pos += 1 }
        self.cursor = (la, next);
    }

    fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.shift()
        }
    }

    fn is_empty(&self) -> bool {
        self.pos >= self.iter.as_str().len()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lexeme {
    Lparren,
    Rparren,
    Lbracket,
    Tbracket,
    Rbracket,
    Lbrace,
    Rbrace,

    Keyword(String),
    Word(String),
    Mword(String),
    Name(String),
    Underscore,
    Comma,
    Dot,
    Rarrow,
    Colon,
    Semicolon,
    Bar,
    Equals,

    Operator(String),

    String(String),
    Integer(String),
    Float(String),

    Comment(String),

    Indent(String),
    // Unindent(String),
    Whitespace(String),
    Newline,
    Eof,
}

fn is_keyword(kw: &str) -> bool {
    match kw {
        "use"
        | "data"
        | "forall"
        | "case"
        | "of"
        | "let" => true,
        _ => false,
    }
}

fn is_whitespace(w: char) -> bool {
    match w {
        ' ' | '\t' => true,
        _ => false,
    }
}

fn is_digit(d: char) -> bool {
    match d {
        '0' ... '9' => true,
        _ => false,
    }
}

fn is_hex_digit(d: char) -> bool {
    match d {
        'a' ... 'f'
        | 'A' ... 'F' => true,
        d => is_digit(d),
    }
}

fn is_letter(l: char) -> bool {
    match l {
        'a' ... 'z'
        | 'A' ... 'Z'
        | '_' => true,
        _ => false,
    }
}

fn is_special(s: char) -> bool {
    match s {
        '!' ... '&'
        | '*' ... '/'
        | ':' ... '@'
        | '\\' | '^'
        | '_' | '|'
        | '~' => true,
        _ => false,
    }
}

#[derive(Debug)]
pub enum LexerError {
    MysteriousChar,
    UnexpectedEof,
}

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    is_line_start: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let cursor = Cursor::new(source);

        Lexer {
            cursor,
            is_line_start: true,
        }
    }

    fn read_one<C>(&mut self, cond: C) -> bool
    where C: Fn((Option<char>, Option<char>)) -> bool
    {
        let cur = self.cursor.cursor;
        if cond(cur) && cur.0 != None {
            self.cursor.shift();
            true
        }
        else {
            false
        }
    }

    fn read_many<C>(&mut self, cond: C) -> usize
    where C: Fn((Option<char>, Option<char>)) -> bool
    {
        let mut len = 0;
        loop {
            let cur = self.cursor.cursor;
            if !cond(cur) || cur.0 == None { break }

            len += 1;
            self.cursor.shift()
        }

        len
    }

    fn whitespace(&mut self) -> Result<Option<Lexeme>, LexerError> {
        if self.read_one(|(c, _)| c == Some('\n')) {
            self.is_line_start = true;

            return Ok(Some(Lexeme::Newline))
        }

        let is_line_start = self.is_line_start;
        self.is_line_start = false;

        if !self.cursor.cursor.0
            .map(is_whitespace).unwrap_or(false)
        {
            return Ok(None)
        }

        let len = self.read_many(|(_, la)|
            la.map(is_whitespace).unwrap_or(false)
        );

        let ws = self.cursor.tail(len + 1).to_string();
        self.cursor.shift();

        if is_line_start {
            Ok(Some(Lexeme::Indent(ws)))
        } else {
            Ok(Some(Lexeme::Whitespace(ws)))
        }

    }

    pub fn next_token(&mut self) -> Result<Lexeme, LexerError> {
        if let Some(tok) = self.whitespace()? {
            return Ok(tok)
        }

        let (c, la) = self.cursor.cursor;
        let c = match c {
            Some(c) => c,
            None => return Ok(Lexeme::Eof),
        };

        use self::Lexeme::*;
        let tok = match c {
            '-' if la == Some('-') => self.comment(),
            '[' => Ok(Lbracket),
            'T' if la == Some('[') => {
                self.cursor.skip(1);
                Ok(Tbracket)
            },
            ']' => Ok(Rbracket),
            '{' => Ok(Lbrace),
            '}' => Ok(Rbrace),
            '(' => Ok(Lparren),
            ')' => Ok(Rparren),
            '"' => self.string(),
            '0' if la == Some('x') => self.hex_integer(),
            s if is_special(s) => self.operator(),
            l if is_letter(l) => self.word(),
            d if is_digit(d) => self.number(),
            _ => {
                return Err(LexerError::MysteriousChar);
            },
        };

        self.cursor.shift();

        tok
    }

    fn comment(&mut self) -> Result<Lexeme, LexerError> {
        let pos = self.cursor.pos;
        self.read_many(|(_, la)|
            la != Some('\n') && la != None
        );

        let comment = self.cursor.from_pos(pos).to_string();
        Ok(Lexeme::Comment(comment))
    }

    fn string(&mut self) -> Result<Lexeme, LexerError> {
        let pos = self.cursor.pos;
        self.read_many(|(c, la)|
            la != Some('\"') || c == Some('\\')
        );

        if !self.read_one(|(c, _)| c != None) {
            return Err(LexerError::UnexpectedEof)
        }

        let string = self.cursor.from_pos(pos).to_string();
        Ok(Lexeme::String(string))
    }

    fn hex_integer(&mut self) -> Result<Lexeme, LexerError> {
        let pos = self.cursor.pos;
        self.cursor.skip(2);

        self.read_many(|(c, _la)|
            c.map(is_hex_digit).unwrap_or(false)
        );

        let number = self.cursor.from_pos(pos).to_string();
        Ok(Lexeme::Integer(number))
    }

    fn operator(&mut self) -> Result<Lexeme, LexerError> {
        let pos = self.cursor.pos;
        self.read_many(|(c, _)|
            c.map(is_special).unwrap_or(false)
        );

        let operator = self.cursor.from_pos(pos);
        let lexeme = match operator {
            "," => Lexeme::Comma,
            "." => Lexeme::Dot,
            "->" => Lexeme::Rarrow,
            ":" => Lexeme::Colon,
            "|" => Lexeme::Bar,
            "=" => Lexeme::Equals,
            _ => Lexeme::Operator(operator.to_string())
        };

        Ok(lexeme)
    }

    fn word(&mut self) -> Result<Lexeme, LexerError> {
        let pos = self.cursor.pos;
        self.read_many(|(c, _)|
            c.map(|c| is_letter(c) || is_digit(c)).unwrap_or(false)
        );

        let is_mword = self.read_one(|(c, _)| c == Some('['));

        let word = self.cursor.from_pos(pos).to_string();
        let lexeme = match &*word {
            "_" => Lexeme::Underscore,
            _ if is_mword => Lexeme::Mword(word),
            _ if is_keyword(&*word) => Lexeme::Keyword(word),
            _ => Lexeme::Word(word),
        };

        Ok(lexeme)
    }

    fn number(&mut self) -> Result<Lexeme, LexerError> {
        let is_digit = |(c, _): (Option<char>, _)| {
            c.map(|c| is_digit(c)).unwrap_or(false)
        };

        let pos = self.cursor.pos;
        let mut is_integer = true;

        self.read_one(|(c, _)| c == Some('-'));
        self.read_many(is_digit);

        if self.read_one(|(c, _)| c == Some('.')) {
            is_integer = false;
            self.read_many(is_digit);
        }

        if self.read_one(|(c, _)| c == Some('e')) {
            is_integer = false;

            self.read_one(|(c, _)| c == Some('-'));
            self.read_many(is_digit);
        }

        let number = self.cursor.from_pos(pos).to_string();
        if is_integer {
            Ok(Lexeme::Integer(number))
        }
        else {
            Ok(Lexeme::Float(number))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Lexeme;
    use super::{Lexer};

    fn format_lexeme(lexeme: &Lexeme) -> &str {
        use Lexeme::*;
        let string = match lexeme {
            Lparren => "(",
            Rparren => ")",
            Lbracket => "[",
            Tbracket => "T[",
            Rbracket => "]",
            Lbrace => "{",
            Rbrace => "}",

            Underscore => "_",
            Comma => ",",
            Dot => ".",
            Rarrow => "->",
            Colon => ":",
            Semicolon => ";",
            Bar => "|",
            Equals => "=",
            Newline => "\n",

            Keyword(s)
            | Word(s)
            | Mword(s)
            | Name(s)
            | Operator(s)
            | String(s)
            | Integer(s)
            | Float(s)
            | Comment(s)
            | Indent(s)
            | Whitespace(s) => &*s,
            Eof => ""
        };

        string
    }

    #[test] fn string() {
        let string = "\"hello world\"";
        let mut lexer = Lexer::new(string);

        let lexeme = lexer.next_token().unwrap();
        let gold = Lexeme::String(string.to_string());

        assert_eq!(lexeme, gold)
    }

    #[test] fn all_lexemes() {
        let gold = concat!(
            "--\n",
            "(\n",
            "    [] T[]{}\n",
            "    use std;\n",
            "    _,. -> :; = |\n",
            "    0xff 2 * 2 / 0.5\n",
            "\n",
            "    \"Hello world\"\n",
            ")\n",
        );

        let mut lexer = Lexer::new(gold);
        let mut pretty = String::new();

        loop {
            let lexeme = lexer.next_token().unwrap();
            if lexeme == Lexeme::Eof { break }

            pretty.push_str(format_lexeme(&lexeme));
        }

        assert_eq!(pretty, gold)
    }
}
