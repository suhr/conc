use std::io::{self, Chars, BufReader, BufRead, Seek};
use std::fs::File;

#[derive(Debug, Clone, Copy)]
pub struct Position {
    from: (usize, usize, usize),
    to: (usize, usize, usize)
}

impl Position {
    fn point(abs: usize, line: usize, column: usize) -> Self {
        Position {
            from: (abs, line, column),
            to: (abs, line, column),
        }
    }
}

#[derive(Debug, Clone)]
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
    Underscore,
    Comma,
    Dot,
    Larrow,
    Colon,
    Bar,
    Equals,

    Operator(String),

    String(String),
    Char(char),
    Integer(String),
    Float(String),

    Comment(String),
    DocComment(String),
    TopDocComment(String),

    Indent,
    Unindent,
    Newline,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    lexeme: Lexeme,
    position: Position,
}

// There's no stream regexps for Rust, so I have to implement a FSM myself
#[derive(Debug, Clone, Copy)]
enum LexerState {
    Inital,
    MaybeInt,
    MaybeFloat,

}

#[derive(Debug)]
pub enum LexerError {
    NotUtf8,
    IoError(io::Error),
    UnexpectedEof(Position),
    InvalidInteger(Position),
    MysteriousChar(Position),
}

impl ::std::convert::From<io::CharsError> for LexerError {
    fn from(err: io::CharsError) -> Self {
        use self::io::CharsError::*;
        match err {
            NotUtf8 => LexerError::NotUtf8,
            Other(e) => LexerError::IoError(e),
        }
    }
}

fn is_keyword(kw: &str) -> bool {
    match kw {
        "data"
        | "forall"
        | "case"
        | "where"
        | "if"
        | "else"
        | "infix"
        | "infixl"
        | "infixr" => true,
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
        | ';' ... '@'
        | '\\' | '^' 
        | '_' | '|' 
        | '~' => true,
        _ => false,
    }
}

#[derive(Debug)]
struct Lexer {
    char_iter: Chars<BufReader<File>>,
    cursor: (Option<char>, Option<char>),
    state: LexerState,
    chars: String,
    indent: usize,
    seek: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    fn shift(&mut self) -> Result<(), LexerError> {
        let (_, la) = self.cursor;
        let next = match self.char_iter.next() {
            Some(Ok('\n')) => {
                self.line += 1;
                self.column = 0;
                Some('\n')
            },
            Some(Ok(c)) => {
                self.column += 1;
                Some(c)
            },
            Some(Err(e)) =>
                return Err(e.into()),
            None => None,
        };

        self.seek += 1;
        self.cursor = (la, next);
        Ok(())
    }

    fn cursor_position(&self) -> Position {
        Position::point(
            self.seek, self.line, self.column
        )
    }

    fn point_token(&self, lex: Lexeme) -> Token {
        Token {
            lexeme: lex,
            position: self.cursor_position(),
        }
    }

    fn skip_whitespace(&mut self) -> Result<Option<Token>, LexerError> {
        let is_line_start = self.column == 0;
        let start_pos = (self.seek, self.line, self.column);

        let mut indent = 0;
        loop {
            let c = self.cursor;
            match c.1 {
                Some(w) if is_whitespace(w) => {
                    indent += 1
                },
                _ => break
            }
            self.shift()?
        }

        if is_line_start {
            let position = Position {
                from: start_pos,
                to: (self.seek, self.line, self.column),
            };

            use std::cmp::Ordering::*;
            let res = match indent.cmp(&self.indent) {
                Greater => Some(Token {
                    position,
                    lexeme: Lexeme::Indent,
                }),
                Less => Some(Token {
                    position,
                    lexeme: Lexeme::Unindent,
                }),
                Equal => None,

            };

            Ok(res)
        } else {
            Ok(None)
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        if let Some(tok) = self.skip_whitespace()? {
            return Ok(tok)
        }

        let (c, la) = self.cursor;
        let c = match c {
            Some(c) => c,
            None => return Ok(self.point_token(Lexeme::Eof)),
        };
        let state = self.state;

        use self::LexerState::*;
        use self::Lexeme::*;
        let tok = match c {
            '-' if la == Some('-') => self.comment(),
            '[' => Ok(self.point_token(Lbracket)),
            'T' if la == Some('[') => self.t_bracket(),
            ']' => Ok(self.point_token(Rbracket)),
            '{' => Ok(self.point_token(Lbrace)),
            '}' => Ok(self.point_token(Rbrace)),
            '(' => Ok(self.point_token(Lparren)),
            ')' => Ok(self.point_token(Rparren)),
            '"' => self.string(),
            '\'' => self.char(),
            '0' if la == Some('x') => self.hex_integer(),
            s if is_special(s) => self.operator(),
            l if is_letter(l) => self.word(),
            d if is_digit(d) => self.integer_or_word(),
            _ => unimplemented!(),
        };

        self.shift()?;

        tok
    }

    fn read_word(&mut self) -> Result<String, LexerError> {
        unimplemented!()
    }

    fn word(&mut self) -> Result<Token, LexerError> {
        let start_pos = (self.seek, self.line, self.column);

        let word = self.read_word()?;
        let is_mword = match self.cursor.1 {
            Some('[') => {
                self.shift()?;
                true
            },
            _ => false
        };
        let lexeme = match &*word {
            "_" => Lexeme::Underscore,
            _ if is_mword => Lexeme::Mword(word),
            _ => Lexeme::Word(word),
        };

        let position = Position {
            from: start_pos,
            to: (self.seek, self.line, self.column),
        };

        Ok(Token {
            position,
            lexeme,
        })
    }

    fn integer_or_word(&mut self) -> Result<Token, LexerError> {
        let start_pos = (self.seek, self.line, self.column);

        let mut thing = String::new();
        let lexeme;
        loop {
            thing.push(self.cursor.0.unwrap());

            match self.cursor.1 {
                Some(d) if is_digit(d) => (),
                Some(l) if is_letter(l) => {
                    let rem = self.read_word()?;
                    thing.push_str(&rem);

                    let lexeme = match self.cursor.1 {
                        Some('[') => {
                            self.shift()?;
                            Lexeme::Mword(thing)
                        },
                        _ => Lexeme::Word(thing)
                    };

                    let position = Position {
                        from: start_pos,
                        to: (self.seek, self.line, self.column),
                    };

                    return Ok(Token {
                        position,
                        lexeme,
                    })
                },
                Some('.') => {
                    thing.push('.');
                    self.shift()?;

                    if !self.cursor.0.map(is_digit).unwrap_or(false) {
                        let pos = Position {
                            from: start_pos,
                            to: (self.seek, self.line, self.column),
                        };
                        return Err(LexerError::InvalidInteger(pos))
                    }

                    while self.cursor.1.map(is_digit).unwrap_or(false) {
                        thing.push(self.cursor.0.unwrap());
                        self.shift()?;
                    }

                    lexeme = Lexeme::Float(thing);
                    break
                },
                _ => {
                    lexeme = Lexeme::Integer(thing);
                    break
                },
            }

            self.shift()?;
        }

        let position = Position {
            from: start_pos,
            to: (self.seek, self.line, self.column),
        };

        Ok(Token {
            position,
            lexeme
        })
    }

    fn comment(&mut self) -> Result<Token, LexerError> {
        let start_pos = (self.seek, self.line, self.column);
        self.shift()?;
        self.shift()?;

        let lex = match self.cursor.0 {
            Some('.') => {
                self.shift()?;
                Lexeme::DocComment
            },
            Some('^') => {
                self.shift()?;
                Lexeme::TopDocComment
            },
            _ => Lexeme::Comment,
        };

        let mut line = String::new();
        loop {
            match self.cursor.0 {
                Some('\n') | None => break,
                Some(c) => line.push(c),
            }
            self.shift()?;
        }

        let position = Position {
            from: start_pos,
            to: (self.seek, self.line, self.column),
        };

        Ok(Token {
            position,
            lexeme: lex(line),
        })
    }

    fn string(&mut self) -> Result<Token, LexerError> {
        let start_pos = (self.seek, self.line, self.column);
        self.shift()?;

        let mut string = String::new();
        loop {
            match self.cursor {
                (Some('\\'), Some('"')) => {
                    string.push('"');
                    self.shift()?;
                },
                (Some('\\'), Some('\\')) => {
                    string.push('\\');
                    self.shift()?;
                },
                (Some('"'), _) => break,
                (Some(c), _) => string.push(c),
                (None, _) => {
                    let pos = self.cursor_position();
                    return Err(LexerError::UnexpectedEof(pos))
                },
            }

            self.shift()?;
        }

        let position = Position {
            from: start_pos,
            to: (self.seek, self.line, self.column),
        };

        Ok(Token {
            position,
            lexeme: Lexeme::String(string)
        })
    }

    fn char(&mut self) -> Result<Token, LexerError> {
        unimplemented!()
    }

    fn hex_integer(&mut self) -> Result<Token, LexerError> {
        let start_pos = (self.seek, self.line, self.column);
        self.shift()?;
        self.shift()?;

        let mut number = "0x".to_string();
        self.shift()?;
        loop {
            let eon = self.cursor.1
                .map(|w| is_whitespace(w) || w == '\n')
                .unwrap_or(true);

            match self.cursor.0 {
                Some(d) if is_hex_digit(d) =>{
                    number.push(d)
                },
                _ => unimplemented!()
            }

            if eon { break }
            self.shift()?;
        }
        
        let position = Position {
            from: start_pos,
            to: (self.seek, self.line, self.column),
        };

        Ok(Token {
            position,
            lexeme: Lexeme::Integer(number),
        })
    }

    fn t_bracket(&mut self) -> Result<Token, LexerError> {
        let position = Position {
            from: (self.seek, self.line, self.column),
            to: (self.seek + 1, self.line, self.column + 1)
        };
        self.shift()?;

        Ok(Token {
            position,
            lexeme: Lexeme::Tbracket,
        })
    }

    fn operator(&mut self) -> Result<Token, LexerError> {
        let start_pos = (self.seek, self.line, self.column);
        
        let mut operator = String::new();
        loop {
            let eop = !self.cursor.1.map(is_special).unwrap_or(false);
            operator.push(self.cursor.0.unwrap());

            if eop { break }
            self.shift()?;
        }

        let position = Position {
            from: start_pos,
            to: (self.seek, self.line, self.column)
        };

        let lexeme = match &*operator {
            "," => Lexeme::Comma,
            "." => Lexeme::Dot,
            "->" => Lexeme::Larrow,
            ":" => Lexeme::Colon,
            "|" => Lexeme::Bar,
            "=" => Lexeme::Equals,
            _ => Lexeme::Operator(operator)
        };

        Ok(Token {
            position,
            lexeme,
        })
    }
}
