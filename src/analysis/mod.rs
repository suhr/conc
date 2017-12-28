use super::parser::{self};
use super::parser::lexer::Position;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
struct Function {
    scheme: Vec<Word>,
    input: TypeExpr,
    output: TypeExpr,
}

type Dictonary = HashMap<Symbol, Entry>;

#[derive(Debug, Clone, PartialEq, Default)]
struct Entry {
    ty: Option<Function>,
    eff: Option<parser::StackEff>,
    code: Option<FullExpr>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Symbol {
    path: Vec<String>,
    name: String,
    is_special: bool,
}

impl Symbol {
    fn new(path: Vec<String>, name: String) -> Self {
        Symbol {
            path, name,
            is_special: false,
        }
    }
    fn from_name(name: String) -> Self {
        Symbol {
            path: vec![],
            name,
            is_special: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Word {
    word: String,
    position: Position,
}

impl From<parser::Name> for Word {
    fn from(source: parser::Name) -> Self {
        Word {
            word: source.0,
            position: source.1
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct VariableId {
    name: String,
    nesting: usize,
}

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    id: VariableId,
    position: Position,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Soundness {
    Ok,
    Nonsense,
}

impl Default for Soundness {
    fn default() -> Self {
        Soundness::Ok
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
struct ExprCtx {
    soundness: Soundness,
    ty: Option<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Branch(Vec<Pattern>, FullExpr);

#[derive(Debug, Clone, PartialEq)]
enum FullExpr {
    Comp(Vec<FullExpr>, ExprCtx),
    Conc(Vec<FullExpr>, ExprCtx),
    Quote(Box<FullExpr>, ExprCtx),
    Lambda(Option<Pattern>, Box<FullExpr>, ExprCtx),
    Case(usize, Vec<Branch>),
    If(Box<FullExpr>, Box<FullExpr>, ExprCtx),
    Literal(Literal, ExprCtx),
    Variable(Variable, ExprCtx),
    Word(Word),
    Empty
}

#[derive(Debug, Clone, PartialEq)]
struct Lambda {
    pattern: Pattern,
    vars: Vec<VariableId>,
    expr: Box<FullExpr>,

}

#[derive(Debug, Clone, PartialEq)]
enum Pattern {
    Comp(Vec<Pattern>),
    Conc(Vec<Pattern>),
    Placeholder,
    Literal(Literal),
    Variable(Variable),
    Word(Word),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
enum TypeExpr {
    Comp(Vec<TypeExpr>),
    Conc(Vec<TypeExpr>),
    Word(Word),
    Variable(Variable),
    Multivar(Variable),
    Function(Box<Function>),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
enum TransError {
    
}

fn derive_pattern_vars() {
    unimplemented!();
}

fn collect_symbols(decls: &[parser::Decl], path: &mut Vec<String>) -> Result<Vec<Symbol>, TransError> {
    let mut syms = vec![];
    for decl in decls {
        match decl {
            //&parser::Decl::DataDef(_, _) => unimplemented!(),
            &parser::Decl::FuncType(ref name, _) => {
                syms.push(Symbol::new(path.clone(), name.0.clone()));
            },
            &parser::Decl::FuncDef(ref name, _, _, ref moar) => {
                syms.push(Symbol::new(path.clone(), name.0.clone()));
                
                if let &Some(ref moar) = moar {
                    path.push(name.0.clone());
                    syms.extend(collect_symbols(moar, path)?);
                    path.pop().unwrap();
                }
            },
        }
    }

    Ok(syms)
}

fn translate_decl(tree: Vec<parser::Decl>, mut path: Vec<Symbol>) -> Result<Dictonary, TransError> {
    let mut dict: Dictonary = HashMap::new();

    //let mut trans = Default::default();
    for decl in tree {
        match decl {
            //parser::Decl::DataDef(_, _) => unimplemented!(),
            parser::Decl::FuncType(name, tex) => {
                let tex = translate_func_ty(tex)?;
                dict.get_mut(&Symbol::from_name(name.0));
            },
            parser::Decl::FuncDef(name, eff, expr, moar) => {
                
                
                if let Some(moar) = moar {
                    path.push(Symbol::from_name(name.0));
                    //let decl = translate_decl(tree, path);
                    path.pop().unwrap();
                }
            },
        }
    }

    Ok(dict)
}

fn translate_func_ty(ty: parser::Expr<parser::TypeExpr>) -> Result<Function, TransError> {
    unimplemented!()
}

#[derive(Debug, Clone, PartialEq, Default)]
struct ExprTranslator {
    level: usize,
    defined_vars: Vec<VariableId>,
}

fn translate_expr(expr: parser::Expr<parser::FullExpr>, trans: &mut ExprTranslator) -> Result<FullExpr, TransError> {
    let fe = match expr {
        parser::Expr::Comp(comp) => {
            let mut res = vec![];
            for e in comp {
                res.push(translate_expr(e, trans)?);
            }
            FullExpr::Comp(res, Default::default())
        },
        parser::Expr::Conc(conc) => {
            let mut res = vec![];
            for e in conc {
                res.push(translate_expr(e, trans)?);
            }

            FullExpr::Comp(res, Default::default())
        },
        parser::Expr::Enclosed(expr) => 
            translate_expr(*expr, trans)?,
        parser::Expr::Mexpr(_, _) => unimplemented!(),
        parser::Expr::Tuple(_) => unimplemented!(),
        parser::Expr::Plain(fe) => {
            match fe {
                parser::FullExpr::Block(expr) =>
                    translate_expr(*expr, trans)?,
                parser::FullExpr::Case(_, _) => unimplemented!(),
                parser::FullExpr::If(_, _, _) => unimplemented!(),
                parser::FullExpr::Infix(e1, op, e2) => {
                    let e1 = translate_expr(*e1, trans)?;
                    let e2 = translate_expr(*e2, trans)?;
                    let op = FullExpr::Word(op.into());

                    FullExpr::Comp(vec![FullExpr::Conc(vec![e1, e2], Default::default()), op], Default::default())
                },
                parser::FullExpr::Lambda(pat, expr) => {
                    unimplemented!();
                    trans.level += 1;
                    translate_expr(*expr, trans)?;
                },
                parser::FullExpr::List(_) => unimplemented!(),
                parser::FullExpr::Quote(expr) => {
                    let expr = Box::new(translate_expr(*expr, trans)?);
                    FullExpr::Quote(expr, Default::default())
                },
                parser::FullExpr::Simple(_) => unimplemented!(),
            }
        },
        parser::Expr::Empty => unimplemented!(),
    };

    Ok(fe)
}