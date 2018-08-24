// Simple S-expression reader.

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

pub type Result = ::std::result::Result<Option<Datum>, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    line: usize
}

impl fmt::Display for ParseError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: Parse error: {}", self.line, &self.msg)
    }
}

#[derive(Clone,Debug)]
pub struct Cons(Rc<(Datum, Datum)>);

impl Cons
{
    pub fn car(&self) -> &Datum {
        &(self.0).0
    }

    pub fn cdr(&self) -> &Datum {
        &(self.0).1
    }
}

#[derive(Clone,Debug)]
pub struct Symbol(Rc<String>);

impl Symbol
{
    pub fn eq(a: &Symbol, b: &Symbol) -> bool {
        Rc::ptr_eq(&a.0, &b.0)
    }
}

#[derive(Clone,Debug)]
pub struct Vector(Rc<Vec<Datum>>);

#[derive(Clone,Copy,Debug)]
pub enum Number
{
    Flo(f64),
    Fix(i64)
}

impl PartialEq for Number
{
    // TODO: This is wrong, given that 1.0 != 1 due to representations...
    fn eq(&self, other: &Number) -> bool {
        match (self, other) {
            (Number::Fix(x), Number::Fix(y)) => x == y,
            (Number::Flo(x), Number::Flo(y)) => x == y,
            (Number::Flo(x), Number::Fix(y)) => {
                // if x is integer in range of i64 then x.as_int() == y else false
                false
            }
            (Number::Fix(x), Number::Flo(y)) => {
                // if y is integer in range of i64 then y.as_int() == x else false
                false
            }
        }
    }
}

#[derive(Clone,Debug)]
pub enum Datum
{
    Cons(Cons),
    Number(Number),
    Bool(bool),
    Str(String),
    Sym(Symbol),
    Chr(char),
    Vector(Vector),
    Nil
}

impl Datum
{
    pub fn is_cons(&self) -> Option<&Cons> {
        match self {
            Datum::Cons(ref c) => Some(c),
            _ => None
        }
    }

    pub fn is_number(&self) -> Option<&Number> {
        match self {
            Datum::Number(ref s) => Some(s),
            _ => None
        }
    }

    pub fn is_fix(&self) -> Option<i64> {
        match self {
            Datum::Number(Number::Fix(x)) => Some(*x),
            _ => None
        }
    }

    pub fn is_flo(&self) -> Option<f64> {
        match self {
            Datum::Number(Number::Flo(x)) => Some(*x),
            _ => None
        }
    }

    pub fn is_bool(&self) -> Option<bool> {
        match self {
            Datum::Bool(x) => Some(*x),
            _ => None
        }
    }
    
    pub fn is_symbol(&self) -> Option<&Symbol> {
        match self {
            Datum::Sym(ref s) => Some(s),
            _ => None
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Datum::Nil => true,
            _ => false
        }
    }

    pub fn eqv(a: &Datum, b:&Datum) -> bool {
        match (a, b) {
            (Datum::Nil, Datum::Nil) => true,
            (Datum::Number(ref x), Datum::Number(ref y)) => x == y,
            (Datum::Bool(x), Datum::Bool(y)) => x == y,
            (Datum::Chr(x), Datum::Chr(y)) => x == y,
            (Datum::Sym(ref x), Datum::Sym(ref y)) => Symbol::eq(x, y),
            _ => false
        }
    }
}

pub struct Symtab(HashMap<String, Symbol>);

impl Symtab
{
    pub fn new() -> Symtab
    {
        Symtab(HashMap::new())
    }

    pub fn intern(&mut self, name:&str) -> Symbol {
        if let Some(s) = self.0.get(name) {
            return s.clone();
        }
        self.do_intern(name.to_string())
    }

    pub fn intern_string(&mut self, name:String) -> Symbol
    {
        if let Some(s) = self.0.get(&name) {
            return s.clone();
        }
        self.do_intern(name)
    }

    fn do_intern(&mut self, name:String) -> Symbol {
        let sym = Symbol(Rc::new(name.clone()));
        self.0.insert(name, sym.clone());
        sym
    }
}

pub type Input = Iterator<Item = std::io::Result<char>>;

pub struct Parser<'a> {
    c0: char,
    input: &'a mut Input,
    syms: &'a mut Symtab,
    line: usize,
    allow_improper: bool,
    quote: Datum,
    dot: Datum
}
    
const OOB : char = '\0';

type Res = ::std::result::Result<Datum, ParseError>;

impl<'a> Parser<'a>
{
    pub fn new(input: &'a mut Input, symtab: &'a mut Symtab, allow_improper: bool) -> Parser<'a>
    {
        let quote_sym = symtab.intern("quote");
        let dot_sym = symtab.intern(".");
        let mut parser = Parser {
            c0: OOB,
            input: input,
            syms: symtab,
            line: 1,
            allow_improper,
            quote: Datum::Sym(quote_sym),
            dot: Datum::Sym(dot_sym)
        };
        parser.next();
        parser
    }

    pub fn parse(&mut self) -> Result
    {
        self.eat_whitespace_and_comment();
        if self.peek() == OOB {
            Ok(None)
        } else {
            match self.parse_datum_nodot() {
                Ok(x) => Ok(Some(x)),
                Err(e)  => Err(e)
            }
        }
    }
    
    fn fail(&self, s: &str) -> Res {
        Err(ParseError{ msg: s.to_string(), line: self.line })
    }

    fn fails(&self, s: String) -> Res {
        Err(ParseError{ msg: s, line: self.line })
    }

    // Precondition: we have just called eat_whitespace_and_comment().
    fn parse_datum(&mut self) -> Res
    {
        return match self.peek() {
            OOB  => self.fail("Unexpected EOF"),
            '('  => self.parse_list(),
            '#'  => self.parse_sharp(),
            '"'  => self.parse_string(),
            '\'' => self.parse_quote(),
            c if is_symbol_or_number_initial(c)
                 => self.parse_symbol_or_number(),
            _    => self.fails(format!("Unknown character {}", self.peek()))
        }
    }

    fn parse_datum_nodot(&mut self) -> Res
    {
        let datum = self.parse_datum()?;
        if Datum::eqv(&datum, &self.dot) {
            self.fail("Illegal dot symbol")
        } else {
            Ok(datum)
        }
    }

    fn parse_list(&mut self) -> Res
    {
        let mut data = vec![];
        let mut last = Datum::Nil;

        self.must_eat('(');
        loop {
            self.eat_whitespace_and_comment();
            if self.peek() == ')' {
                break;
            }
            let datum = self.parse_datum()?;
            if Datum::eqv(&datum, &self.dot) {
                if !self.allow_improper {
                    return self.fail("Illegal dotted pair")
                }
                self.eat_whitespace_and_comment();
                last = self.parse_datum_nodot()?;
                self.eat_whitespace_and_comment();
                break;
            }
            data.push(datum);
        }
        self.must_eat(')');

        let mut result = last;
        for d in data {
            result = cons(d, result);
        }
        Ok(result)
    }

    fn parse_vector(&mut self) -> Res
    {
        let mut data = vec![];

        self.must_eat('(');
        loop {
            self.eat_whitespace_and_comment();
            if self.peek() == ')' {
                break;
            }
            data.push(self.parse_datum_nodot()?);
        }
        self.must_eat(')');

        Ok(Datum::Vector(Vector(Rc::new(data))))
    }

    fn parse_quote(&mut self) -> Res
    {
        self.must_eat('\'');
        self.eat_whitespace_and_comment();
        let d = self.parse_datum_nodot()?;
        Ok(cons(self.quote.clone(), cons(d, Datum::Nil)))
    }

    fn parse_sharp(&mut self) -> Res
    {
        self.must_eat('#');
        match self.peek() {
            't' =>  { self.next(); Ok(Datum::Bool(true)) },
            'f' =>  { self.next(); Ok(Datum::Bool(false)) },
            '(' =>  { self.parse_vector() }
            _   =>  { self.fail("Bad sharp sequence") }
        }
    }

    fn parse_string(&mut self) -> Res
    {
        self.must_eat('"');
        let mut s = String::new();
        loop {
            match self.get() {
                '\\' => {
                    s.push(match self.get() {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        OOB => { return self.fail("EOF in string") },
                        c   => c
                    })
                }
                '"'  => { break; }
                OOB  => { return self.fail("EOF in string") }
                c    => { s.push(c); }
            }
        }
        Ok(Datum::Str(s))
    }

    fn parse_symbol_or_number(&mut self) -> Res
    {
        let mut name = Vec::new();
        name.push(self.get());
        while is_symbol_or_number_subsequent(self.peek()) {
            name.push(self.get());
        }
        let (is_number, is_integer) = is_number_syntax(&name);
        let name_str: String = name.into_iter().collect();
        if is_number {
            if is_integer {
                Ok(Datum::Number(Number::Fix(i64::from_str(&name_str).unwrap()))) // TODO: Range error?
            } else {
                Ok(Datum::Number(Number::Flo(f64::from_str(&name_str).unwrap()))) // TODO: Range error?
            }
        } else {
            Ok(Datum::Sym(self.syms.intern_string(name_str)))
        }
    }

    fn eat_whitespace_and_comment(&mut self)
    {
        loop {
            match self.peek() {
                ' ' | '\t'
                    => { self.next(); }
                '\r'
                    => { self.line += 1; self.next();
                         if self.peek() == '\n' {
                             self.next();
                         }
                       }
                '\n'
                    => { self.line += 1; self.next(); }
                ';'
                    => { self.next();
                         loop {
                             match self.peek() {
                                 OOB | '\r' | '\n' => { break; }
                                 _                 => { self.next() }
                             }
                         }
                    }
                _
                    => { return; }
            }
        }
    }

    fn peek(&self) -> char
    {
        self.c0
    }

    fn get(&mut self) -> char
    {
        let c = self.c0;
        self.next();
        c
    }

    fn next(&mut self)
    {
        self.c0 = self.getchar();
    }

    fn must_eat(&mut self, c:char)
    {
        if self.peek() != c {
            panic!("Required '{}' but did not see it", c);
        }
        self.next();
    }

    fn getchar(&mut self) -> char
    {
        match self.input.next() {
            None         => { OOB },
            Some(Err(e)) => { panic!("i/o error {}", e) },
            Some(Ok(c))  => { c }
        }
    }
}

fn cons(a: Datum, b: Datum) -> Datum
{
    Datum::Cons(Cons(Rc::new((a,b))))
}

fn is_digit(c:char) -> bool
{
    match c {
        '0'...'9' => true,
        _         => false
    }
}

fn is_symbol_initial(c:char) -> bool
{
    match c {
        'a'...'z' | 'A'...'Z' | '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '_' | '-' | '+' | '=' |
        ':' | '<' | ',' | '>' | '?' | '/'
            => true,
        _   => false
    }
}

fn is_symbol_or_number_initial(c:char) -> bool
{
    is_symbol_initial(c) || is_digit(c) || c == '.'
}

fn is_symbol_or_number_subsequent(c:char) -> bool
{
    is_symbol_initial(c) || is_digit(c) || c == '.' || c == '#'
}

fn is_number_syntax(s:&Vec<char>) -> (bool, bool)
{
    let mut i = 0;

    macro_rules! digits {
        () => {{
            let p = i;
            while i < s.len() && is_digit(s[i]) {
                i += 1;
            }
            i > p
        }}
    }
    macro_rules! opt_sign {
        () => {
            if i < s.len() && (s[i] == '+' || s[i] == '-') {
                i += 1;
            }
        }
    }

    opt_sign!();

    let hasint = digits!();
    let mut hasfrac = false;

    if i < s.len() && s[i] == '.' {
        i += 1;
        hasfrac = digits!();
        if !hasfrac {
            return (false, false);
        }
    }

    if i < s.len() && (s[i] == 'e' || s[i] == 'E') {
        i += 1;
        opt_sign!();
        hasfrac = digits!();
        if !hasfrac {
            return (false, false);
        }
    }

    let is_number = i == s.len() && (hasint || hasfrac);
    if is_number {
        (true, !hasfrac)
    } else {
        (false, false)
    }
}

extern crate unicode_reader;

#[test]
fn test_generic()
{
    use unicode_reader::CodePoints;

    let mut syms = Symtab::new();
    let hi = Datum::Sym(syms.intern("hi"));
    let ho = Datum::Sym(syms.intern("ho"));
    let mut input = CodePoints::from("  hi ho(37)".as_bytes());
    let mut parser = Parser::new(&mut input, &mut syms, true);
    assert_eq!(Datum::eqv(&parser.parse().unwrap().unwrap(), &hi), true);
    assert_eq!(Datum::eqv(&parser.parse().unwrap().unwrap(), &ho), true);
    let d = &parser.parse().unwrap().unwrap();
    if let Some(c) = d.is_cons() {
        if let Some(n) = c.car().is_fix() {
            assert_eq!(n, 37);
        }
        assert_eq!(c.cdr().is_nil(), true);
    } else {
        panic!("Bad result {:?}", d)
    }
}
