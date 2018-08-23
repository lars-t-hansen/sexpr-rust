// Simple S-expression reader.

use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

// TODO: very likely, Pair wants to be Rc<> and Vector wants to have
// Rc<> wrapped around it, so that it's possible to create DAGs /
// share data in various ways when processing the s-expression.

pub type Pair = Box<(Datum, Datum)>;
pub type Symbol = Rc<SymValue>;

#[derive(Clone,Debug)]
pub enum Datum
{
    Cons(Pair),
    Flo(f64),
    Fix(i64),
    Bool(bool),
    Str(String),
    Sym(Symbol),
    Chr(char),
    Vector(Vec<Datum>),
    Nil
}

impl Datum {
    // TODO: One can argue about this, given that 1.0 != 1 due to representations...
    
    pub fn eqv(a: &Datum, b:&Datum) -> bool {
        match (a, b) {
            (Datum::Nil, Datum::Nil) => true,
            (Datum::Flo(x), Datum::Flo(y)) => x == y,
            (Datum::Fix(x), Datum::Fix(y)) => x == y,
            (Datum::Bool(x), Datum::Bool(y)) => x == y,
            (Datum::Chr(x), Datum::Chr(y)) => x == y,
            (Datum::Sym(ref x), Datum::Sym(ref y)) => Rc::ptr_eq(x, y),
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct SymValue
{
    name: String
}

pub struct Symtab
{
    table:     HashMap<String, Symbol>,
    pub quote: Datum,
    pub dot:   Datum
}

impl Symtab
{
    pub fn new() -> Symtab
    {
        let mut table = HashMap::new();

        let quote_sym = Rc::new(SymValue { name: "quote".to_string() });
        table.insert(quote_sym.name.clone(), Rc::clone(&quote_sym));

        let dot_sym = Rc::new(SymValue { name: ".".to_string() });
        table.insert(dot_sym.name.clone(), Rc::clone(&dot_sym));

        Symtab {
            table:   table,
            quote:   Datum::Sym(quote_sym),
            dot:     Datum::Sym(dot_sym)
        }
    }

    pub fn intern(&mut self, name:String) -> Symbol
    {
        if let Some(s) = self.table.get(&name) {
            return Rc::clone(s);
        }

        let sym = Rc::new(SymValue { name: name.clone() });
        self.table.insert(name, Rc::clone(&sym));
        sym
    }
}

// TODO: allow user to control whether dotted pairs is a thing, for
// some applications they are not desirable.  In this case, the dot
// syntax should be recognized but rejected.
//
// TODO: error reporting, including line number of error.

pub type Input = Iterator<Item = std::io::Result<char>>;

pub struct Parser<'a> {
    c0: char,
    input: &'a mut Input,
    syms: &'a mut Symtab
}
    
const OOB : char = '\0';

impl<'a> Parser<'a>
{
    pub fn new(input: &'a mut Input, symtab: &'a mut Symtab) -> Parser<'a>
    {
        let mut parser = Parser {
            c0: OOB,
            input: input,
            syms: symtab
        };
        parser.next();
        parser
    }

    pub fn parse(&mut self) -> Option<Datum>
    {
        self.eat_whitespace_and_comment();
        if self.peek() == OOB { None } else { Some(self.parse_datum()) }
    }
    
    // Precondition: we have just called eat_whitespace_and_comment().
    fn parse_datum(&mut self) -> Datum
    {
        return match self.peek() {
            OOB  => panic!("Unexpected EOF"),
            '('  => self.parse_list(),
            '#'  => self.parse_sharp(),
            '"'  => self.parse_string(),
            '\'' => self.parse_quote(),
            c if is_symbol_or_number_initial(c)
                 => self.parse_symbol_or_number(),
            _    => panic!("Unknown character {}", self.peek())
        }
    }

    fn parse_list(&mut self) -> Datum
    {
        let mut data = vec![];
        let mut last = Datum::Nil;

        self.must_eat('(');
        loop {
            self.eat_whitespace_and_comment();
            if self.peek() == ')' {
                break;
            }
            let datum = self.parse_datum();
            if Datum::eqv(&datum, &self.syms.dot) {
                self.eat_whitespace_and_comment();
                last = self.parse_datum();
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
        return result;
    }

    fn parse_vector(&mut self) -> Datum
    {
        let mut data = vec![];

        self.must_eat('(');
        loop {
            self.eat_whitespace_and_comment();
            if self.peek() == ')' {
                break;
            }
            data.push(self.parse_datum());
        }
        self.must_eat(')');

        return Datum::Vector(data);
    }

    fn parse_quote(&mut self) -> Datum
    {
        self.must_eat('\'');
        self.eat_whitespace_and_comment();
        let d = self.parse_datum();
        cons(self.syms.quote.clone(), cons(d, Datum::Nil))
    }

    fn parse_sharp(&mut self) -> Datum
    {
        self.must_eat('#');
        match self.peek() {
            't' =>  { self.next(); Datum::Bool(true) },
            'f' =>  { self.next(); Datum::Bool(false) },
            '(' =>  { self.parse_vector() }
            _   =>  { panic!("Bad sharp sequence"); }
        }
    }

    fn parse_string(&mut self) -> Datum
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
                        OOB => { panic!("EOF in string"); },
                        c   => c
                    })
                }
                '"'  => { break; }
                OOB  => { panic!("EOF in string") }
                c    => { s.push(c); }
            }
        }
        Datum::Str(s)
    }

    fn parse_symbol_or_number(&mut self) -> Datum
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
                Datum::Fix(i64::from_str(&name_str).unwrap()) // Range error?
            } else {
                Datum::Flo(f64::from_str(&name_str).unwrap()) // Range error?
            }
        } else {
            Datum::Sym(self.syms.intern(name_str))
        }
    }

    fn eat_whitespace_and_comment(&mut self)
    {
        loop {
            match self.peek() {
                ' ' | '\t' | '\r' | '\n'
                    => { self.next(); }
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
    Datum::Cons(Box::new((a,b)))
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
    let hi = Datum::Sym(syms.intern("hi".to_string()));
    let mut input = CodePoints::from("  hi".as_bytes());
    let mut parser = Parser::new(&mut input, &mut syms);
    assert_eq!(Datum::eqv(&parser.parse().unwrap(), &hi), true);
    println!("there");
}
