// Simple S-expression reader.

extern crate unicode_reader;

use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

use unicode_reader::CodePoints;

pub type Pair = Box<(Datum, Datum)>;
pub type Symbol = Rc<SymValue>;

#[derive(Clone)]
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

type Input = Iterator<Item = std::io::Result<char>>;

// To compare symbols, use ptr_eq()
pub struct SymValue
{
    name: String
}

pub struct Symtab
{
    table:   HashMap<String, Symbol>,
    quote:   Symbol
}

const OOB : char = '\0';

pub fn make_symtab() -> Symtab
{
    let mut table = HashMap::new();
    let quote_sym = Rc::new(SymValue { name: "quote".to_string() });
    table.insert(quote_sym.name.clone(), Rc::clone(&quote_sym)).unwrap();
    Symtab {
        table:   table,
        quote:   quote_sym
    }
}

pub fn test(syms:&mut Symtab) -> Option<Datum>
{
    let mut input_ = CodePoints::from(std::io::stdin());
    let mut input = Parser {
        c0: OOB,
        c1: OOB,
        input: &mut input_,
        syms: syms
    };
    input.parse()
}

struct Parser<'a> {
    c0: char,
    c1: char,
    input: &'a mut Input,
    syms: &'a mut Symtab
}
    
impl<'a> Parser<'a>
{
    fn parse(&mut self) -> Option<Datum>
    {
        self.eat_whitespace_and_comment();
        if self.peek() == OOB { None } else { Some(self.parse_datum()) }
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

    fn eat_digits(&mut self, s:&mut String, mut musthave: bool)
    {
        loop {
            if !is_digit(self.peek()) {
                if musthave {
                    panic!("Expected at least one digit");
                }
                break;
            }
            musthave = false;
            s.push(self.get());
        }
    }
    
    fn parse_datum(&mut self) -> Datum
    {
        loop {
            self.eat_whitespace_and_comment();
            match self.peek() {
                OOB  => { panic!("Unexpected EOF") }
                '('  => { return self.parse_list(); }
                '#'  => { return self.parse_sharp(); }
                '"'  => { return self.parse_string(); }
                '\'' => { return self.parse_quote(); }
                c    => {
                    if is_number_initial(c) {
                        return self.parse_number();
                    }
                    if is_symbol_initial(c) {
                        return self.parse_symbol();
                    }
                    if c == '.' && is_digit(self.peek2()) {
                        return self.parse_number();
                    }
                    panic!("Unknown");
                }
            }
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
            if self.peek() == '.' {
                self.next();
                self.eat_whitespace_and_comment();
                last = self.parse_datum();
                self.eat_whitespace_and_comment();
                break;
            }
            data.push(self.parse_datum());
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
        let d = self.parse_datum();
        cons(Datum::Sym(Rc::clone(&self.syms.quote)), cons(d, Datum::Nil))
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

    // TODO: Really not right; we should parse a char sequence and then decide
    // whether it's a number or a symbol.  Consider "-1", currently a symbol.
    
    fn parse_number(&mut self) -> Datum
    {
        let mut n = String::new();
        let mut flonum = false;
        self.eat_digits(&mut n, false);
        if self.peek() == '.' {
            flonum = true;
            n.push(self.get());
            self.eat_digits(&mut n, true);
        }
        if self.peek() == 'e' || self.peek() == 'E' {
            flonum = true;
            n.push(self.get());
            if self.peek() == '+' || self.peek() == '-' {
                n.push(self.get());
            }
            self.eat_digits(&mut n, true);
        }
        if flonum {
            Datum::Flo(f64::from_str(&n).unwrap()) // Range error?
        } else {
            Datum::Fix(i64::from_str(&n).unwrap()) // Range error?
        }
    }

    // TODO: Really not right; we should parse a char sequence and then decide
    // whether it's a number or a symbol.
    
    fn parse_symbol(&mut self) -> Datum
    {
        let mut name = String::new();
        name.push(self.get());
        while is_symbol_subsequent(self.peek()) {
            name.push(self.get());
        }
        Datum::Sym(self.intern(name))
    }

    fn intern(&mut self, name:String) -> Symbol
    {
        if let Some(s) = self.syms.table.get(&name) {
            return Rc::clone(s);
        }

        let sym = Rc::new(SymValue { name: name.clone() });
        self.syms.table.insert(name, Rc::clone(&sym)).unwrap();
        sym
    }

    fn peek(&mut self) -> char
    {
        self.c0
    }

    fn peek2(&mut self) -> char
    {
        if self.c1 == OOB {
            self.c1 = self.getchar();
        }
        self.c1
    }

    fn get(&mut self) -> char
    {
        let c = self.c0;
        self.next();
        c
    }

    fn next(&mut self)
    {
        self.c0 = self.c1;
        self.c1 = OOB;
        if self.c0 == OOB {
            self.c0 = self.getchar();
        }
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
        '0'...'9'
            => true,
        _   => false
    }
}

fn is_number_initial(c:char) -> bool
{
    is_digit(c)
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

fn is_symbol_subsequent(c:char) -> bool
{
    is_symbol_initial(c) ||
        match c {
            '.' | '0'...'9'
                => true,
            _   => false
        }
}
