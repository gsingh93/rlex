#![feature(macro_rules)]

extern crate collections;

use collections::HashMap;
use std::string::String;

macro_rules! dict(
    ($($key:expr => $val:expr),*) => ({
        let mut h = collections::HashMap::new();
        $(
            h.insert($key, $val);
        )*
        h
    })
)

#[deriving(Show)]
enum Token {
    Plus, Minus
}

struct Lexer {
    toks: HashMap<String, Token>
}

impl Lexer {
    fn new(toks: &HashMap<&str, Token>) -> Lexer {
        let mut toks_: HashMap<String, Token> = HashMap::new();
        for (s, t) in toks.iter() {
            toks_.insert(s.to_string(), *t);
        }

        Lexer {toks: toks_}
    }

    fn analyze(&self, text: &str) -> Vec<Token> {
        let mut text = text.clone();
        let mut toks = Vec::new();
        while text.len() != 0 {
            for (s, t) in self.toks.iter() {
                if text.starts_with(s.as_slice()) {
                    toks.push(*t);
                    text = text.slice_from(s.len());
                }
            }
        }

        toks
    }
}

fn main() {
    let toks = dict!["+" => Plus,
                     "-" => Minus];
    let lexer = Lexer::new(&toks);
    let result_toks = lexer.analyze("+-");
    for tok in result_toks.iter() {
        println!("{}", tok);
    }
}
