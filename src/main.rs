mod lexer;
use crate::lexer::*;

fn main() {
    let input = "main ()\n print(3)\n\n";
    let result = lex(input);
    println!("{:?}", result);
}
