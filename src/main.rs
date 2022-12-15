mod lexer;
use crate::lexer::*;

fn main() {
    let input = "main ()\n print(true)\n\n";
    let result = lex(input);
    println!("{:?}", result);
}
