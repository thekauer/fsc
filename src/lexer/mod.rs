use nom::{
  IResult,
  branch::alt,
  multi::many0_count,
  combinator::recognize,
  sequence::pair,
  character::complete::{alpha1, alphanumeric1},
  bytes::complete::tag,
};

#[derive(Debug, PartialEq)]
pub enum Keyword {
    If,
    Else,
    For,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    And,
    Or,
    Not,
}

enum IndentStyle {
    Spaces,
    Tabs,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Keyword(Keyword),
    Number(i64),
    Op(Operator),
    LParen,
    RParen,
    At,
    Comma,
    Newline,
    Indent(IndentStyle, usize),
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
  recognize(
    pair(
      alt((alpha1, tag("_"))),
      many0_count(alt((alphanumeric1, tag("_"))))
    )
  )(input)
}
