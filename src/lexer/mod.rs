use nom::{
    branch::alt,
    bytes::complete::{tag, escaped},
    character::complete::{alpha1, alphanumeric1, newline, digit1, multispace0, line_ending, space0},
    combinator::{recognize, map_res, map},
    multi::{many0_count, many0},
    sequence::{pair, delimited},
    IResult, error::ParseError,
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

#[derive(Debug, PartialEq)]
pub enum Lit {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Keyword(Keyword),
    Lit(Lit),
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    Indent,
    Dedent,

    At,
    Comma,
}

pub fn lex(input : &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut depth = 0;
    let mut rest = input;
    
    while let Ok((new_rest, result)) = lex_line(rest, depth) {
        if let Some((token,new_depth)) = result {
            tokens.extend(token);
            depth = new_depth;
            rest = new_rest;
        }
    }
    tokens
}

fn lex_line(input: &str, depth : usize) -> IResult<&str, Option<(Vec<Token>,usize)>> {
    let (input, indentation) =  indent(input, depth)?;
    let (input, tokens) = many0(alt((
        ws(lit),
        ws(identifier_or_keyword),
        ws(symbol)
        )))(input)?;
    let (input, _) = line_ending(input)?;
    if let Some((indent_token,indent)) = indentation {
        let mut result = vec![indent_token];
        result.extend(tokens);
        Ok((input, Some((result,indent))))
    } else {
        Ok((input, Some((tokens, depth))))
    }
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
  where
  F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
  delimited(
    space0,
    inner,
    space0
  )
}

fn lit(input : &str) -> IResult<&str, Token> {
    alt((
        map(float, |f| Token::Lit(Lit::Float(f))),
        map(integer, |i| Token::Lit(Lit::Int(i))),
        //map(string, |s| Token::Lit(Lit::String(s))),
        map(boolean, |b| Token::Lit(Lit::Bool(b)))
    ))(input)
}

fn integer(input : &str) -> IResult<&str, i64> {
    map_res(digit1, |s: &str| s.parse())(input)
}

fn boolean(input : &str) -> IResult<&str, bool> {
    alt((
        map(tag("true"), |_| true),
        map(tag("false"), |_| false)
    ))(input)
}

fn float(input : &str) -> IResult<&str, f64> {
    map_res(recognize(pair(digit1, pair(tag("."), digit1))), |s: &str| s.parse())(input)
}

fn indent(input: &str, depth: usize) -> IResult<&str, Option<(Token,usize)>> {
    let (input, indent) = many0_count(tag(" "))(input)?;
    if indent == depth {
        Ok((input, None))
    } else if indent > depth {
        Ok((input, Some((Token::Indent, indent))))
    } else {
        Ok((input, Some((Token::Dedent, indent))))
    }
}

fn symbol(input: &str) -> IResult<&str, Token> {
    let sym = recognize(alt((
        tag("("),
        tag(")"),
        tag("["),
        tag("]"),
        tag("{"),
        tag("}"),
        tag("@"),
        tag(","),
    )))(input);

    match sym {
        Ok((i, o)) => {
            let sym = match o {
                "(" => Token::LParen,
                ")" => Token::RParen,
                "[" => Token::LBracket,
                "]" => Token::RBracket,
                "{" => Token::LBrace,
                "}" => Token::RBrace,
                "@" => Token::At,
                "," => Token::Comma,
                _ => panic!("Unknown symbol: {}", o),
            };
            Ok((i, sym))
        }
        Err(e) => Err(e),
    }
}

fn identifier_or_keyword(input: &str) -> IResult<&str, Token> {
    let result = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input);

    match result {
        Ok((i, o)) => {
            let kw_result = keyword(o);
            match kw_result {
                Ok((_, kw)) => Ok((i, kw)),
                Err(_) => Ok((i, Token::Ident(o.to_string()))),
            }
        }
        Err(e) => Err(e),
    }
}

fn keyword(input: &str) -> IResult<&str, Token> {
    let kw = recognize(alt((tag("if"), tag("else"), tag("for"))))(input);

    match kw {
        Ok((i, o)) => {
            let kw = match o {
                "if" => Token::Keyword(Keyword::If),
                "else" => Token::Keyword(Keyword::Else),
                "for" => Token::Keyword(Keyword::For),
                _ => panic!("Unknown keyword: {}", o),
            };
            Ok((i, kw))
        }
        Err(e) => Err(e),
    }
}
