use super::*;

#[cfg(test)]
mod tests {
    #[test]
    fn test_identifier() {
        let input = "main";
        let tokens = identifier(input);
        println!("{:?}", ast);
    }
}
