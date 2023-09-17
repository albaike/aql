use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use std::vec::Vec;
use itertools::Itertools;

/// # Special Characters
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(EnumIter)]
#[derive(Clone)]
pub enum SpecialCharacter {
    /// [`<alias>`](#variant.Alias) `::= '='`
    Alias,
    /// [`<bind>`](#variant.Bind) `::= ':'`
    Bind,
    /// [`<apply>`](#variant.Apply) `::= '.'`
    Apply,
    /// [`<match>`](#variant.Match) `::= '->'`
    Match,
    StartList,
    EndList,
    StartSet,
    EndSet,
}

impl ToString for SpecialCharacter {
    fn to_string(&self) -> String {
        return match self {
            SpecialCharacter::Alias => String::from("="),
            SpecialCharacter::Bind => String::from(":"),
            SpecialCharacter::Apply => String::from("."),
            SpecialCharacter::Match => String::from(">"),
            SpecialCharacter::StartList => String::from("("),
            SpecialCharacter::EndList => String::from(")"),
            SpecialCharacter::StartSet => String::from("{"),
            SpecialCharacter::EndSet => String::from("}"),
        }
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum Token {
    Name(String),
    Special(SpecialCharacter)
}

impl ToString for Token {
    fn to_string(&self) -> String {
        return match self {
            Token::Name(s) => s.clone(),
            Token::Special(c) => c.to_string(),
        }
    }
}

pub fn tokens_to_string(tokens: Vec<Token>) -> String {
    return tokens.iter().map(|token| token.to_string()).join(" ");
}

pub trait ToTokens {
    fn to_tokens(&self) -> Vec<Token>;
}

impl ToTokens for String {
    fn to_tokens(&self) -> Vec<Token> {
        let mut vec: Vec<Token> = Vec::new();
        let mut name: String = String::from("");

        macro_rules! add {
            ($token: expr) => {
                vec.push($token)
            }
        }

        macro_rules! add_name {
            () => {
                if name.len() > 0 {
                    add!(Token::Name(name));
                    name = String::from("");
                }
            }
        }

        for c in self.chars() {
            match c {
                '='  => {add_name!();add!(Token::Special(SpecialCharacter::Alias));},
                ':'  => {add_name!();add!(Token::Special(SpecialCharacter::Bind));},
                '.'  => {add_name!();add!(Token::Special(SpecialCharacter::Apply));},
                '>'  => {add_name!();add!(Token::Special(SpecialCharacter::Match));}
                '('  => {add_name!();add!(Token::Special(SpecialCharacter::StartList));},
                ')'  => {add_name!();add!(Token::Special(SpecialCharacter::EndList));},
                '{'  => {add_name!();add!(Token::Special(SpecialCharacter::StartSet));},
                '}'  => {add_name!();add!(Token::Special(SpecialCharacter::EndSet));},
                ' '  => {add_name!()}
                '\t' => {add_name!()}
                '\n' => {add_name!()}
                '\r' => {add_name!()}
                _    => {name.push(c);}
            }
        }
        add_name!();

        return vec;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spec::lambda::all_lambdas;

    macro_rules! assert_reflexes {
        ($txt: expr, $tokens: expr) => {
            assert_eq!(
                $txt.to_string().to_tokens(),
                $tokens
            );
            assert_eq!(
                $txt.to_string(),
                tokens_to_string($tokens)
            );
        }
    }

    #[test]
    fn it_reflexes_empty() {
        assert_reflexes!(
            "",
            vec![]
        );
    }

    #[test]
    fn it_reflexes_special() {
        for c in <SpecialCharacter as IntoEnumIterator>::iter() {
            assert_reflexes!(
                c.clone(),
                vec![Token::Special(c.clone())]
            );
        }
    }

    #[test]
    fn it_reflexes_name() {
        assert_reflexes!(
            "a",
            vec![Token::Name("a".to_string())]
        );

        assert_reflexes!(
            "aaaaaaaa",
            vec![Token::Name("aaaaaaaa".to_string())]
        );
    }

    #[test]
    fn it_reflexes_names() {
        assert_reflexes!(
            "a b c a",
            vec![
                Token::Name("a".to_string()),
                Token::Name("b".to_string()),
                Token::Name("c".to_string()),
                Token::Name("a".to_string()),
            ]
        );
    }

    #[test]
    fn it_reflexes_bindings() {
        assert_reflexes!(
            "x : M",
            vec![
                Token::Name("x".to_string()),
                Token::Special(SpecialCharacter::Bind),
                Token::Name("M".to_string()),
            ]
        );

        assert_reflexes!(
            "{ x = y } : x : M",
            vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Name("x".to_string()),
                Token::Special(SpecialCharacter::Alias),
                Token::Name("y".to_string()),
                Token::Special(SpecialCharacter::EndSet),
                Token::Special(SpecialCharacter::Bind),
                Token::Name("x".to_string()),
                Token::Special(SpecialCharacter::Bind),
                Token::Name("M".to_string()),
            ]
        );
    }

    #[test]
    fn it_reflexes_sets() {
        assert_reflexes!(
            "{ a b c a }",
            vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Name("a".to_string()),
                Token::Name("b".to_string()),
                Token::Name("c".to_string()),
                Token::Name("a".to_string()),
                Token::Special(SpecialCharacter::EndSet),
            ]
        );

        assert_reflexes!(
            "{ { a b c a } }",
            vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Special(SpecialCharacter::StartSet),
                Token::Name("a".to_string()),
                Token::Name("b".to_string()),
                Token::Name("c".to_string()),
                Token::Name("a".to_string()),
                Token::Special(SpecialCharacter::EndSet),
                Token::Special(SpecialCharacter::EndSet),
            ]
        );
    }

    // #[test]
    // fn it_reflexes_lambda() {
    //     for lambda in all_lambdas() {
    //         assert_reflexes!(
    //             lambda.text,
    //             lambda.clone().tokens
    //         );
    //     }
    // }
}