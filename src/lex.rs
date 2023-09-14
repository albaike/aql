use tokio_stream::Iter;
use futures_util::stream::StreamExt;
use async_stream::stream;
use futures_core::stream::Stream;
use std::str::Chars;

/// Special characters include:
/// ---------------
/// | `=` | Alias |
/// | `:` | Bind  |
/// | `.` | Apply |
/// | `>` | Match |
/// ---------------
#[derive(Debug)]
#[derive(PartialEq)]
pub enum SpecialCharacter {
    Alias,
    Bind,
    Apply,
    Match,
    StartList,
    EndList,
    StartSet,
    EndSet,
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Token {
    Name(String),
    Special(SpecialCharacter)
}

pub fn lex(mut stream: Iter<Chars<'_>>) -> impl Stream<Item = Token> + '_ {
    stream! {
        let mut name: String = String::from("");
        macro_rules! yield_name {
            () => {
                if name.len() > 0 {
                    yield Token::Name(name);
                    name = String::from("");
                }
            }
        }

        while let Some(c) = stream.next().await {
            match c {
                '='  => {yield_name!();yield Token::Special(SpecialCharacter::Alias);},
                ':'  => {yield_name!();yield Token::Special(SpecialCharacter::Bind);},
                '.'  => {yield_name!();yield Token::Special(SpecialCharacter::Apply);},
                '>'  => {yield_name!();yield Token::Special(SpecialCharacter::Match);}
                '('  => {yield_name!();yield Token::Special(SpecialCharacter::StartList);},
                ')'  => {yield_name!();yield Token::Special(SpecialCharacter::EndList);},
                '{'  => {yield_name!();yield Token::Special(SpecialCharacter::StartSet);},
                '}'  => {yield_name!();yield Token::Special(SpecialCharacter::EndSet);},
                ' '  => {yield_name!()}
                '\t' => {yield_name!()}
                '\n' => {yield_name!()}
                '\r' => {yield_name!()}
                _    => {name.push(c);}
            }
        }
        yield_name!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::pin_mut;

    async fn text_tokens(text: String) -> Vec<Token> {
        let stream = tokio_stream::iter(text.chars());
        let tokens = lex(stream);
        pin_mut!(tokens);

        let mut v: Vec<Token> = Vec::new();
        while let Some(token) = tokens.next().await {
            v.push(token);
        }
        return v;
    }

    macro_rules! assert_lexes {
        ($txt: expr, $tokens: expr) => {
            assert_eq!(
                text_tokens($txt.to_string()).await,
                $tokens
            )
        }
    }

    #[tokio::test]
    async fn it_lexes() {
        assert_lexes!(
            "a",
            vec![Token::Name("a".to_string())]
        );

        assert_lexes!(
            "aaaaaaaa",
            vec![Token::Name("aaaaaaaa".to_string())]
        );

        assert_lexes!(
            "a b c a",
            vec![
                Token::Name("a".to_string()),
                Token::Name("b".to_string()),
                Token::Name("c".to_string()),
                Token::Name("a".to_string()),
            ]
        );

        assert_lexes!(
            "x:M",
            vec![
                Token::Name("x".to_string()),
                Token::Special(SpecialCharacter::Bind),
                Token::Name("M".to_string()),
            ]
        );

        assert_lexes!(
            "{x=y}:x:M",
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
}