use crate::lex::Token;
use crate::lex::SpecialCharacter;
use crate::parse::Node;
use crate::parse::Operand;
use crate::parse::Container;
use crate::parse::Expression;
use crate::parse::util::make_expr;
use once_cell::sync::Lazy;

pub struct Lambda {
    pub name: String,
    pub text: String,
    pub lambda_text: String,
    pub lex: Vec<Token>,
    pub parse: Expression,
}

macro_rules! lazy {
    ($l: expr) => {
        Lazy::new(|| {$l})
    }
}

pub static Identity: Lazy<Lambda> = lazy!(Lambda {
    name: "Identity".to_string(),
    text: "x:x".to_string(),
    lambda_text: "\\lambda x . x".to_string(),
    lex: vec![
        Token::Name("x".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("x".to_string()),
    ],
    parse: make_expr(
        vec![
            Node::Container(Container::Set),
            Node::Name("x".to_string()),
            Node::Operand(Operand::Bind),
            Node::Name("x".to_string())
        ],
        vec![
            (0, 2),
            (2, 1),
            (2, 3),
        ]
    )
});

pub static Binding: Lazy<Lambda> = lazy!(Lambda {
    name: "Binding".to_string(),
    text: "x:M".to_string(),
    lambda_text: "\\lambda x.M[x]".to_string(),
    lex: vec![
        Token::Name("x".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("M".to_string()),
    ],
    parse: make_expr(
        vec![
            Node::Container(Container::Set),
            Node::Name("x".to_string()),
            Node::Operand(Operand::Bind),
            Node::Name("M".to_string())
        ],
        vec![
            (0, 2),
            (2, 1),
            (2, 3),
        ]
    )
});

pub static AlphaConversion: Lazy<Lambda> = lazy!(Lambda {
    name: "Alpha conversion".to_string(),
    text: "{x=y}:x:M".to_string(),
    lambda_text: "\\lambda x.M[x])\\rightarrow(\\lambda y.M[y])".to_string(),
    lex: vec![
        Token::Special(SpecialCharacter::StartSet),
        Token::Name("x".to_string()),
        Token::Special(SpecialCharacter::Alias),
        Token::Name("y".to_string()),
        Token::Special(SpecialCharacter::EndSet),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("x".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("M".to_string()),
    ],
    parse: make_expr(
        vec![
            Node::Container(Container::Set),
            Node::Container(Container::Set),
            Node::Name("x".to_string()),
            Node::Operand(Operand::Alias),
            Node::Name("y".to_string()),
            Node::Operand(Operand::Bind),
            Node::Name("x".to_string()),
            Node::Operand(Operand::Bind),
            Node::Name("M".to_string()),
        ],
        vec![
            (0, 5),
            (5, 1),
            (5, 7),
            (1, 3),
            (3, 2),
            (3, 4),
            (7, 6),
            (7, 8),
        ]
    )
});

pub static K: Lazy<Lambda> = lazy!(Lambda {
    name: "K".to_string(),
    text: "x:y:x".to_string(),
    lambda_text: "\\lambda y . \\lambda x . x".to_string(),
    lex: vec![
        Token::Name("x".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("y".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("x".to_string()),
    ],
    parse: make_expr(
        vec![
            Node::Container(Container::Set),
            Node::Name("x".to_string()),
            Node::Operand(Operand::Bind),
            Node::Name("y".to_string()),
            Node::Operand(Operand::Bind),
            Node::Name("x".to_string())
        ],
        vec![
            (0, 2),
            (2, 1),
            (2, 4),
            (4, 3),
            (4, 5),
        ]
    )
});