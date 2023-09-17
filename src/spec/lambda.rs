use crate::lex::Token;
use crate::lex::SpecialCharacter;
use crate::parse::Node;
use crate::parse::Operand;
use crate::parse::Container;
use std::sync::LazyLock;
// use crate::parse::Expression;
// use crate::parse::util::make_expr;

macro_rules! lazy {
    ($l: expr) => {
        LazyLock::new(|| {$l})
    }
}

pub struct Lambda {
    pub name: String,
    pub text: String,
    pub lambda_text: String,
    pub tokens: Vec<Token>,
    pub parse_nodes: Vec<Node>,
    pub parse_edges: Vec<(usize, usize)>,
}

static IDENTITY: LazyLock<Lambda> = lazy!(Lambda {
    name: String::from("Identity"),
    text: String::from("x:x"),
    lambda_text: String::from("\\lambda x . x"),
    tokens: vec![
        Token::Name(String::from("x")),
        Token::Special(SpecialCharacter::Bind),
        Token::Name(String::from("x")),
    ],
    parse_nodes: vec![
        Node::Container(Container::Set),
        Node::Name(String::from("x")),
        Node::Operand(Operand::Bind),
        Node::Name(String::from("x")),
    ],
    parse_edges: vec![
        (0, 2),
        (2, 1),
        (2, 3),
    ]
});

static BINDING: LazyLock<Lambda> = lazy!(Lambda {
    name: "Binding".to_string(),
    text: "x:M".to_string(),
    lambda_text: "\\lambda x.M[x]".to_string(),
    tokens: vec![
        Token::Name("x".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("M".to_string()),
    ],
    parse_nodes: vec![
        Node::Container(Container::Set),
        Node::Name("x".to_string()),
        Node::Operand(Operand::Bind),
        Node::Name("M".to_string())
    ],
    parse_edges: vec![
        (0, 2),
        (2, 1),
        (2, 3),
    ]
});

static ALPHA_CONVERSION: LazyLock<Lambda> = lazy!(Lambda {
    name: "Alpha conversion".to_string(),
    text: "{x=y}:x:M".to_string(),
    lambda_text: "\\lambda x.M[x])\\rightarrow(\\lambda y.M[y])".to_string(),
    tokens: vec![
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
    parse_nodes: vec![
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
    parse_edges: vec![
        (0, 5),
        (5, 1),
        (5, 7),
        (1, 3),
        (3, 2),
        (3, 4),
        (7, 6),
        (7, 8),
    ]
});

static K: LazyLock<Lambda> = lazy!(Lambda {
    name: "K".to_string(),
    text: "x:y:x".to_string(),
    lambda_text: "\\lambda y . \\lambda x . x".to_string(),
    tokens: vec![
        Token::Name("x".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("y".to_string()),
        Token::Special(SpecialCharacter::Bind),
        Token::Name("x".to_string()),
    ],
    parse_nodes: vec![
        Node::Container(Container::Set),
        Node::Name("x".to_string()),
        Node::Operand(Operand::Bind),
        Node::Name("y".to_string()),
        Node::Operand(Operand::Bind),
        Node::Name("x".to_string())
    ],
    parse_edges: vec![
        (0, 2),
        (2, 1),
        (2, 4),
        (4, 3),
        (4, 5),
    ]
});

pub fn all_lambdas() -> Vec<&'static Lambda> {
    return vec![
        LazyLock::force(&IDENTITY),
        LazyLock::force(&BINDING),
        LazyLock::force(&ALPHA_CONVERSION),
        LazyLock::force(&K),
    ];
}