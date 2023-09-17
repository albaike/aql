use petgraph::Graph;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::HashSet;
use itertools::Itertools;
use crate::lex::{Token, ToTokens, tokens_to_string};
use crate::lex::SpecialCharacter;

/// # Container
/// 
/// These types will be implemented in the interpreter.
/// All types are immutable by default.
/// 
/// ## Formal Grammar
/// 
/// [`<container>`](#container) `::= <set> | <list>`
#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub enum Container {
    /// A group of unique expressions
    /// 
    /// ## Formal Grammar
    /// 
    /// [`<set>`](#variant.Set) `::=` [`'{'`](../lex/enum.SpecialCharacter.html#variant.StartSet) `(` [`<expression>`](./enum.Expression.html) `)*` [`'}'`](../lex/enum.SpecialCharacter.html#variant.EndSet)`
    Set,
    /// An ordered group of expressions
    /// 
    /// ## Formal Grammar
    /// 
    /// [`<list>`](#variant.List) `::=` [`'('`](../lex/enum.SpecialCharacter.html#variant.StartList) `(` [`<expression>`](./enum.Expression.html) `)*` [`')'`](../lex/enum.SpecialCharacter.html#variant.EndList)`
    List
}

/// # Operand
/// ## Formal Grammar
/// `<operation> ::=` [`<expression>`](./enum.Expression.html) `<operand>` [`<expression>`](./enum.Expression.html)
/// 
/// [`<operand>`](#) ::=` [`<alias>`](#variant.Alias) `|` [`<bind>`](#variant.Bind) `|` [`<apply>`](#variant.Apply) `|` [`<match>`](#variant.Match)
#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub enum Operand {
    /// Assign the right operand (value) to the left operand (name) within that namespace
    /// 
    /// [Formal Grammar](../lex/enum.SpecialCharacter.html#variant.Alias)
    Alias,
    /// Assign the right operand (function) to the left operand (variables)
    /// 
    /// [Formal Grammar](../lex/enum.SpecialCharacter.html#variant.Bind)
    Bind,
    /// Substitute the values of the left operand (arguments) to the right operand (function)
    /// 
    /// [Formal Grammar](../lex/enum.SpecialCharacter.html#variant.Apply)
    Apply,
    /// [Formal Grammar](../lex/enum.SpecialCharacter.html#variant.Match)
    Match,
}

/// # Expression
/// ## Formal Grammar
/// [`<expression>`](#) `::= <clause> (<seperator> <clause>)*`
/// 
/// `<separator> ::= (' ' | '\t' | '\n' | '\r' | ',')+`
/// 
/// `<clause> ::=` [`<name>`](#variant.Name) `|` [`<operation>`](#variant.Operation) `|` [`<container>`](#variant.Container)
///
#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub enum Node {
    /// ## Formal Grammar
    /// [`<name>`](#variant.Name) `::= ( <letter> | <number> | '_' )+`
    Name(String),
    Operand(Operand),
    Container(Container),
    OpenContainer(Container),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub tree: Graph<Node, ()>
}

impl Expression {
    pub fn new () -> Self {
        let mut tree = Graph::<Node, ()>::new();
        tree.add_node(Node::OpenContainer(Container::Set));
        return Expression {
            tree
        }
    }

    pub fn empty () -> Self {
        return Expression {
            tree: Graph::<Node, ()>::new()
        }
    }

    fn parse_token(self: &mut Self, token: &Token, root: NodeIndex) -> NodeIndex {
        macro_rules! add_name {
            ($name: expr) => {{
                self.add_child(
                    Node::Name($name),
                    self.search_accept(root)
                )
            }}
        }

        macro_rules! add_operand {
            ($operand: expr) => {{
                self.add_fork(Node::Operand($operand), root)
            }}
        }

        macro_rules! open_container {
            ($node: expr) => {{
                self.add_child(Node::OpenContainer($node), root)
            }}
        }

        macro_rules! close_container {
            ($container: expr) => {{
                let parent = self.search_up(Node::OpenContainer($container), root)
                                 .expect("Open container should have a matching parent");
                self.replace_node(Node::Container($container), parent)
            }}
        }

        return  match token {
            Token::Name(name) => {add_name!(name.clone())},
            Token::Special(SpecialCharacter::Alias) => {add_operand!(Operand::Alias)}
            Token::Special(SpecialCharacter::Bind) => {add_operand!(Operand::Bind)}
            Token::Special(SpecialCharacter::Apply) => {add_operand!(Operand::Apply)}
            Token::Special(SpecialCharacter::Match) => {add_operand!(Operand::Match)}
            Token::Special(SpecialCharacter::StartList) => {open_container!(Container::List)}
            Token::Special(SpecialCharacter::StartSet) => {open_container!(Container::Set)}
            Token::Special(SpecialCharacter::EndList) => {close_container!(Container::List)}
            Token::Special(SpecialCharacter::EndSet) => {close_container!(Container::Set)}
        };
    }

    pub fn children(&self, index: NodeIndex) -> impl Iterator<Item = NodeIndex> + '_ {
        return self.tree.edges_directed(index, Direction::Outgoing)
                        .map(|edge| edge.target())
                        .sorted();
    }

    pub fn add_expression(
        &mut self,
        expr: &Expression,
        mut index: NodeIndex,
        expr_index: NodeIndex
    ) -> NodeIndex {
        let node = expr.tree.node_weight(expr_index).expect("Should have a node at index");
        index = self.add_child(node.clone(), index);
        for child in expr.children(expr_index) {
            index = self.add_expression(expr, index, child);
        }
        return index;
    }
}

trait ExpressionTree {
    fn parent(&self, index: NodeIndex) -> Option<NodeIndex>;
    fn search_up(&self, node: Node, index: NodeIndex) -> Option<NodeIndex>;
    fn search_accept(&self, index: NodeIndex) -> NodeIndex;
    fn add_child(&mut self, node: Node, parent: NodeIndex) -> NodeIndex;
    fn replace_node(&mut self, node: Node, index: NodeIndex) -> NodeIndex;
    fn add_fork(&mut self, node: Node, parent: NodeIndex) -> NodeIndex;
}

impl ExpressionTree for Expression {
    fn parent(&self, index: NodeIndex) -> Option<NodeIndex> {
        let edge = self.tree.first_edge(index, Direction::Incoming);

        return match edge {
            Some(e) => {
                let points = self.tree.edge_endpoints(e);
                return match points {
                    Some(p) => Some(p.0),
                    _ => None
                }
            },
            _ => None
        }
    }

    fn search_accept(&self, index: NodeIndex) -> NodeIndex {
        return match self.tree.node_weight(index)
                       .expect("Must search on existing nodes") 
        {
            Node::Operand(_) => {
                match self.children(index).count() {
                    2 => self.search_accept(self.parent(index).expect("Must have parent")),
                    _ => index
                }
            },
            Node::OpenContainer(_) => index,
            _ => self.search_accept(self.parent(index).expect("Must have parent"))
        };
    }

    fn search_up(&self, node: Node, index: NodeIndex) -> Option<NodeIndex> {
        return match self.tree.node_weight(index) {
            Some(w) => {
                if *w == node {
                    Some(index)
                } else {
                    match self.parent(index) {
                        Some(p) => self.search_up(node, p),
                        _ => None
                    }
                }
            },
            _ => None
        }
    }

    fn add_child(self: &mut Self, node: Node, parent: NodeIndex) -> NodeIndex {
        let index = self.tree.add_node(node);
        self.tree.add_edge(parent, index, ());
        return index;
    }

    fn replace_node(self: &mut Self, node: Node, index: NodeIndex) -> NodeIndex {
        self.tree[index] = node;
        return index;
    }

    fn add_fork(self: &mut Self, node: Node, index: NodeIndex) -> NodeIndex {
        let (edge, leaf_index) = match
            self.tree.node_weight(index)
                     .expect("Node at index should exist") 
        {
            Node::OpenContainer(_) =>  {
                let edge = self.tree.first_edge(index, Direction::Outgoing)
                                          .expect("Parent node not connected");
                (
                    edge, 
                    self.tree.edge_endpoints(edge)
                             .expect("Parent edge not found").1
                )
            },
            _ => {
                (
                    self.tree.first_edge(index, Direction::Incoming)
                             .expect("Parent node not connected"),
                    index
                )
            }
        };

        let parent_index = self.add_child(
            node,
            self.tree.edge_endpoints(edge)
                             .expect("parent edge should be connected")
                             .0
        );
        self.tree.remove_edge(edge)
            .expect("Should remove");

        self.tree.add_edge(parent_index, leaf_index, ());

        return parent_index;
        // return leaf_index;
    }

}

impl PartialEq for Expression {
    // FIXME repacing edges with (Node, Node)
    // Placing edges in tree order
    fn eq(&self, other: &Expression) -> bool {
        return 
            self.tree.edge_count() == other.tree.edge_count()
            && self.tree.node_count() == other.tree.node_count()
            && {
                let mut set = HashSet::with_capacity(self.tree.edge_count());
                for edge in self.tree.raw_edges() {
                    set.insert((edge.source(), edge.target()));
                }
                set
            } == {
                let mut set = HashSet::with_capacity(other.tree.edge_count());
                for edge in self.tree.raw_edges() {
                    set.insert((edge.source(), edge.target()));
                }
                set

            }
            && self.tree.node_weights().collect::<Vec<&Node>>() == other.tree.node_weights().collect::<Vec<&Node>>();
    }

}

// impl<const N: usize> From<[Token; N]> for Expression {
//     fn from (tokens: [Token; N]) -> Self {
//         let mut expr = Self::new();
//         let mut index = NodeIndex::new(0);
// 
//         for token in tokens {
//             index = expr.parse_token(&token, index);
//         }
//         return expr;
//     }
// }

impl From<&Vec<Token>> for Expression {
    fn from (tokens: &Vec<Token>) -> Self {
        let mut expr = Self::new();
        let mut index = NodeIndex::new(0);

        for token in tokens.iter() {
            index = expr.parse_token(&token, index);
        }
        expr.parse_token(&Token::Special(SpecialCharacter::EndSet), index);
        return expr;
    }
}

impl ToTokens for Expression {
    fn to_tokens(&self) -> Vec<Token> {
        fn to_tokens_part(expr: &Expression, index: NodeIndex) -> Vec<Token> {
            let node = expr.tree.node_weight(index).expect("Should have root");

            let v = match node {
                Node::Name(n) => vec![Token::Name(n.clone())],
                Node::Operand(o) => {
                    let mut tokens: Vec<Token> = Vec::new();
                    let children: Vec<NodeIndex> = expr.children(index).collect();
                    tokens.extend(
                        to_tokens_part(
                            expr,
                            *children.get(0).expect("m")
                        ).iter().map(|token| token.clone())
                    );
                    tokens.push(Token::Special(match o {
                        Operand::Alias => SpecialCharacter::Alias,
                        Operand::Bind => SpecialCharacter::Bind,
                        Operand::Apply => SpecialCharacter::Apply,
                        Operand::Match => SpecialCharacter::Match,
                    }));
                    tokens.extend(
                        to_tokens_part(
                            expr,
                            *children.get(1).expect("m")
                        ).iter().map(|token| token.clone())
                    );
                    tokens
                },
                Node::Container(c) | Node::OpenContainer(c) => {
                    let mut tokens: Vec<Token> = Vec::new();
                    if index != NodeIndex::new(0) {
                        tokens.push(Token::Special(match c {
                            Container::Set => SpecialCharacter::StartSet,
                            Container::List => SpecialCharacter::StartList,
                        }));
                    }

                    for child in expr.children(index) {
                        tokens.extend(to_tokens_part(expr, child).iter().map(|token| token.clone()));
                    }

                    if index != NodeIndex::new(0) {
                        tokens.push(Token::Special(match c {
                            Container::Set => SpecialCharacter::EndSet,
                            Container::List => SpecialCharacter::EndList,
                        }));
                    }
                    tokens
                },
            };
            return v;
        }

        return to_tokens_part(self, NodeIndex::new(0));
    }
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        return tokens_to_string(self.to_tokens());
    }
}

pub trait ToExpression {
    fn to_expression(&self) -> Expression;
}

pub mod util {
    use super::*;
    pub fn make_expr(nodes: &Vec<Node>, edges: &Vec<(usize, usize)>) -> Expression {
        let mut tree= Graph::<Node, ()>::new();
        for node in nodes {
            tree.add_node(node.clone());
        }

        for edge in edges {
            tree.add_edge(NodeIndex::new(edge.0), NodeIndex::new(edge.1), ());
        }
        return Expression {
            tree
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::util::*;
    use crate::spec::lambda::all_lambdas;

    macro_rules! test_expr {
        ($tokens: expr, $nodes: expr, $edges: expr) => {
            let expr = make_expr($nodes, $edges);
            assert_eq!(Expression::from(&$tokens), expr);
            assert_eq!(expr.to_tokens(), $tokens);
            assert_eq!(Expression::from(&$tokens).to_tokens(), $tokens);
        }
    }

    macro_rules! some_name_token {
        () => {Token::Name(String::from("x"))}
    }

    macro_rules! all_operand_tokens {
        () => {[
            SpecialCharacter::Alias,
            SpecialCharacter::Bind,
            SpecialCharacter::Apply,
            SpecialCharacter::Match,
        ]}
    }

    macro_rules! all_container_tokens {
        () => {[
            (SpecialCharacter::StartList, SpecialCharacter::EndList),
            (SpecialCharacter::StartSet, SpecialCharacter::EndSet),
        ]}
    }

    #[test]
    fn it_parses_empty() {
        test_expr!(
            (vec![] as Vec<Token>),
            &vec![Node::Container(Container::Set)],
            &vec![]
        );
    }

    #[test]
    fn it_parses_name() {
        test_expr!(
            vec![Token::Name(String::from("a"))],
            &vec![
                Node::Container(Container::Set),
                Node::Name(String::from("a"))
            ],
            &vec![
                (0, 1)
            ]
        );
    }

    #[test]
    fn it_parses_set() {
        test_expr!(
            vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("a")),
                Token::Name(String::from("b")),
                Token::Special(SpecialCharacter::EndSet),
            ],
            &vec![
                Node::Container(Container::Set),
                Node::Container(Container::Set),
                Node::Name(String::from("a")),
                Node::Name(String::from("b"))
            ],
            &vec![
                (0, 1),
                (1, 2),
                (1, 3),
            ]
        );
    }

    #[test]
    fn it_parses_operation() {
        for operand_token in all_operand_tokens!() {
            let mut expr = Expression::new();
            let mut index = NodeIndex::new(0);

            index = expr.parse_token(&some_name_token!(), index);
            assert_eq!(index, NodeIndex::new(1));
            assert_eq!(expr.parent(index), Some(NodeIndex::new(0)));

            index = expr.parse_token(&Token::Special(operand_token), index);
            assert_eq!(index, NodeIndex::new(2));
            assert_eq!(expr.parent(index), Some(NodeIndex::new(0)));
            assert_eq!(expr.parent(NodeIndex::new(1)), Some(index));
        }
    }

    #[test]
    fn it_parses_nested_set() {
        let mut expr = Expression::from(&vec![
            Token::Special(SpecialCharacter::StartSet),
            Token::Special(SpecialCharacter::EndSet)
        ]);

        assert_eq!(
            expr.children(NodeIndex::new(0)).collect::<Vec<NodeIndex>>(),
            vec![NodeIndex::new(1)]
        );

        assert_eq!(
            expr.children(NodeIndex::new(1)).collect::<Vec<NodeIndex>>(),
            vec![]
        );

    }

    #[test]
    fn it_parses_set_set_binding() {
        test_expr!(
            vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("a")),
                Token::Special(SpecialCharacter::EndSet),
                Token::Special(SpecialCharacter::Bind),
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("b")),
                Token::Special(SpecialCharacter::EndSet),
            ],
            &vec![
                Node::Container(Container::Set),
                Node::Container(Container::Set),
                Node::Name(String::from("a")),
                Node::Operand(Operand::Bind),
                Node::Container(Container::Set),
                Node::Name(String::from("b"))
            ],
            &vec![
                (1, 2),
                (0, 3),
                (3, 1),
                (3, 4),
                (4, 5),
            ]
        );
    }

    #[test]
    fn it_parses_lambda() {
        for lambda in all_lambdas() {
            test_expr!(
                lambda.tokens,
                &lambda.parse_nodes,
                &lambda.parse_edges
            );
        }
    }

    // #[test]
    // fn it_parses_set2() {
    //     let mut expr = Expression::new();
    //     let mut index = NodeIndex::new(0);

    //     index = expr.parse_token(&Token::Special(SpecialCharacter::StartSet), index);
    //     assert_eq!(index, NodeIndex::new(1));
    //     assert_eq!(expr.parent(index), Some(NodeIndex::new(0)));
    //     assert_eq!(expr.search_up(Node::OpenContainer(Container::Set), index), Some(index));

    //     index = expr.parse_token(&Token::Special(SpecialCharacter::EndSet), index);
    //     assert_eq!(index, NodeIndex::new(1));
    //     assert_eq!(expr.parent(index), Some(NodeIndex::new(0)));
    //     assert_eq!(expr.search_up(Node::OpenContainer(Container::Set), index), Some(NodeIndex::new(0)));

    //     index = expr.parse_token(&Token::Special(SpecialCharacter::Alias), index);
    //     // println!("{:?}", expr.tree.raw_edges());
    //     assert_eq!(index, NodeIndex::new(2));
    //     assert_eq!(expr.parent(index), Some(NodeIndex::new(0)));
    //     assert_eq!(expr.parent(NodeIndex::new(1)), Some(NodeIndex::new(2)));

    //     index = expr.parse_token(&Token::Name(String::from("a")), index);
    //     assert_eq!(index, NodeIndex::new(3));
    //     assert_eq!(expr.parent(index), Some(NodeIndex::new(2)));
    //     assert_eq!(expr.parent(NodeIndex::new(1)), Some(NodeIndex::new(2)));
    //     assert_eq!(expr.parent(NodeIndex::new(2)), Some(NodeIndex::new(0)));

    //     index = expr.parse_token(&Token::Special(SpecialCharacter::Alias), index);
    //     assert_eq!(index, NodeIndex::new(4));
    //     assert_eq!(expr.parent(index), Some(NodeIndex::new(2)));
    //     assert_eq!(expr.parent(NodeIndex::new(3)), Some(index));
    //     assert_eq!(expr.parent(NodeIndex::new(1)), Some(NodeIndex::new(2)));
    //     assert_eq!(expr.parent(NodeIndex::new(2)), Some(NodeIndex::new(0)));

    //     index = expr.parse_token(&Token::Name("b".to_string()), index);
    //     assert_eq!(index, NodeIndex::new(5));
    //     assert_eq!(expr.parent(index), Some(NodeIndex::new(4)));
    // }

    #[test]
    fn it_integrates_string () {
        test_expr!(
            vec![
                Token::Name(String::from("a")),
                Token::Name(String::from("b")),
            ],
            &vec![
                Node::Container(Container::Set),
                Node::Name(String::from("a")),
                Node::Name(String::from("b"))
            ],
            &vec![
                (0, 1),
                (0, 2),
            ]
        );
    }
}