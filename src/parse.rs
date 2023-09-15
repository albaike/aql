use petgraph::Graph;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use std::collections::HashSet;
use crate::lex::Token;
use crate::lex::SpecialCharacter;

/// # Container
/// 
/// These types will be implemented in the interpreter.
/// All types are immutable by default.
/// 
/// ## Formal Grammar
/// 
/// [`<container>`](#container) `::= <set> | <list>`
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Node {
    /// ## Formal Grammar
    /// [`<name>`](#variant.Name) `::= ( <letter> | <number> | '_' )+`
    Name(String),
    Operand(Operand),
    Container(Container),
}

#[derive(Debug)]
pub struct Expression {
    pub tree: Graph<Node, ()>
}

trait ExpressionTree {
    fn parent(self: &Self, index: NodeIndex) -> Option<NodeIndex>;
    fn add_node(self: &mut Self, node: Node, parent: NodeIndex) -> NodeIndex;
    fn add_fork(self: &mut Self, node: Node, parent: NodeIndex) -> NodeIndex;
}

impl ExpressionTree for Expression {
    fn parent(self: &Self, index: NodeIndex) -> Option<NodeIndex> {
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

    fn add_node(self: &mut Self, node: Node, parent: NodeIndex) -> NodeIndex {
        let index = self.tree.add_node(node);
        self.tree.add_edge(parent, index, ());
        return index;
    }

    fn add_fork(self: &mut Self, node: Node, index: NodeIndex) -> NodeIndex {
        // find leaf
        // println!("Finding edge connected to {:?}", index);
        let edge = self.tree.first_edge(index, Direction::Outgoing)
                                  .expect("Root node not connected");

        // println!("Finding node at the end of {:?}", edge);
        let leaf_index = self.tree.edge_endpoints(edge)
                                            .expect("Root edge not found").1;

        // println!("Removing edge {:?}", edge);
        self.tree.remove_edge(edge)
            .expect("Should remove");

        let parent_index = self.add_node(node, index);
        self.tree.add_edge(parent_index, leaf_index, ());

        return parent_index;
    }

}

pub trait ExpressionTrait {
    fn new () -> Self;
    fn parse(self: &mut Self, token: Token, root: NodeIndex) -> NodeIndex;
}

impl PartialEq for Expression {
    fn eq(self: &Self, other: &Expression) -> bool {
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

impl ExpressionTrait for Expression {
    fn new () -> Expression {
        let mut tree = Graph::<Node, ()>::new();
        tree.add_node(Node::Container(Container::Set));
        return Expression {
            tree
        }
    }

    fn parse(self: &mut Self, token: Token, mut root: NodeIndex) -> NodeIndex {
    let mut special_root: Option<NodeIndex> = None;

    macro_rules! branch {
            ($node: expr) => {{
            special_root = None;
                self.add_node($node, root)
        }}
    }

    macro_rules! leaf {
            ($node: expr) => {{
                self.add_node($node, root);
            root
        }}
    }

    macro_rules! close_branch {
        () => {{
                self.parent(root)
                    .expect("Open branch should have a parent")
        }}
    }

    macro_rules! fork_leaf {
            ($node: expr) => {{
                self.add_fork($node, root)
        }}
    }

    // Close operations
        let node = self.tree.node_weight(root).expect("S");
        match node {
            Node::Operand(_) => {
            special_root = Some(close_branch!());
        }
        _ => ()
    }

    root = match token {
            Token::Name(name) => {leaf!(Node::Name(name))},
            Token::Special(SpecialCharacter::Alias) => {fork_leaf!(Node::Operand(Operand::Alias))}
            Token::Special(SpecialCharacter::Bind) => {fork_leaf!(Node::Operand(Operand::Bind))}
            Token::Special(SpecialCharacter::Apply) => {fork_leaf!(Node::Operand(Operand::Apply))}
            Token::Special(SpecialCharacter::Match) => {fork_leaf!(Node::Operand(Operand::Match))}
            Token::Special(SpecialCharacter::StartList) => {branch!(Node::Container(Container::List))}
            Token::Special(SpecialCharacter::StartSet) => {branch!(Node::Container(Container::Set))}
        Token::Special(SpecialCharacter::EndList) => {close_branch!()}
        Token::Special(SpecialCharacter::EndSet) => {close_branch!()}
    };

    return match special_root {
        Some(r) => r,
        _ => root
    }
}
}

pub mod util {
    use super::*;
    pub fn make_expr(nodes: Vec<Node>, edges: Vec<(usize, usize)>) -> Expression {
        let mut tree= Graph::<Node, ()>::new();
        for node in nodes {
            tree.add_node(node);
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
    use crate::lex::lex;
    use futures_util::pin_mut;
    use futures_util::stream::StreamExt;

    async fn text_expr(text: String) -> Expression {
        let stream = tokio_stream::iter(text.chars());
        let tokens = lex(stream);
        pin_mut!(tokens);

        let mut expression = Expression::new();
        let mut subroot = NodeIndex::new(0);

        while let Some(token) = tokens.next().await {
            subroot = expression.parse(token, subroot);
        }
        return expression;
    }

    macro_rules! assert_tree_eq {
        ($txt: expr, $nodes: expr, $edges: expr) => {
            assert_expr_eq!($txt, make_expr($nodes, $edges))
        }
    }

    macro_rules! assert_expr_eq {
        ($txt: expr, $expr: expr) => {
            assert_eq!(
                text_expr($txt.to_string()).await,
                $expr
            )
        }
    }

    #[tokio::test]
    async fn it_parses_empty() {
        let mut tree: Graph<Node, ()> = Graph::<Node, ()>::new();
        tree.add_node(Node::Container(Container::Set));

        assert_tree_eq!(
            "",
            vec![Node::Container(Container::Set)],
            vec![]
        )
    }

    #[tokio::test]
    async fn it_parses_name() {
        assert_tree_eq!(
            "a",
            vec![
                Node::Container(Container::Set),
                Node::Name("a".to_string())
            ],
            vec![
                (0, 1)
            ]
        )
    }

    #[tokio::test]
    async fn it_parses_set() {
        assert_tree_eq!(
            "{a b}",
            vec![
                Node::Container(Container::Set),
                Node::Container(Container::Set),
                Node::Name("a".to_string()),
                Node::Name("b".to_string())
            ],
            vec![
                (0, 1),
                (1, 2),
                (1, 3),
            ]
        )
    }

    #[tokio::test]
    async fn it_parses_operation() {
        assert_tree_eq!(
            "a:b",
            vec![
                Node::Container(Container::Set),
                Node::Name("a".to_string()),
                Node::Operand(Operand::Bind),
                Node::Name("b".to_string())
            ],
            vec![
                (0, 2),
                (2, 1),
                (2, 3),
            ]
        )
    }

    #[tokio::test]
    async fn it_parses_set_set_binding() {
        assert_tree_eq!(
            "{a}:{b}",
            vec![
                Node::Container(Container::Set),
                Node::Container(Container::Set),
                Node::Name("a".to_string()),
                Node::Operand(Operand::Bind),
                Node::Container(Container::Set),
                Node::Name("b".to_string())
            ],
            vec![
                (1, 2),
                (0, 3),
                (3, 1),
                (3, 4),
                (4, 5),
            ]
        )
    }
}