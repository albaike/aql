use petgraph::Graph;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use crate::lex::Token;
use crate::lex::SpecialCharacter;
// mod lex;

/// Container types.
/// 
/// These types will be implemented in the interpreter.
/// All types are fixed by default.
#[derive(Debug)]
pub enum Container {
    /// A set of unique items
    Set,
    /// An ordered list of items
    List
}

#[derive(Debug)]
pub enum Operation {
    /// Assign the right operand (value) to the left operand (name) within that namespace
    Alias,
    /// Assign the right operand (function) to the left operand (variables)
    Bind,
    /// Substitute the values of the left operand (arguments) to the right operand (functio)
    Apply,
    Match,
}

#[derive(Debug)]
pub enum Node {
    Name(String),
    Container(Container),
    Operation(Operation)
}

pub fn parse(token: Token, tree: &mut Graph<Node, ()>, mut root: NodeIndex) -> NodeIndex {
    fn add_branch(node: Node, tree: &mut Graph<Node, ()>, root: NodeIndex) -> NodeIndex {
        let index = tree.add_node(node);
        tree.add_edge(root, index, ());
        return index;
    }

    fn parent(tree: &mut Graph<Node, ()>, index: NodeIndex) -> NodeIndex {
        let edge = tree.first_edge(index, Direction::Incoming)
                                  .expect("Should have parent");

        // Will crash on malformed token sequence
        return tree.edge_endpoints(edge)
                   .expect("Expect parent node").0;
    }

    fn add_fork_leaf(node: Node, tree: &mut Graph<Node, ()>, index: NodeIndex) -> NodeIndex {
        // find leaf
        // println!("Finding edge connected to {:?}", index);
        let edge = tree.first_edge(index, Direction::Outgoing)
                                  .expect("Root node not connected");

        // println!("Finding node at the end of {:?}", edge);
        let leaf_index = tree.edge_endpoints(edge)
                                            .expect("Root edge not found").1;

        // println!("Removing edge {:?}", edge);
        tree.remove_edge(edge)
            .expect("Should remove");

        let parent_index = add_branch(node, tree, index);
        tree.add_edge(parent_index, leaf_index, ());

        return parent_index;
    }

    let mut special_root: Option<NodeIndex> = None;

    macro_rules! branch {
        ($node: expr) => {{
            special_root = None;
            add_branch($node, tree, root)
        }}
    }

    macro_rules! leaf {
        ($node: expr) => {{
            add_branch($node, tree, root);
            root
        }}
    }

    macro_rules! close_branch {
        () => {{
            parent(tree, root)
        }}
    }

    macro_rules! fork_leaf {
        ($node: expr) => {{
            add_fork_leaf($node, tree, root)
        }}
    }

    // Close operations
    let node = tree.node_weight(root).expect("S");
    match node {
        Node::Operation(_) => {
            println!("Closed operation");
            special_root = Some(close_branch!());
        }
        _ => ()
    }

    root = match token {
        Token::Name(name) => {leaf!(Node::Name(name))},
        Token::Special(SpecialCharacter::Alias) => {fork_leaf!(Node::Operation(Operation::Alias))}
        Token::Special(SpecialCharacter::Bind) => {fork_leaf!(Node::Operation(Operation::Bind))}
        Token::Special(SpecialCharacter::Apply) => {fork_leaf!(Node::Operation(Operation::Apply))}
        Token::Special(SpecialCharacter::Match) => {fork_leaf!(Node::Operation(Operation::Match))}
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::lex;
    use petgraph::dot::{Dot, Config};
    use futures_util::pin_mut;
    use futures_util::stream::StreamExt;

    async fn text_tree(text: String) -> Graph<Node, ()> {
        let stream = tokio_stream::iter(text.chars());
        let tokens = lex(stream);
        pin_mut!(tokens);

        let mut tree = Graph::<Node, ()>::new();
        let root = tree.add_node(Node::Container(Container::Set));
        let mut subroot = root;

        while let Some(token) = tokens.next().await {
            subroot = parse(token, &mut tree, subroot);
        }
        return tree;
    }

    fn make_tree(nodes: Vec<Node>, edges: Vec<(usize, usize)>) -> Graph<Node, ()> {
        let mut tree: Graph<Node, ()> = Graph::<Node, ()>::new();
        for node in nodes {
            tree.add_node(node);
        }

        for edge in edges {
            tree.add_edge(NodeIndex::new(edge.0), NodeIndex::new(edge.1), ());
        }
        return tree;
    }

    macro_rules! assert_tree_eq {
        ($txt: expr, $nodes: expr, $edges: expr) => {
            assert_eq!(
                format!("{:?}", Dot::with_config(
                    &text_tree($txt.to_string()).await,
                    &[Config::GraphContentOnly]
                )),
                format!("{:?}", Dot::with_config(
                    &make_tree($nodes, $edges),
                    &[Config::GraphContentOnly]
                )),
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
                Node::Operation(Operation::Bind),
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
                Node::Operation(Operation::Bind),
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