use tokio_stream::Iter;
use async_stream::stream;
use futures_core::stream::Stream;
use futures_util::pin_mut;
use futures_util::stream::StreamExt;
use std::str::Chars;
use petgraph::Graph;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::dot::{Dot, Config};

/// Special characters include:
/// ---------------
/// | `=` | Alias |
/// | `:` | Bind  |
/// | `.` | Apply |
/// | `>` | Match |
/// ---------------
#[derive(Debug)]
enum SpecialCharacter {
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
enum Token {
    Name(String),
    Special(SpecialCharacter)
}

// lexer

fn lex(mut stream: Iter<Chars<'_>>) -> impl Stream<Item = Token> + '_ {
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

// parser

/// Container types.
/// 
/// These types will be implemented in the interpreter.
/// All types are fixed by default.
#[derive(Debug)]
enum Container {
    /// A set of unique items
    Set,
    /// An ordered list of items
    List
}

#[derive(Debug)]
enum Operation {
    /// Assign the right operand (value) to the left operand (name) within that namespace
    Alias,
    /// Assign the right operand (function) to the left operand (variables)
    Bind,
    /// Substitute the values of the left operand (arguments) to the right operand (functio)
    Apply,
    Match,
}

#[derive(Debug)]
enum Node {
    Name(String),
    Container(Container),
    Operation(Operation)
}

fn parse(token: Token, tree: &mut Graph<Node, ()>, mut root: NodeIndex) -> NodeIndex {
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
        println!("Finding edge connected to {:?}", index);
        let edge = tree.first_edge(index, Direction::Outgoing)
                                  .expect("Root node not connected");

        println!("Finding node at the end of {:?}", edge);
        let leaf_index = tree.edge_endpoints(edge)
                                            .expect("Root edge not found").1;

        println!("Removing edge {:?}", edge);
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

/// # aql (algebraic query language)
/// 
/// ## Language
/// 
/// ### Name
/// 
/// `a` --- [name](#name) `a`
/// <br/><br/>
/// 
/// Note: All numbers are [names](#name), not [literals](#literal)
/// 
/// <br/>
///
/// ### Operator
/// 
/// #### Alias
/// 
/// `a=b` --- [alias](#alias) `b` as `a`
/// <br/><br/>
/// 
/// Assigns `b` to `a` within the [container](#container) it is written in.
/// 
/// If `a` is a new [name](#name) then it is added to the [container's](#container) *symbol table*.
/// 
/// If the [container](#container) is a [set](#set) then `a` must be unique within that [set](#set), similarly to declaring `a` by itself in the [set](#set).
/// 
/// `{a=b}`     Valid
/// 
/// `{a=b a=c}` Invalid
/// 
/// `{a a=b}`   Invalid
/// 
/// `(a=b a=c)` Valid
/// 
/// #### Bind
/// 
/// `x:F` --- [bind](#bind) `x` to `F`
/// <br/><br/>
/// 
/// If `F` contains the name `x`, then `x` is a *bound variable* in `F`.
/// 
/// Otherwise, `x` is a *free variable* in `F`.
/// The use of free variables in this language is unknown,
/// further study of lambda calculus is needed here.
/// 
/// If `x` is an [alias](#alias) statement or a [set](#set) with [alias](#alias) statements,
/// then the [alias](#alias) is applied to any names `x` in `F`.
/// 
/// #### Apply
/// 
/// `x.F` --- [apply](#apply) `x` to `F`
/// <br/><br/>
///
/// #### Match
/// 
/// `A->a` --- [match](#match) `A` to `a`
/// 
/// <br/>
///
/// ### Container
/// 
/// #### Set
/// 
/// `{a..z}` --- Contains a number of *unique* expressions.
/// 
/// #### List 
/// 
/// `(a..z)` --- Contains a number of *ordered* expressions.
///  
/// <br/>
/// 
/// ## Rewriting rules
/// 
/// Advanced syntax is generated from the [basic syntax](#language).
///
/// ### Implicit sets
///
/// `a.{x}:F ::= {x=a}.{x}:F`
/// 
/// This replacement applies when the [set](#set) `{x}` contains one [name](#name) only.
/// 
/// Perhaps:
/// 
/// `x ::= {x}`
/// 
/// `{x} ::= x`
/// 
/// Ex:
/// 
/// `x:M ::= {x}:M`
/// 
///  
/// ### Anonymous Operators
/// 
/// #### Left Anonymous
/// 
/// `=a` `:a` `.a` `->a`
/// 
/// #### Right Anonymous
/// 
/// `a=` `a:` `a.` `a->`
/// 
/// <br/>
///
/// ## λ-calculus equivalence
/// 
/// | Name | λ-calc | aql (implicit) | aql (explicit)
/// |-----|---|-----|----|
/// | Variable | `$x$`      | `x`   | <tr></tr> |
/// | Binding | `$(\lambda x.M[x])$` | `x:M` | `{x}:M` <tr></tr> |
/// | Alpha conversion | `$(\lambda x.M[x])\rightarrow(\lambda y.M[y])$` | `{x=y}:x:M` | `{x=y}:{x}:M` <tr></tr> |
/// | Beta conversion | `$((\lambda x.M)\ E)\rightarrow (M[x:=E])$` | `E.M` | `{x=E}.M` <tr></tr> |
/// | Church numeral 0 | `$\lambda f.\lambda x.x$` | `f:x:x` | <tr></tr> |
/// | Church numeral 1 | `$\lambda f.\lambda x.f x$` | `f:x:x.f` | <tr></tr> |
/// | Church numeral 2 | `$\lambda f.\lambda x.f (f x)$` | `f:x:x.f.f` | <tr></tr> |
/// ----------------
///
/// ## Examples:
/// 
/// `a = b.{b:(d->b)}`
/// 
/// Name `a` is aliased to value `b` which matches type `d`
/// 
/// <br/>
///
/// ## Formal grammar
/// 
/// `<expression> ::= <clause> (<seperator> <clause>)*`
/// 
/// `<separator> ::= (' ' | '\t' | '\n' | '\r' | ',')+`
/// 
/// `<clause> ::= <name> | <operation> | <container>`
/// 
/// [`<name>`](#name) `::= ( <letter> | <number> | '_' )+`
/// 
/// [`<operation>`](#operation) `::= <expression> <operand> <expression>`
/// 
/// `<operand> ::= <alias> | <bind> | <apply> | <match>`
/// 
/// [`<alias>`](#alias) `::= '='`
/// 
/// [`<bind>`](#bind) `::= ':'`
/// 
/// [`<apply>`](#apply) `::= '.'`
/// 
/// [`<match>`](#match) `::= '->'`
/// 
/// [`<container>`](#container) `::= <set> | <list>`
/// 
/// [`<set>`](#set) `::= '{' (<expression>)* '}'`
/// 
/// [`<list>`](#list) `::= '(' (<expression>)* ')'`
/// 
/// <br/>
///
#[tokio::main]
async fn main() {
    // let stream = tokio_stream::iter("f:x:x.f".chars());
    let stream = tokio_stream::iter("{x=y}:{x}:M".chars());
    // let stream = tokio_stream::iter("a.b c".chars());

    let mut tree = Graph::<Node, ()>::new();
    let root = tree.add_node(Node::Container(Container::Set));
    let mut subroot = root;

    let tokens = lex(stream);
    pin_mut!(tokens);
    while let Some(token) = tokens.next().await {
        print!("{:?}", token);
        subroot = parse(token, &mut tree, subroot);
        println!(" {:?} :\n{:?}", subroot, Dot::with_config(&tree, &[Config::GraphContentOnly]));
    }

}
