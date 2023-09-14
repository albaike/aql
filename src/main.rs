use futures_util::pin_mut;
use futures_util::stream::StreamExt;
use petgraph::Graph;
use petgraph::dot::{Dot, Config};

use crate::lex::lex;
mod lex;

use crate::parse::Container;
use crate::parse::Node;
use crate::parse::parse;
mod parse;

// parser


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
/// | Identity | `$\lambda x.x$` | `x:x` | `{x}:x` <tr></tr> |
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
