use std::collections::{BTreeMap, BTreeSet};
use petgraph::graph::NodeIndex;
use itertools::Itertools;
// use itertools::collect_tuple;
use crate::parse::{Expression, ToExpression};
use crate::parse::Node;
use crate::parse::Operand;
use crate::parse::Container;

trait EntityType {
    fn rename(&self, a: Entity, b:Entity) -> Option<Entity>;
}

#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub struct Name {
    value: String
}

impl Name {
    fn new (value: String) -> Self {
        return Name {
            value
        };
    }
}

impl EntityType for Name {
    fn rename(&self, a: Entity, b: Entity) -> Option<Entity> {
        return match a {
            Entity::Name(name) => {
                return if name == *self {
                    Some(b)
                } else {
                    None
                }
            },
            _ => None
        };
    }
}

impl ToExpression for Name {
    fn to_expression(&self) -> Expression {
        let mut expression =  Expression::empty();
        expression.tree.add_node(Node::Name(self.value.clone()));
        return expression;
    }
}

#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub struct Operation {
    left: Box<Entity>,
    right: Box<Entity>
}

impl Operation {
    fn new(left: Entity, right: Entity) -> Self {
        return Operation {
            left: Box::new(left),
            right: Box::new(right)
        }
    }

    fn to_expression(&self, operand: Operand) -> Expression {
        let mut expression =  Expression::empty();
        let index = expression.tree.add_node(Node::Operand(operand));
        expression.add_expression(&self.left.to_expression(), index, NodeIndex::new(0));
        expression.add_expression(&self.right.to_expression(), index, NodeIndex::new(0));
        return expression;
    }
}

impl EntityType for Operation {
    fn rename(&self, a: Entity, b: Entity) -> Option<Entity> {
        return match (*self.right).rename(a, b) {
            Some(replaced) => Some(Entity::Alias(Operation {
                left: self.left.clone(),
                right: Box::new(replaced),
            })),
            None => None
        }
    }
}

#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub struct Set {
    entities: BTreeSet<Entity>,
    names: BTreeMap<String, Entity>,
    bindings: BTreeMap<String, Entity>,
}

impl Set {
    pub fn new () -> Set {
        return Set {
            entities: BTreeSet::new(),
            names: BTreeMap::new(),
            bindings: BTreeMap::new(),
        }
    }

    // TODO: validate that sets have unique names
    pub fn add (&mut self, entity: Entity) {
        self.entities.insert(entity.clone());

        match entity.clone() {
            Entity::Name(n) => {
                self.names.insert(n.value, entity);
            },
            Entity::Alias(alias) => {
                match *(alias.left) {
                    Entity::Name(name) => {
                        self.names.insert(
                            name.value,
                            *(alias.right)
                        );
                    },
                    _ => ()
                }
            },
            Entity::Binding(binding) => {
                match *(binding.left) {
                    Entity::Name(name) => {
                        self.bindings.insert(
                            name.value,
                            *(binding.right)
                        );
                    },
                    _ => ()
                }
            },
            _ => ()
        }
    }

    pub fn apply(&self, entity: Entity) -> Self {
        let mut source = self.clone();
        match entity {
            Entity::Set(set) => {
                for e in set.entities {
                    source = source.apply(e);
                }
            },
            Entity::Alias(alias) => {
                match source.rename(*(alias.left), *(alias.right)) {
                    Some(Entity::Set(s)) => {
                        source = s;
                    },
                    _ => ()
                }
            },
            _ => ()
        }
        return source
    }
}

impl<const N: usize, const M: usize, const O: usize> From<([Entity; N], [(String, Entity); M], [(String, Entity); O])> for Set {
    fn from(values: ([Entity; N], [(String, Entity); M], [(String, Entity); O])) -> Self {
        return Set {
            entities: BTreeSet::from(values.0),
            names: BTreeMap::from(values.1),
            bindings: BTreeMap::from(values.2),
        }
    }
}

impl ToExpression for Set {
    fn to_expression(&self) -> Expression {
        let mut expression =  Expression::empty();
        let index = expression.tree.add_node(Node::Container(Container::Set));

        for item in self.entities.clone() {
            expression.add_expression(&item.to_expression(), index, NodeIndex::new(0));
        }

        return expression;
    }
}

impl EntityType for Set {
    fn rename(&self, a: Entity, b: Entity) -> Option<Entity> {
        let mut new = self.clone();
        let mut renamed = false;
        for e in self.entities.clone() {
            match e.rename(a.clone(), b.clone()) {
                Some(replaced) => {
                    new.entities.remove(&e);
                    new.add(replaced);
                    renamed = true;
                },
                None => ()
            }
        }

        return if renamed {
            Some(Entity::Set(new))
        } else {
            None
        };
    }
}

#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub enum Entity {
    Set(Set),
    Name(Name),
    Alias(Operation),
    Binding(Operation)
}

impl EntityType for Entity {
    fn rename(&self, a: Entity, b: Entity) -> Option<Entity> {
        return match self {
            Entity::Set(e) => e.rename(a, b),
            Entity::Name(e) => e.rename(a, b),
            Entity::Alias(e) => e.rename(a, b),
            Entity::Binding(e) => e.rename(a, b),
        }
    }
}

impl ToExpression for Entity {
    fn to_expression(&self) -> Expression {
        return match self {
            Entity::Set(e) => e.to_expression(),
            Entity::Name(e) => e.to_expression(),
            Entity::Alias(e) => e.to_expression(Operand::Alias),
            Entity::Binding(e) => e.to_expression(Operand::Bind),
        }
    }
}

impl ToString for Entity {
    fn to_string(&self) -> String {
        return self.to_expression().to_string();
    }
}

// struct Engine {
//     root: Set
// }

pub fn read(expr: &Expression, index: NodeIndex) -> Option<Entity> {
    return match expr.tree.node_weight(index) {
        Some(Node::Name(n)) => Some(
            Entity::Name(Name::new(String::from(n))),
        ),
        Some(Node::Operand(o)) => {
            match expr.children(index)
                    .map(|child|
                        read(expr, child)
                            .expect("Child should be readable"))
                    .collect_tuple::<(Entity, Entity)>() {
                Some((a, b)) => {
                    match o {
                        Operand::Alias => Some(Entity::Alias(Operation::new(a, b))),
                        Operand::Apply => {
                            match b {
                                Entity::Set(b_set) => Some(Entity::Set(
                                    b_set.apply(a)
                                )),
                                _ => None
                            }
                        },
                        _ => None
                    }
                },
                _ => None
            }
        },
        Some(Node::Container(n)) => {
            match n {
                Container::Set => {
                    let mut set = Set::new();
                    for child in expr.children(index) {
                        match read(expr, child) {
                            Some(entity) => set.add(entity),
                            _ => {}
                        }
                    }
                    Some(Entity::Set(set))
                },
                _ => None
            }
        },
        _ => None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::{Token, ToTokens};
    use crate::lex::SpecialCharacter;

    macro_rules! test_expr {
        ($expr: expr, $entity: expr) => {{
            assert_eq!(
                read(&$expr, NodeIndex::new(0)),
                Some($entity)
            );
            println!("{:?}", $entity.to_expression());
            assert_eq!(
                $expr.to_tokens(),
                $entity.to_expression().to_tokens()
            );
        }}
    }

    macro_rules! test_expr_exec {
        ($expr: expr, $exec: expr, $entity: expr) => {{
            assert_eq!(
                read(&$expr, NodeIndex::new(0)),
                Some($entity)
            );
            assert_eq!(
                $exec.to_tokens(),
                $entity.to_expression().to_tokens()
            );
        }}
    }

    // #[test]
    // fn it_reads_empty() {
    //     assert_eq!(
    //         read(&Expression::from(vec![]), NodeIndex::new(0)),
    //         Some(Entity::Set(Set::new()))
    //     );

    // }
    
    #[test]
    fn it_reads_nested_set() {
        test_expr!(
            Expression::from(&vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Special(SpecialCharacter::EndSet),
            ]),
            Entity::Set(Set {
                entities: BTreeSet::from([
                    Entity::Set(Set {
                        entities: BTreeSet::new(),
                        names: BTreeMap::new(),
                        bindings: BTreeMap::new()
                    })
                ]),
                names: BTreeMap::new(),
                bindings: BTreeMap::new()
            })
        );
    }

    #[test]
    fn it_reads_nested_set_with_name() {
        test_expr!(
            Expression::from(&vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("a")),
                Token::Special(SpecialCharacter::EndSet),
            ]),
            Entity::Set(Set {
                entities: BTreeSet::from([
                    Entity::Set(Set {
                        entities: BTreeSet::from([
                            Entity::Name(Name::new(String::from("a")))
                        ]),
                        names: BTreeMap::from([
                            (String::from("a"), Entity::Name(Name::new(String::from("a"))))
                        ]),
                        bindings: BTreeMap::new()
                    })
                ]),
                names: BTreeMap::new(),
                bindings: BTreeMap::new()
            })
        );
    }

    #[test]
    fn it_reads_nested_set_with_alias() {
        test_expr!(
            Expression::from(&vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("a")),
                Token::Special(SpecialCharacter::Alias),
                Token::Name(String::from("b")),
                Token::Special(SpecialCharacter::EndSet),
            ]),
            Entity::Set(Set::from((
                [
                    Entity::Set(Set::from((
                        [
                            Entity::Alias(Operation::new(
                                Entity::Name(Name::new(String::from("a"))),
                                Entity::Name(Name::new(String::from("b")))
                            ))
                        ],
                        [
                            (
                                String::from("a"),
                                Entity::Name(Name::new(String::from("b")))
                            )
                        ], []
                    )))
                ],
                [], []
            )))
        );
    }

    // #[test]
    // fn it_reads_nested_set_with_binding() {
    //     assert_eq!(
    //         read(
    //             &Expression::from([
    //                 Token::Special(SpecialCharacter::StartSet),
    //                 Token::Name(String::from("a")),
    //                 Token::Special(SpecialCharacter::Bind),
    //                 Token::Name(String::from("b")),
    //                 Token::Special(SpecialCharacter::EndSet),
    //             ]),
    //             NodeIndex::new(0)
    //         ),
    //         Some(Entity::Set(Set {
    //             items: BTreeSet::from([
    //                 Entity::Set(Set {
    //                     items: BTreeSet::from([
    //                         Entity::Binding(
    //                             Box::new(Entity::Name(String::from("a"))),
    //                             Box::new(Entity::Name(String::from("b")))
    //                         )
    //                     ]),
    //                     names: BTreeMap::new()
    //                 })
    //             ]),
    //             names: BTreeMap::new()
    //         }))
    //     );
    // }

    #[test]
    fn it_reads_alias_apply() {
        test_expr_exec!(
            Expression::from(&vec![
                // Arg: {a=b}
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("a")),
                Token::Special(SpecialCharacter::Alias),
                Token::Name(String::from("b")),
                Token::Special(SpecialCharacter::EndSet),

                Token::Special(SpecialCharacter::Apply),

                // Fn: {c=a}
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("c")),
                Token::Special(SpecialCharacter::Alias),
                Token::Name(String::from("a")),
                Token::Special(SpecialCharacter::EndSet),
            ]),
            // result: {c=b}
            Expression::from(&vec![
                Token::Special(SpecialCharacter::StartSet),
                Token::Name(String::from("c")),
                Token::Special(SpecialCharacter::Alias),
                Token::Name(String::from("b")),
                Token::Special(SpecialCharacter::EndSet),
            ]),
            Entity::Set(Set::from((
                [
                    Entity::Set(Set::from((
                        [
                            Entity::Alias(Operation::new(
                                Entity::Name(Name::new(String::from("c"))),
                                Entity::Name(Name::new(String::from("b")))
                            ))
                        ],
                        [
                            (
                                String::from("c"),
                                Entity::Name(Name::new(String::from("b")))
                            )
                        ], []
                    )))
                ],
                [], []
            )))
        );
    }

    #[test]
    fn it_does_church_arithmetic () {
        let zero = Expression::from(
            &String::from("f:x:x").to_tokens()
        );
        let one = Expression::from(
            &String::from("f:x:x.f").to_tokens()
        );
        let succ = Expression::from(
            &String::from("n:f:x:x.f.n").to_tokens()
        );

        let succ_zero = Expression::from(
            &String::from("n:f:x:x.f.n.f:x:x").to_tokens()
        );

        println!("{:?}", succ_zero);

        assert_eq!(
            read(&succ_zero, NodeIndex::new(0)).expect("Should exec").to_string(),
            one.to_string()
        );
    }
}