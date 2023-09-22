use std::collections::BTreeSet;
use petgraph::graph::NodeIndex;
use itertools::Itertools;
use crate::parse::{Expression, ToExpression};
use crate::parse::Node;
use crate::parse::Operator;
use crate::parse::Container;
use enum_dispatch::enum_dispatch;

type Entities = Vec<EntityEnum>;
type Names = Vec<Name>;

/// # Entity
/// `$N \cup O \cup C$`
#[enum_dispatch]
trait EntityType {
    /// `$\rightarrow O(e', p, e'') : {e', e''} $`
    fn children(&self) -> Entities;
    fn names(&self) -> Names;
    fn bound(&self) -> Names;
    fn evaluate(&self) -> EntityEnum;
    fn apply(&self, entity: &EntityEnum, free: &Names) -> EntityEnum;
    fn replace(&self, a: &EntityEnum, b: &EntityEnum, free: &Names) -> EntityEnum;
}

fn default_names<T: EntityType>(entity: &T) -> Names {
    let e = entity.children();
    let mut children = e.iter();

    match children.next().map(|first_child|
        children.fold(
            first_child.names(),
            |bound_running, child| 
                BTreeSet::from_iter(
                    bound_running
                ).union(
                    &BTreeSet::from_iter(child.names())
                ).cloned()
                .collect::<Vec<Name>>()
        )
    ) {
        Some(i) => i,
        None => Vec::<Name>::new()
    }
}

/// `$B(e) := e \rightarrow  : \cup \{ B(c) \| c \in C(e) \} $`
fn default_bound<T: EntityType>(entity: &T) -> Names {
    let e = entity.children();
    let mut children = e.iter();

    match children.next().map(|first_child|
        children.fold(
            first_child.bound(),
            |bound_running, child| 
                BTreeSet::from_iter(
                    bound_running
                ).union(
                    &BTreeSet::from_iter(child.bound())
                ).cloned()
                .collect::<Vec<Name>>()
        )
    ) {
        Some(i) => i,
        None => vec![]
    }
}

fn default_to_string(entity: impl ToExpression) -> String {
    return entity.to_expression().to_string()
}

fn entity_free(entity: impl EntityType) -> Names {
    BTreeSet::from_iter(entity.names())
            .difference(&BTreeSet::from_iter(entity.bound()))
            .cloned()
            .collect::<Vec<Name>>()
}

fn union_maybe_name(free: &Names, entity: &EntityEnum) -> Names {
    match entity {
        EntityEnum::Name(n) => 
            BTreeSet::from_iter(free.clone())
                    .union(&BTreeSet::from([n.clone()]))
                    .cloned()
                    .collect::<Vec<Name>>()
        ,
        _ => free.clone()
    }
}

#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub struct Name {
    value: String
}

impl Name {
    fn new(value: String) -> Self {
        Self {
            value
        }
    }
}

impl EntityType for Name {
    /// `$C(e) :=  e \rightarrow N : \varnothing $`
    fn children(&self) -> Entities {
        vec![]
    }

    /// `$NS(e) :=  e \rightarrow N : {e} $`
    fn names(&self) -> Names {
        vec![self.clone()]
    }

    fn bound(&self) -> Names {
        default_bound(self)
    }

    fn evaluate(&self) -> EntityEnum {
        self.clone().into()
    }

    fn apply(&self, _entity: &EntityEnum, _free: &Names) -> EntityEnum {
        self.clone().into()
    }

    fn replace(&self, a: &EntityEnum, b: &EntityEnum, free: &Names) -> EntityEnum {
        match a {
            EntityEnum::Name(name) => {
                if *name == self.clone() {
                    if free.contains(self) {
                        b.clone()
                    } else {
                        self.clone().into()
                    }
                } else {
                    self.clone().into()
                }
            },
            _ => self.clone().into()
        }
    }
}

impl ToExpression for Name {
    fn to_expression(&self) -> Expression {
        let mut expression =  Expression::empty();
        expression.tree.add_node(Node::Name(self.value.clone()));
        expression
    }
}

impl ToString for Name {
    fn to_string(&self) -> String {
        default_to_string(self.clone()) // TODO: pass by reference
    }
}

#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub struct Operation {
    operator: Operator,
    left: Box<EntityEnum>,
    right: Box<EntityEnum>
}

impl Operation {
    fn new(operator: Operator, left: EntityEnum, right: EntityEnum) -> Self {
        Self {
            operator,
            left: Box::new(left),
            right: Box::new(right)
        }
    }
}

impl ToExpression for Operation {
    fn to_expression(&self) -> Expression {
        let mut expression =  Expression::empty();
        let index = expression.tree.add_node(Node::Operator(self.operator.clone()));
        expression.add_expression(&self.left.to_expression(), index, NodeIndex::new(0));
        expression.add_expression(&self.right.to_expression(), index, NodeIndex::new(0));
        expression
    }
}

impl EntityType for Operation {
    fn children(&self) -> Entities {
        vec![
            Box::into_inner(self.left.clone()),
            Box::into_inner(self.right.clone()),
        ]
    }

    fn names(&self) -> Names {
        default_names(self)
    }

    fn bound(&self) -> Names {
        match self.operator {
            Operator::Bind => {
                BTreeSet::<Name>::from_iter(self.right.bound())
                .union(&BTreeSet::<Name>::from_iter(self.right.bound()))
                .cloned()
                .collect::<Vec<Name>>()

            },
            _ => default_bound(self)
        }
    }

    fn evaluate(&self) -> EntityEnum {
        match self.operator {
            Operator::Apply => {
                let free = vec![];
                self.left.apply(
                    &Box::into_inner(self.right.clone()),
                    &free
                )
            },
            _ => Self::new(
                self.operator.clone(),
                self.left.evaluate(),
                self.right.evaluate(),
            ).into()
        }
    }

    fn apply(&self, entity: &EntityEnum, free: &Names) -> EntityEnum {
        match self.operator {
            Operator::Alias => entity.replace(
                &Box::into_inner(self.left.clone()),
                &Box::into_inner(self.right.clone()),
                &union_maybe_name(free, entity)
            ),
            Operator::Bind => self.right.replace(
                &Box::into_inner(self.left.clone()),
                entity,
                &union_maybe_name(free, entity)
            ),
            _ => entity.clone()
        }
    }

    fn replace(&self, a: &EntityEnum, b: &EntityEnum, free: &Names) -> EntityEnum {
        Self::new(
            self.operator.clone(),
            self.left.replace(
                a,
                b,
                free,
            ),
            self.right.replace(
                a,
                b,
                free,
            ),
        ).into()
    }
}

#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub struct Set {
    entities: BTreeSet<EntityEnum>
}

impl Set {
    pub fn new () -> Set {
        return Set {
            entities: BTreeSet::new(),
        }
    }

    pub fn add (&mut self, entity: EntityEnum) {
        self.entities.insert(entity.clone());
    }
}

impl<const N: usize> From<[EntityEnum; N]> for Set {
    fn from(values: [EntityEnum; N]) -> Self {
        return Set {
            entities: BTreeSet::from(values),
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
    fn children(&self) -> Entities {
        self.entities.iter()
                    .cloned()
                    .collect::<Vec<EntityEnum>>()
    }

    fn names(&self) -> Names {
        default_names(self)
    }

    fn bound(&self) -> Names {
        default_names(self)
    }

    fn evaluate(&self) -> EntityEnum {
        Set {
            entities: self.children().iter().map(|child| child.evaluate()).collect()
        }.into()
    }

    fn apply(&self, entity: &EntityEnum, free: &Names) -> EntityEnum {
        Set {
            entities: self.children()
                          .iter()
                          .map(|child| child.apply(
                                entity,
                              &union_maybe_name(free, entity)
                            ))
                          .collect()
        }.into()
    }

    fn replace(&self, a: &EntityEnum, b: &EntityEnum, free: &Names) -> EntityEnum {
        Set {
            entities: self.children()
                          .iter()
                          .map(|child| child.replace(
                              a,
                              b,
                              &union_maybe_name(free, &child)
                          ))
                          .collect()
        }.into()
    }
}

#[enum_dispatch(EntityType)]
#[derive(PartialOrd, PartialEq)]
#[derive(Eq, Hash, Ord, Clone)]
#[derive(Debug)]
pub enum EntityEnum {
    Name,
    Operation,
    Set
}

impl ToExpression for EntityEnum {
    fn to_expression(&self) -> Expression {
        match self {
            EntityEnum::Name(e) => e.to_expression(),
            EntityEnum::Operation(e) => e.to_expression(),
            EntityEnum::Set(e) => e.to_expression(),
        }
    }
}

impl ToString for EntityEnum {
    fn to_string(&self) -> String {
        default_to_string(self.clone())
    }
}

pub fn read(expr: &Expression, index: NodeIndex) -> Option<EntityEnum> {
    return match expr.tree.node_weight(index) {
        Some(Node::Name(n)) => Some(
            Name::new(n.clone()).into()
        ),
        Some(Node::Operator(o)) => {
            match expr.children(index)
                    .map(|child|
                        read(expr, child)
                            .expect("Child should be readable"))
                    .collect_tuple::<(EntityEnum, EntityEnum)>() {
                Some((a, b)) => {
                    match o {
                        Operator::Match => None,
                        _ => Some(Operation::new(o.clone(), a, b).into()),
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
                    Some(set.into())
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

    fn test_entity_monad(
        entity: EntityEnum,
        children: BTreeSet<EntityEnum>,
        names: BTreeSet<Name>,
        bound: BTreeSet<Name>,
        evaluate: EntityEnum,
    ) {
        assert_eq!(BTreeSet::from_iter(entity.children()), children);
        assert_eq!(BTreeSet::from_iter(entity.names()), names);
        assert_eq!(BTreeSet::from_iter(entity.bound()), bound);
        assert_eq!(entity.evaluate(), evaluate);
    }

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

    #[test]
    fn it_reads_empty() {
        // assert_eq!(
        //     read(&Expression::from(vec![]), NodeIndex::new(0)),
        //     Some(Entity::Set(Set::new()))
        // );

        test_entity_monad(
            Set {
                entities: BTreeSet::from([]),
            }.into(),
            BTreeSet::new(),
            BTreeSet::new(),
            BTreeSet::new(),
            Set::new().into()
        );
    }

    #[test]
    fn it_reads_name() {
        test_entity_monad(
            EntityEnum::Name(Name::new(String::from("a"))),
            BTreeSet::new(),
            BTreeSet::from([Name::new(String::from("a"))]),
            BTreeSet::new(),
            EntityEnum::Name(Name::new(String::from("a"))),
        )
    }
    
    #[test]
    fn it_reads_alias() {
        test_entity_monad(
            EntityEnum::Operation(
                Operation::new(
                    Operator::Alias,
                    EntityEnum::Name(Name::new(String::from("a"))),
                    EntityEnum::Name(Name::new(String::from("b"))),
                )
            ),
            BTreeSet::from([
                EntityEnum::Name(Name::new(String::from("a"))),
                EntityEnum::Name(Name::new(String::from("b"))),
            ]),
            BTreeSet::from([
                Name::new(String::from("a")),
                Name::new(String::from("b")),
            ]),
            BTreeSet::new(),
            EntityEnum::Operation(
                Operation::new(
                    Operator::Alias,
                    EntityEnum::Name(Name::new(String::from("a"))),
                    EntityEnum::Name(Name::new(String::from("b"))),
                )
            ),
        )
    }
    
    #[test]
    fn it_reads_bind() {
        test_entity_monad(
            EntityEnum::Operation(
                Operation::new(
                    Operator::Bind,
                    EntityEnum::Name(Name::new(String::from("a"))),
                    EntityEnum::Name(Name::new(String::from("b"))),
                )
            ),
            BTreeSet::from([
                EntityEnum::Name(Name::new(String::from("a"))),
                EntityEnum::Name(Name::new(String::from("b"))),
            ]),
            BTreeSet::from([
                Name::new(String::from("a")),
                Name::new(String::from("b")),
            ]),
            BTreeSet::new(),
            EntityEnum::Operation(
                Operation::new(
                    Operator::Bind,
                    EntityEnum::Name(Name::new(String::from("a"))),
                    EntityEnum::Name(Name::new(String::from("b"))),
                )
            ),
        )
    }
    #[test]
    fn it_reads_identity() {
        test_entity_monad(
            EntityEnum::Operation(Operation::new(
                Operator::Apply,
                EntityEnum::Operation(Operation::new(
                    Operator::Bind,
                    EntityEnum::Name(Name::new(String::from("x"))),
                    EntityEnum::Name(Name::new(String::from("x"))),
                )),
                EntityEnum::Name(Name::new(String::from("x"))),
            )),
            BTreeSet::from([
                EntityEnum::Operation(Operation::new(
                    Operator::Bind,
                    EntityEnum::Name(Name::new(String::from("x"))),
                    EntityEnum::Name(Name::new(String::from("x"))),
                )),
                EntityEnum::Name(Name::new(String::from("x"))),
            ]),
            BTreeSet::from([
                Name::new(String::from("x")),
            ]),
            BTreeSet::new(),
            EntityEnum::Name(Name::new(String::from("x")))
        )
    }
    
    #[test]
    fn it_reads_set() {
        // test_expr!(
        //     Expression::from(&vec![
        //         Token::Special(SpecialCharacter::StartSet),
        //         Token::Special(SpecialCharacter::EndSet),
        //     ]),
        test_entity_monad(
            Set {
                entities: BTreeSet::from([
                    Set {
                        entities: BTreeSet::new(),
                    }.into()
                ]),
            }.into(),
            BTreeSet::from([
                Set {
                    entities: BTreeSet::new(),
                }.into()
            ]),
            BTreeSet::new(),
            BTreeSet::new(),
            Set::new().into()
        );
    }

    // #[test]
    // fn it_reads_nested_set_with_name() {
    //     test_expr!(
    //         Expression::from(&vec![
    //             Token::Special(SpecialCharacter::StartSet),
    //             Token::Name(String::from("a")),
    //             Token::Special(SpecialCharacter::EndSet),
    //         ]),
    //         Set {
    //             entities: BTreeSet::from([
    //                 Entity::Set(Set {
    //                     entities: BTreeSet::from([
    //                         Entity::Name(Name::new(String::from("a")))
    //                     ]),
    //                     names: BTreeMap::from([
    //                         (String::from("a"), Entity::Name(Name::new(String::from("a"))))
    //                     ]),
    //                     bindings: BTreeMap::new()
    //                 })
    //             ]),
    //         }.into()
    //     );
    // }

    // #[test]
    // fn it_reads_nested_set_with_alias() {
    //     test_expr!(
    //         Expression::from(&vec![
    //             Token::Special(SpecialCharacter::StartSet),
    //             Token::Name(String::from("a")),
    //             Token::Special(SpecialCharacter::Alias),
    //             Token::Name(String::from("b")),
    //             Token::Special(SpecialCharacter::EndSet),
    //         ]),
    //         Entity::Set(Set::from((
    //             [
    //                 Entity::Set(Set::from((
    //                     [
    //                         Entity::Alias(Operation::new(
    //                             Entity::Name(Name::new(String::from("a"))),
    //                             Entity::Name(Name::new(String::from("b")))
    //                         ))
    //                     ],
    //                     [
    //                         (
    //                             String::from("a"),
    //                             Entity::Name(Name::new(String::from("b")))
    //                         )
    //                     ], []
    //                 )))
    //             ],
    //             [], []
    //         )))
    //     );
    // }

    // // #[test]
    // // fn it_reads_nested_set_with_binding() {
    // //     assert_eq!(
    // //         read(
    // //             &Expression::from([
    // //                 Token::Special(SpecialCharacter::StartSet),
    // //                 Token::Name(String::from("a")),
    // //                 Token::Special(SpecialCharacter::Bind),
    // //                 Token::Name(String::from("b")),
    // //                 Token::Special(SpecialCharacter::EndSet),
    // //             ]),
    // //             NodeIndex::new(0)
    // //         ),
    // //         Some(Entity::Set(Set {
    // //             items: BTreeSet::from([
    // //                 Entity::Set(Set {
    // //                     items: BTreeSet::from([
    // //                         Entity::Binding(
    // //                             Box::new(Entity::Name(String::from("a"))),
    // //                             Box::new(Entity::Name(String::from("b")))
    // //                         )
    // //                     ]),
    // //                     names: BTreeMap::new()
    // //                 })
    // //             ]),
    // //             names: BTreeMap::new()
    // //         }))
    // //     );
    // // }

    // #[test]
    // fn it_reads_alias_apply() {
    //     test_expr_exec!(
    //         Expression::from(&vec![
    //             // Arg: {a=b}
    //             Token::Special(SpecialCharacter::StartSet),
    //             Token::Name(String::from("a")),
    //             Token::Special(SpecialCharacter::Alias),
    //             Token::Name(String::from("b")),
    //             Token::Special(SpecialCharacter::EndSet),

    //             Token::Special(SpecialCharacter::Apply),

    //             // Fn: {c=a}
    //             Token::Special(SpecialCharacter::StartSet),
    //             Token::Name(String::from("c")),
    //             Token::Special(SpecialCharacter::Alias),
    //             Token::Name(String::from("a")),
    //             Token::Special(SpecialCharacter::EndSet),
    //         ]),
    //         // result: {c=b}
    //         Expression::from(&vec![
    //             Token::Special(SpecialCharacter::StartSet),
    //             Token::Name(String::from("c")),
    //             Token::Special(SpecialCharacter::Alias),
    //             Token::Name(String::from("b")),
    //             Token::Special(SpecialCharacter::EndSet),
    //         ]),
    //         Entity::Set(Set::from((
    //             [
    //                 Entity::Set(Set::from((
    //                     [
    //                         Entity::Alias(Operation::new(
    //                             Entity::Name(Name::new(String::from("c"))),
    //                             Entity::Name(Name::new(String::from("b")))
    //                         ))
    //                     ],
    //                     [
    //                         (
    //                             String::from("c"),
    //                             Entity::Name(Name::new(String::from("b")))
    //                         )
    //                     ], []
    //                 )))
    //             ],
    //             [], []
    //         )))
    //     );
    // }

    // #[test]
    // fn it_does_church_arithmetic () {
    //     let zero = Expression::from(
    //         &String::from("f:x:x").to_tokens()
    //     );
    //     let one = Expression::from(
    //         &String::from("f:x:x.f").to_tokens()
    //     );
    //     let succ = Expression::from(
    //         &String::from("n:f:x:x.f.n").to_tokens()
    //     );

    //     let succ_zero = Expression::from(
    //         &String::from("n:f:x:x.f.n.f:x:x").to_tokens()
    //     );

    //     println!("{:?}", succ_zero);

    //     assert_eq!(
    //         read(&succ_zero, NodeIndex::new(0)).expect("Should exec").to_string(),
    //         one.to_string()
    //     );
    // }
}