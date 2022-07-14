// use crate::{
//     entity::{Entity, EntityKind},
//     resolver::Exposure,
// };
use crate::{Expression, Identifier, LocalDeclarationIndex, Number, Text};
use lushui_ast::{self as ast, Explicitness::*};
use lushui_component::Component;
use lushui_lowered_ast::{Attribute, Attributes, BareAttribute};
// use lushui_session::BuildSession;
use lushui_span::Span;
use lushui_utilities::{difference, DisplayWith};
use std::default::default;

// @Beacon @Task do something smart if spaces differ (which cannot have color)
// like replacing them with a different character like the Unicode space symbol
// or however it is called ("SP")
fn assert_eq(expected: &str, actual: impl AsRef<str>) {
    let actual = actual.as_ref();

    if actual != expected {
        panic!(
            "the actual textual representation of the HIR node does not match the expected one:\n{}",
            difference(expected, actual, ""),
        );
    }
}

trait ComponentExt {
    fn add(&mut self, name: &str, kind: EntityKind) -> Identifier;

    fn add_below(
        &mut self,
        name: &str,
        kind: EntityKind,
        parent: LocalDeclarationIndex,
    ) -> Identifier;
}

impl ComponentExt for Component {
    fn add(&mut self, name: &str, kind: EntityKind) -> Identifier {
        self.add_below(name, kind, self.local_root())
    }

    fn add_below(
        &mut self,
        name: &str,
        kind: EntityKind,
        parent: LocalDeclarationIndex,
    ) -> Identifier {
        let identifier = ast::Identifier::new_unchecked(name.into(), default());
        let entity = Entity {
            source: identifier.clone(),
            parent: Some(parent),
            exposure: Exposure::Unrestricted,
            attributes: default(),
            kind,
        };
        let index = self.bindings.insert(entity);
        Identifier::new(index.global(self), identifier)
    }
}

fn type_() -> Expression {
    Expression::new(default(), default(), crate::BareExpression::Type)
}

#[test]
fn pi_type_application_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let array = component
        .add("Array", EntityKind::untyped_data_type())
        .into_expression();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .into_expression();

    assert_eq(
        "topmost.Array topmost.Int -> Type",
        (Expression::new(
            default(),
            default(),
            crate::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: Expression::new(
                    default(),
                    default(),
                    crate::Application {
                        callee: array,
                        argument: int,
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                codomain: type_(),
            }
            .into(),
        ))
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn pi_type_named_parameter() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let array = component.add("Array", EntityKind::untyped_data_type());
    let int = component.add("Int", EntityKind::untyped_data_type());
    let container = component.add("Container", EntityKind::untyped_data_type());
    let alpha = Identifier::parameter("alpha");

    assert_eq(
        "(alpha: topmost.Array topmost.Int) -> topmost.Container alpha",
        Expression::new(
            default(),
            default(),
            crate::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: Some(alpha.clone()),
                domain: Expression::new(
                    default(),
                    default(),
                    crate::Application {
                        callee: array.into_expression(),
                        argument: int.into_expression(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                codomain: Expression::new(
                    default(),
                    default(),
                    crate::Application {
                        callee: container.into_expression(),
                        argument: alpha.into_expression(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn pi_type_implicit_parameter() {
    let session = BuildSession::test();
    let component = Component::test();

    assert_eq(
        "'(whatever: Type) -> Type",
        Expression::new(
            default(),
            default(),
            crate::PiType {
                explicitness: Implicit,
                laziness: None,
                parameter: Some(Identifier::parameter("whatever")),
                domain: type_(),
                codomain: type_(),
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

/// Compare with [pi_type_two_curried_arguments].
#[test]
fn pi_type_higher_order_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .into_expression();

    assert_eq(
        "(topmost.Int -> topmost.Int) -> topmost.Int",
        Expression::new(
            default(),
            default(),
            crate::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: Expression::new(
                    default(),
                    default(),
                    crate::PiType {
                        explicitness: Explicit,
                        laziness: None,
                        parameter: None,
                        domain: int.clone(),
                        codomain: int.clone(),
                    }
                    .into(),
                ),
                codomain: int,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

/// Compare with [pi_type_higher_order_argument].
#[test]
fn pi_type_two_curried_arguments() {
    let session = BuildSession::test();
    let mut component = Component::test();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .into_expression();
    let text = component
        .add("Text", EntityKind::untyped_data_type())
        .into_expression();

    assert_eq(
        "topmost.Int -> topmost.Text -> Type",
        Expression::new(
            default(),
            default(),
            crate::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: int,
                codomain: Expression::new(
                    default(),
                    default(),
                    crate::PiType {
                        explicitness: Explicit,
                        laziness: None,
                        parameter: None,
                        domain: text,
                        codomain: type_(),
                    }
                    .into(),
                ),
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

/// Compare with [lambda_pi_type_body].
#[test]
fn pi_type_lambda_domain() {
    let session = BuildSession::test();
    let component = Component::test();

    let x = Identifier::parameter("x");

    assert_eq(
        r"(\x => x) -> Type",
        Expression::new(
            default(),
            default(),
            crate::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: Expression::new(
                    default(),
                    default(),
                    crate::Lambda {
                        parameter: x.clone(),
                        parameter_type_annotation: None,
                        body_type_annotation: None,
                        body: x.into_expression(),
                        explicitness: Explicit,
                        laziness: None,
                    }
                    .into(),
                ),
                codomain: type_(),
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn application_three_curried_arguments() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let beta = component.add("beta", EntityKind::UntypedFunction);

    assert_eq(
        "alpha topmost.beta (gamma Type) 0",
        Expression::new(
            default(),
            default(),
            crate::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    crate::Application {
                        callee: Expression::new(
                            default(),
                            default(),
                            crate::Application {
                                callee: Identifier::parameter("alpha").into_expression(),
                                argument: beta.into_expression(),
                                explicitness: Explicit,
                            }
                            .into(),
                        ),
                        argument: Expression::new(
                            default(),
                            default(),
                            crate::Application {
                                callee: Identifier::parameter("gamma").into_expression(),
                                argument: type_(),
                                explicitness: Explicit,
                            }
                            .into(),
                        ),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::new(default(), default(), Number::Nat(0u8.into()).into()),
                explicitness: Explicit,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

/// Compare with [application_lambda_argument].
#[test]
fn application_lambda_last_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let take = component.add("take", EntityKind::UntypedFunction);
    let it = Identifier::parameter("it");

    // we might want to format this special case as `topmost.take \it => it` in the future
    assert_eq(
        r"topmost.take (\it => it)",
        Expression::new(
            default(),
            default(),
            crate::Application {
                callee: take.into_expression(),
                argument: Expression::new(
                    default(),
                    default(),
                    crate::Lambda {
                        parameter: it.clone(),
                        parameter_type_annotation: None,
                        body_type_annotation: None,
                        // technically not correct
                        body: it.into_expression(),
                        explicitness: Explicit,
                        laziness: None,
                    }
                    .into(),
                ),
                explicitness: Explicit,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

/// Compare with [application_lambda_last_argument].
#[test]
fn application_lambda_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let take = component.add("take", EntityKind::UntypedFunction);
    let it = Identifier::parameter("it");

    assert_eq(
        r#"topmost.take (\it => it) "who""#,
        Expression::new(
            default(),
            default(),
            crate::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    crate::Application {
                        callee: take.into_expression(),
                        argument: Expression::new(
                            default(),
                            default(),
                            crate::Lambda {
                                parameter: it.clone(),
                                parameter_type_annotation: None,
                                body_type_annotation: None,
                                // technically not correct
                                body: it.into_expression(),
                                explicitness: Explicit,
                                laziness: None,
                            }
                            .into(),
                        ),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::new(default(), default(), Text::Text("who".into()).into()),
                explicitness: Explicit,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn application_implicit_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let identity = component.add("identity", EntityKind::UntypedFunction);

    assert_eq(
        r"topmost.identity 'Type",
        Expression::new(
            default(),
            default(),
            crate::Application {
                callee: identity.into_expression(),
                argument: type_(),
                explicitness: Implicit,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn application_complex_implicit_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let identity = component.add("identity", EntityKind::UntypedFunction);
    let text = component.add("Text", EntityKind::untyped_data_type());

    assert_eq(
        r"topmost.identity '(prepare topmost.Text)",
        Expression::new(
            default(),
            default(),
            crate::Application {
                callee: identity.into_expression(),
                argument: Expression::new(
                    default(),
                    default(),
                    crate::Application {
                        callee: Identifier::parameter("prepare").into_expression(),
                        argument: text.into_expression(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                explicitness: Implicit,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn application_intrinsic_application_callee() {
    let session = BuildSession::test();
    let component = Component::test();

    assert_eq(
        "eta 10 omicron",
        Expression::new(
            default(),
            default(),
            crate::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    crate::IntrinsicApplication {
                        callee: Identifier::parameter("eta"),
                        arguments: vec![Expression::new(
                            default(),
                            default(),
                            Number::Nat(10u8.into()).into(),
                        )],
                    }
                    .into(),
                ),
                argument: Identifier::parameter("omicron").into_expression(),
                explicitness: Explicit,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn lambda_body_type_annotation() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let output = component.add("Output", EntityKind::untyped_data_type());

    assert_eq(
        r"\input: topmost.Output => 0",
        Expression::new(
            default(),
            default(),
            crate::Lambda {
                parameter: Identifier::parameter("input"),
                parameter_type_annotation: None,
                body_type_annotation: Some(output.into_expression()),
                body: Expression::new(default(), default(), Number::Nat(0u8.into()).into()),
                explicitness: Explicit,
                laziness: None,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn lambda_parameter_type_annotation_body_type_annotation() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let input = component.add("Input", EntityKind::untyped_data_type());
    let output = component.add("Output", EntityKind::untyped_data_type());

    assert_eq(
        r"\(input: topmost.Input): topmost.Output => Type",
        Expression::new(
            default(),
            default(),
            crate::Lambda {
                parameter: Identifier::parameter("input"),
                parameter_type_annotation: Some(input.into_expression()),
                body_type_annotation: Some(output.into_expression()),
                body: type_(),
                explicitness: Explicit,
                laziness: None,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn lambda_implicit_parameter() {
    let session = BuildSession::test();
    let component = Component::test();

    assert_eq(
        r"\'(Input: Type) => Type",
        Expression::new(
            default(),
            default(),
            crate::Lambda {
                parameter: Identifier::parameter("Input"),
                parameter_type_annotation: Some(type_()),
                body_type_annotation: None,
                body: type_(),
                explicitness: Implicit,
                laziness: None,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn lambda_implicit_unannotated_parameter() {
    let session = BuildSession::test();
    let component = Component::test();
    let a = Identifier::parameter("a");

    assert_eq(
        r"\'A => \a => a",
        Expression::new(
            default(),
            default(),
            crate::Lambda {
                parameter: Identifier::parameter("A"),
                parameter_type_annotation: None,
                body_type_annotation: None,
                body: Expression::new(
                    default(),
                    default(),
                    crate::Lambda {
                        parameter: a.clone(),
                        parameter_type_annotation: None,
                        body_type_annotation: None,
                        body: a.into_expression(),
                        explicitness: Explicit,
                        laziness: None,
                    }
                    .into(),
                ),
                explicitness: Implicit,
                laziness: None,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

/// Compare with [pi_type_lambda_domain].
#[test]
fn lambda_pi_type_body() {
    let session = BuildSession::test();
    let component = Component::test();

    let x = Identifier::parameter("x");

    assert_eq(
        r"\x => x -> Type",
        Expression::new(
            default(),
            default(),
            crate::Lambda {
                parameter: x.clone(),
                parameter_type_annotation: None,
                body_type_annotation: None,
                body: Expression::new(
                    default(),
                    default(),
                    crate::PiType {
                        explicitness: Explicit,
                        laziness: None,
                        parameter: None,
                        domain: x.into_expression(),
                        codomain: type_(),
                    }
                    .into(),
                ),
                explicitness: Explicit,
                laziness: None,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn intrinsic_application_no_arguments() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let add = component.add("add", EntityKind::UntypedFunction);

    assert_eq(
        "add",
        Expression::new(
            default(),
            default(),
            crate::IntrinsicApplication {
                callee: add,
                arguments: Vec::new(),
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn intrinsic_application_two_arguments() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let add = component.add("add", EntityKind::UntypedFunction);

    assert_eq(
        "add (add 1 3000) 0",
        Expression::new(
            default(),
            default(),
            crate::IntrinsicApplication {
                callee: add.clone(),
                arguments: vec![
                    Expression::new(
                        default(),
                        default(),
                        crate::IntrinsicApplication {
                            callee: add,
                            arguments: vec![
                                Expression::new(
                                    default(),
                                    default(),
                                    Number::Nat(1u8.into()).into(),
                                ),
                                Expression::new(
                                    default(),
                                    default(),
                                    Number::Nat(3000u16.into()).into(),
                                ),
                            ],
                        }
                        .into(),
                    ),
                    Expression::new(default(), default(), Number::Nat(0u8.into()).into()),
                ],
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    );
}

#[test]
fn attributes() {
    let session = BuildSession::test();
    let component = Component::test();

    assert_eq(
        "== @static @unsafe 3 @static (increment 1)",
        Expression::new(
            default(),
            default(),
            crate::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    crate::Application {
                        callee: Identifier::parameter("==").into_expression(),
                        argument: Expression::new(
                            Attributes(vec![
                                Attribute::new(default(), BareAttribute::Static),
                                Attribute::new(default(), BareAttribute::Unsafe),
                            ]),
                            Span::default(),
                            Number::Nat(3u8.into()).into(),
                        ),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::new(
                    Attributes(vec![Attribute::new(default(), BareAttribute::Static)]),
                    default(),
                    crate::Application {
                        callee: Identifier::parameter("increment").into_expression(),
                        argument: Expression::new(
                            default(),
                            default(),
                            Number::Nat(1u8.into()).into(),
                        ),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                explicitness: Explicit,
            }
            .into(),
        )
        .with((&component, &session))
        .to_string(),
    )
}

#[test]
fn path() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let overarching = component.add("overarching", EntityKind::module());
    let middle = component.add_below(
        "middle",
        EntityKind::module(),
        overarching.local_declaration_index(&component).unwrap(),
    );
    let sink = component.add_below(
        "sink",
        EntityKind::UntypedFunction,
        middle.local_declaration_index(&component).unwrap(),
    );

    assert_eq(
        "topmost.overarching.middle.sink",
        sink.into_expression()
            .with((&component, &session))
            .to_string(),
    );
}

#[test]
fn path_identifier_punctuation_punctuation_identifier_segments() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let overarching = component.add("overarching", EntityKind::module());
    let noisy = component.add_below(
        "&/.~##",
        EntityKind::module(),
        overarching.local_declaration_index(&component).unwrap(),
    );
    let zickzack = component.add_below(
        "^^^",
        EntityKind::module(),
        noisy.local_declaration_index(&component).unwrap(),
    );
    let sink = component.add_below(
        "sink",
        EntityKind::UntypedFunction,
        zickzack.local_declaration_index(&component).unwrap(),
    );

    assert_eq(
        "topmost.overarching.&/.~## . ^^^ .sink",
        sink.into_expression()
            .with((&component, &session))
            .to_string(),
    );
}
