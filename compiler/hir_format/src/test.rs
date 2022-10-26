use crate::Display;
use ast::Explicitness::*;
use hir::{
    Attribute, Attributes, BareAttribute, Entity, EntityKind, Exposure, Expression, Identifier,
    LocalDeclarationIndex, Number, Text,
};
use session::{BuildSession, Component, IdentifierExt, LocalDeclarationIndexExt};
use span::Span;
use std::default::default;
use utilities::{difference, displayed};

// @Beacon @Task do something smart if spaces differ (which cannot have color)
// like replacing them with a different character like the Unicode space symbol
// or however it is called ("SP")
fn assert_format(
    expected: &str,
    actual: &Expression,
    component: &Component,
    session: &BuildSession,
) {
    let actual = displayed(|f| actual.write((component, session), f)).to_string();

    assert!(
        actual == expected,
        "the actual textual representation of the HIR node does not match the expected one:\n{}",
        difference(expected, &actual, "")
    );
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
        self.add_below(name, kind, self.root_local())
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
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        "topmost.Array topmost.Int -> topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: Expression::new(
                    default(),
                    default(),
                    hir::Application {
                        callee: array,
                        argument: int,
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                codomain: type_,
            }
            .into(),
        ),
        &component,
        &session,
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

    assert_format(
        "(alpha: topmost.Array topmost.Int) -> topmost.Container alpha",
        &Expression::new(
            default(),
            default(),
            hir::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: Some(alpha.clone()),
                domain: Expression::new(
                    default(),
                    default(),
                    hir::Application {
                        callee: array.into_expression(),
                        argument: int.into_expression(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                codomain: Expression::new(
                    default(),
                    default(),
                    hir::Application {
                        callee: container.into_expression(),
                        argument: alpha.into_expression(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn pi_type_implicit_parameter() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        "'(whatever: topmost.Type) -> topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::PiType {
                explicitness: Implicit,
                laziness: None,
                parameter: Some(Identifier::parameter("whatever")),
                domain: type_.clone(),
                codomain: type_,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

/// Compare with [`pi_type_two_curried_arguments`].
#[test]
fn pi_type_higher_order_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        "(topmost.Int -> topmost.Int) -> topmost.Int",
        &Expression::new(
            default(),
            default(),
            hir::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: Expression::new(
                    default(),
                    default(),
                    hir::PiType {
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
        ),
        &component,
        &session,
    );
}

/// Compare with [`pi_type_higher_order_argument`].
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
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        "topmost.Int -> topmost.Text -> topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: int,
                codomain: Expression::new(
                    default(),
                    default(),
                    hir::PiType {
                        explicitness: Explicit,
                        laziness: None,
                        parameter: None,
                        domain: text,
                        codomain: type_,
                    }
                    .into(),
                ),
            }
            .into(),
        ),
        &component,
        &session,
    );
}

/// Compare with [`lambda_pi_type_body`].
#[test]
fn pi_type_lambda_domain() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    let x = Identifier::parameter("x");

    assert_format(
        r"(\x => x) -> topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::PiType {
                explicitness: Explicit,
                laziness: None,
                parameter: None,
                domain: Expression::new(
                    default(),
                    default(),
                    hir::Lambda {
                        parameter: x.clone(),
                        parameter_type_annotation: None,
                        body_type_annotation: None,
                        body: x.into_expression(),
                        explicitness: Explicit,
                        laziness: None,
                    }
                    .into(),
                ),
                codomain: type_,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn application_three_curried_arguments() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let beta = component.add("beta", EntityKind::UntypedFunction);
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        "alpha topmost.beta (gamma topmost.Type) 0",
        &Expression::new(
            default(),
            default(),
            hir::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    hir::Application {
                        callee: Expression::new(
                            default(),
                            default(),
                            hir::Application {
                                callee: Identifier::parameter("alpha").into_expression(),
                                argument: beta.into_expression(),
                                explicitness: Explicit,
                            }
                            .into(),
                        ),
                        argument: Expression::new(
                            default(),
                            default(),
                            hir::Application {
                                callee: Identifier::parameter("gamma").into_expression(),
                                argument: type_,
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
        ),
        &component,
        &session,
    );
}

/// Compare with [`application_lambda_argument`].
#[test]
fn application_lambda_last_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let take = component.add("take", EntityKind::UntypedFunction);
    let it = Identifier::parameter("it");

    // we might want to format this special case as `topmost.take \it => it` in the future
    assert_format(
        r"topmost.take (\it => it)",
        &Expression::new(
            default(),
            default(),
            hir::Application {
                callee: take.into_expression(),
                argument: Expression::new(
                    default(),
                    default(),
                    hir::Lambda {
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
        &component,
        &session,
    );
}

/// Compare with [`application_lambda_last_argument`].
#[test]
fn application_lambda_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let take = component.add("take", EntityKind::UntypedFunction);
    let it = Identifier::parameter("it");

    assert_format(
        r#"topmost.take (\it => it) "who""#,
        &Expression::new(
            default(),
            default(),
            hir::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    hir::Application {
                        callee: take.into_expression(),
                        argument: Expression::new(
                            default(),
                            default(),
                            hir::Lambda {
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
        ),
        &component,
        &session,
    );
}

#[test]
fn application_implicit_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let identity = component.add("identity", EntityKind::UntypedFunction);
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        r"topmost.identity 'topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::Application {
                callee: identity.into_expression(),
                argument: type_,
                explicitness: Implicit,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn application_complex_implicit_argument() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let identity = component.add("identity", EntityKind::UntypedFunction);
    let text = component.add("Text", EntityKind::untyped_data_type());

    assert_format(
        r"topmost.identity '(prepare topmost.Text)",
        &Expression::new(
            default(),
            default(),
            hir::Application {
                callee: identity.into_expression(),
                argument: Expression::new(
                    default(),
                    default(),
                    hir::Application {
                        callee: Identifier::parameter("prepare").into_expression(),
                        argument: text.into_expression(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                explicitness: Implicit,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn application_intrinsic_application_callee() {
    let session = BuildSession::test();
    let component = Component::test();

    assert_format(
        "eta 10 omicron",
        &Expression::new(
            default(),
            default(),
            hir::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    hir::IntrinsicApplication {
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
        ),
        &component,
        &session,
    );
}

#[test]
fn lambda_body_type_annotation() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let output = component.add("Output", EntityKind::untyped_data_type());

    assert_format(
        r"\input: topmost.Output => 0",
        &Expression::new(
            default(),
            default(),
            hir::Lambda {
                parameter: Identifier::parameter("input"),
                parameter_type_annotation: None,
                body_type_annotation: Some(output.into_expression()),
                body: Expression::new(default(), default(), Number::Nat(0u8.into()).into()),
                explicitness: Explicit,
                laziness: None,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn lambda_parameter_type_annotation_body_type_annotation() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let input = component.add("Input", EntityKind::untyped_data_type());
    let output = component.add("Output", EntityKind::untyped_data_type());
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        r"\(input: topmost.Input): topmost.Output => topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::Lambda {
                parameter: Identifier::parameter("input"),
                parameter_type_annotation: Some(input.into_expression()),
                body_type_annotation: Some(output.into_expression()),
                body: type_,
                explicitness: Explicit,
                laziness: None,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn lambda_implicit_parameter() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    assert_format(
        r"\'(Input: topmost.Type) => topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::Lambda {
                parameter: Identifier::parameter("Input"),
                parameter_type_annotation: Some(type_.clone()),
                body_type_annotation: None,
                body: type_,
                explicitness: Implicit,
                laziness: None,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn lambda_implicit_unannotated_parameter() {
    let session = BuildSession::test();
    let component = Component::test();
    let a = Identifier::parameter("a");

    assert_format(
        r"\'A => \a => a",
        &Expression::new(
            default(),
            default(),
            hir::Lambda {
                parameter: Identifier::parameter("A"),
                parameter_type_annotation: None,
                body_type_annotation: None,
                body: Expression::new(
                    default(),
                    default(),
                    hir::Lambda {
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
        ),
        &component,
        &session,
    );
}

/// Compare with [`pi_type_lambda_domain`].
#[test]
fn lambda_pi_type_body() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_expression();

    let x = Identifier::parameter("x");

    assert_format(
        r"\x => x -> topmost.Type",
        &Expression::new(
            default(),
            default(),
            hir::Lambda {
                parameter: x.clone(),
                parameter_type_annotation: None,
                body_type_annotation: None,
                body: Expression::new(
                    default(),
                    default(),
                    hir::PiType {
                        explicitness: Explicit,
                        laziness: None,
                        parameter: None,
                        domain: x.into_expression(),
                        codomain: type_,
                    }
                    .into(),
                ),
                explicitness: Explicit,
                laziness: None,
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn intrinsic_application_no_arguments() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let add = component.add("add", EntityKind::UntypedFunction);

    assert_format(
        "add",
        &Expression::new(
            default(),
            default(),
            hir::IntrinsicApplication {
                callee: add,
                arguments: Vec::new(),
            }
            .into(),
        ),
        &component,
        &session,
    );
}

#[test]
fn intrinsic_application_two_arguments() {
    let session = BuildSession::test();
    let mut component = Component::test();

    let add = component.add("add", EntityKind::UntypedFunction);

    assert_format(
        "add (add 1 3000) 0",
        &Expression::new(
            default(),
            default(),
            hir::IntrinsicApplication {
                callee: add.clone(),
                arguments: vec![
                    Expression::new(
                        default(),
                        default(),
                        hir::IntrinsicApplication {
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
        ),
        &component,
        &session,
    );
}

#[test]
fn attributes() {
    let session = BuildSession::test();
    let component = Component::test();

    assert_format(
        "== @static @unsafe 3 @static (increment 1)",
        &Expression::new(
            default(),
            default(),
            hir::Application {
                callee: Expression::new(
                    default(),
                    default(),
                    hir::Application {
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
                    hir::Application {
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
        ),
        &component,
        &session,
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

    assert_format(
        "topmost.overarching.middle.sink",
        &sink.into_expression(),
        &component,
        &session,
    );
}

#[test]
fn path_identifier_symbol_symbol_identifier_segments() {
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

    assert_format(
        "topmost.overarching.&/.~## . ^^^ .sink",
        &sink.into_expression(),
        &component,
        &session,
    );
}
