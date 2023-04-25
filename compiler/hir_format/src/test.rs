use crate::Display;
use ast::ParameterKind::*;
use hir::{
    Attribute, Attributes, BareAttribute, Entity, EntityKind, Exposure, Expression, Identifier,
    LocalDeclarationIndex, Number, Text,
};
use session::{
    component::{Component, IdentifierExt, LocalDeclarationIndexExt},
    Context, Session,
};
use span::Span;
use utility::default;
use utility::{difference, displayed};

// @Beacon @Task do something smart if spaces differ (which cannot have color)
// like replacing them with a different character like the Unicode space symbol
// or however it is called ("SP")
fn assert_format(expected: &str, actual: &Expression, session: &Session<'_>) {
    let actual = displayed(|f| actual.write(session, f)).to_string();

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
        let identifier = ast::Identifier::new_unchecked(default(), name.into());
        let entity = Entity {
            source: identifier,
            parent: Some(parent),
            exposure: Exposure::Unrestricted,
            attributes: default(),
            kind,
        };
        let index = self.bindings.insert(entity);
        Identifier::new(index.global(self), identifier)
    }
}

fn parameter(name: &str) -> Identifier {
    Identifier::new(
        hir::Index::Parameter,
        ast::Identifier::new_unchecked(default(), name.into()),
    )
}

#[test]
fn pi_type_application_argument() {
    let mut component = Component::mock();
    let array = component
        .add("Array", EntityKind::untyped_data_type())
        .to_item();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .to_item();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "topmost.Array topmost.Int -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                kind: Explicit,
                binder: None,
                domain: Expression::bare(
                    hir::Application {
                        callee: array,
                        argument: int,
                        kind: Explicit,
                    }
                    .into(),
                ),
                codomain: type_,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn pi_type_named_parameter() {
    let mut component = Component::mock();
    let array = component.add("Array", EntityKind::untyped_data_type());
    let int = component.add("Int", EntityKind::untyped_data_type());
    let container = component.add("Container", EntityKind::untyped_data_type());
    let alpha = parameter("alpha");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "For (alpha: topmost.Array topmost.Int) -> topmost.Container alpha",
        &Expression::bare(
            hir::PiType {
                kind: Explicit,
                binder: Some(alpha),
                domain: Expression::bare(
                    hir::Application {
                        callee: array.to_item(),
                        argument: int.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
                codomain: Expression::bare(
                    hir::Application {
                        callee: container.to_item(),
                        argument: alpha.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn pi_type_implicit_parameter() {
    let mut component = Component::mock();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "For '(whatever: topmost.Type) -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                kind: Implicit,
                binder: Some(parameter("whatever")),
                domain: type_.clone(),
                codomain: type_,
            }
            .into(),
        ),
        &session,
    );
}

/// Compare with [`pi_type_two_curried_arguments`].
#[test]
fn pi_type_higher_order_argument() {
    let mut component = Component::mock();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "(topmost.Int -> topmost.Int) -> topmost.Int",
        &Expression::bare(
            hir::PiType {
                kind: Explicit,
                binder: None,
                domain: Expression::bare(
                    hir::PiType {
                        kind: Explicit,
                        binder: None,
                        domain: int.clone(),
                        codomain: int.clone(),
                    }
                    .into(),
                ),
                codomain: int,
            }
            .into(),
        ),
        &session,
    );
}

/// Compare with [`pi_type_higher_order_argument`].
#[test]
fn pi_type_two_curried_arguments() {
    let mut component = Component::mock();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .to_item();
    let text = component
        .add("Text", EntityKind::untyped_data_type())
        .to_item();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "topmost.Int -> topmost.Text -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                kind: Explicit,
                binder: None,
                domain: int,
                codomain: Expression::bare(
                    hir::PiType {
                        kind: Explicit,
                        binder: None,
                        domain: text,
                        codomain: type_,
                    }
                    .into(),
                ),
            }
            .into(),
        ),
        &session,
    );
}

/// Compare with [`lambda_pi_type_body`].
#[test]
fn pi_type_lambda_domain() {
    let mut component = Component::mock();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();
    let x = parameter("x");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "(for x => x) -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                kind: Explicit,
                binder: None,
                domain: Expression::bare(
                    hir::Lambda {
                        binder: Some(x),
                        domain: None,
                        codomain: None,
                        body: x.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
                codomain: type_,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn application_three_curried_arguments() {
    let mut component = Component::mock();
    let beta = component.add("beta", EntityKind::UntypedFunction);
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "alpha topmost.beta (gamma topmost.Type) 0",
        &Expression::bare(
            hir::Application {
                callee: Expression::bare(
                    hir::Application {
                        callee: Expression::bare(
                            hir::Application {
                                callee: parameter("alpha").to_item(),
                                argument: beta.to_item(),
                                kind: Explicit,
                            }
                            .into(),
                        ),
                        argument: Expression::bare(
                            hir::Application {
                                callee: parameter("gamma").to_item(),
                                argument: type_,
                                kind: Explicit,
                            }
                            .into(),
                        ),
                        kind: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::bare(Number::Nat(0u8.into()).into()),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

/// Compare with [`application_lambda_argument`].
#[test]
fn application_lambda_last_argument() {
    let mut component = Component::mock();
    let take = component.add("take", EntityKind::UntypedFunction);
    let it = parameter("it");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    // we might want to format this special case as `topmost.take for it => it` in the future
    assert_format(
        "topmost.take (for it => it)",
        &Expression::bare(
            hir::Application {
                callee: take.to_item(),
                argument: Expression::bare(
                    hir::Lambda {
                        binder: Some(it),
                        domain: None,
                        codomain: None,
                        // technically not correct
                        body: it.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

/// Compare with [`application_lambda_last_argument`].
#[test]
fn application_lambda_argument() {
    let mut component = Component::mock();
    let take = component.add("take", EntityKind::UntypedFunction);
    let it = parameter("it");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r#"topmost.take (for it => it) "who""#,
        &Expression::bare(
            hir::Application {
                callee: Expression::bare(
                    hir::Application {
                        callee: take.to_item(),
                        argument: Expression::bare(
                            hir::Lambda {
                                binder: Some(it),
                                domain: None,
                                codomain: None,
                                // technically not correct
                                body: it.to_item(),
                                kind: Explicit,
                            }
                            .into(),
                        ),
                        kind: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::bare(Text::Text("who".into()).into()),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn application_implicit_argument() {
    let mut component = Component::mock();
    let identity = component.add("identity", EntityKind::UntypedFunction);
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"topmost.identity 'topmost.Type",
        &Expression::bare(
            hir::Application {
                callee: identity.to_item(),
                argument: type_,
                kind: Implicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn application_complex_implicit_argument() {
    let mut component = Component::mock();
    let identity = component.add("identity", EntityKind::UntypedFunction);
    let text = component.add("Text", EntityKind::untyped_data_type());

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"topmost.identity '(prepare topmost.Text)",
        &Expression::bare(
            hir::Application {
                callee: identity.to_item(),
                argument: Expression::bare(
                    hir::Application {
                        callee: parameter("prepare").to_item(),
                        argument: text.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
                kind: Implicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn application_intrinsic_application_callee() {
    let mut context = Context::mock();
    let session = Session::new(Component::mock(), &mut context);

    assert_format(
        "eta 10 omicron",
        &Expression::bare(
            hir::Application {
                callee: Expression::bare(
                    hir::IntrinsicApplication {
                        callee: parameter("eta"),
                        arguments: vec![Expression::bare(Number::Nat(10u8.into()).into())],
                    }
                    .into(),
                ),
                argument: parameter("omicron").to_item(),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_body_type_annotation() {
    let mut component = Component::mock();
    let output = component.add("Output", EntityKind::untyped_data_type());

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "for input: topmost.Output => 0",
        &Expression::bare(
            hir::Lambda {
                binder: Some(parameter("input")),
                domain: None,
                codomain: Some(output.to_item()),
                body: Expression::bare(Number::Nat(0u8.into()).into()),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_parameter_type_annotation_body_type_annotation() {
    let mut component = Component::mock();
    let input = component.add("Input", EntityKind::untyped_data_type());
    let output = component.add("Output", EntityKind::untyped_data_type());
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "for (input: topmost.Input): topmost.Output => topmost.Type",
        &Expression::bare(
            hir::Lambda {
                binder: Some(parameter("input")),
                domain: Some(input.to_item()),
                codomain: Some(output.to_item()),
                body: type_,
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_implicit_parameter() {
    let mut component = Component::mock();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "for '(Input: topmost.Type) => topmost.Type",
        &Expression::bare(
            hir::Lambda {
                binder: Some(parameter("Input")),
                domain: Some(type_.clone()),
                codomain: None,
                body: type_,
                kind: Implicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_implicit_unannotated_parameter() {
    let a = parameter("a");

    let mut context = Context::mock();
    let session = Session::new(Component::mock(), &mut context);

    assert_format(
        "for 'A => for a => a",
        &Expression::bare(
            hir::Lambda {
                binder: Some(parameter("A")),
                domain: None,
                codomain: None,
                body: Expression::bare(
                    hir::Lambda {
                        binder: Some(a),
                        domain: None,
                        codomain: None,
                        body: a.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
                kind: Implicit,
            }
            .into(),
        ),
        &session,
    );
}

/// Compare with [`pi_type_lambda_domain`].
#[test]
fn lambda_pi_type_body() {
    let mut component = Component::mock();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .to_item();
    let x = parameter("x");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"for x => x -> topmost.Type",
        &Expression::bare(
            hir::Lambda {
                binder: Some(x),
                domain: None,
                codomain: None,
                body: Expression::bare(
                    hir::PiType {
                        kind: Explicit,
                        binder: None,
                        domain: x.to_item(),
                        codomain: type_,
                    }
                    .into(),
                ),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn intrinsic_application_no_arguments() {
    let mut component = Component::mock();
    let add = component.add("add", EntityKind::UntypedFunction);

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "add",
        &Expression::bare(
            hir::IntrinsicApplication {
                callee: add,
                arguments: Vec::new(),
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn intrinsic_application_two_arguments() {
    let mut component = Component::mock();
    let add = component.add("add", EntityKind::UntypedFunction);

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "add (add 1 3000) 0",
        &Expression::bare(
            hir::IntrinsicApplication {
                callee: add,
                arguments: vec![
                    Expression::bare(
                        hir::IntrinsicApplication {
                            callee: add,
                            arguments: vec![
                                Expression::bare(Number::Nat(1u8.into()).into()),
                                Expression::bare(Number::Nat(3000u16.into()).into()),
                            ],
                        }
                        .into(),
                    ),
                    Expression::bare(Number::Nat(0u8.into()).into()),
                ],
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn attributes() {
    let mut context = Context::mock();
    let session = Session::new(Component::mock(), &mut context);

    assert_format(
        "== @static @unsafe 3 @static (increment 1)",
        &Expression::bare(
            hir::Application {
                callee: Expression::bare(
                    hir::Application {
                        callee: parameter("==").to_item(),
                        argument: Expression::new(
                            Attributes(vec![
                                Attribute::new(default(), BareAttribute::Static),
                                Attribute::new(default(), BareAttribute::Unsafe),
                            ]),
                            Span::default(),
                            Number::Nat(3u8.into()).into(),
                        ),
                        kind: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::new(
                    Attributes(vec![Attribute::new(default(), BareAttribute::Static)]),
                    default(),
                    hir::Application {
                        callee: parameter("increment").to_item(),
                        argument: Expression::bare(Number::Nat(1u8.into()).into()),
                        kind: Explicit,
                    }
                    .into(),
                ),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    )
}

#[test]
fn path() {
    let mut component = Component::mock();
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

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format("topmost.overarching.middle.sink", &sink.to_item(), &session);
}

#[test]
fn path_identifier_symbol_symbol_identifier_segments() {
    let mut component = Component::mock();
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

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "topmost.overarching.&/.~## . ^^^ .sink",
        &sink.to_item(),
        &session,
    );
}
