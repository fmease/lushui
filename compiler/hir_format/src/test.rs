use crate::Display;
use ast::Explicitness::*;
use hir::{
    Attribute, Attributes, BareAttribute, Entity, EntityKind, Exposure, Expression, Identifier,
    LocalDeclarationIndex, Number, Text,
};
use session::{
    component::{Component, IdentifierExt, LocalDeclarationIndexExt},
    Context, Session,
};
use span::Span;
use std::default::default;
use utilities::{difference, displayed};

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
    let mut component = Component::mock();
    let array = component
        .add("Array", EntityKind::untyped_data_type())
        .into_item();
    let int = component
        .add("Int", EntityKind::untyped_data_type())
        .into_item();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "topmost.Array topmost.Int -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                explicitness: Explicit,
                binder: None,
                domain: Expression::bare(
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
        &session,
    );
}

#[test]
fn pi_type_named_parameter() {
    let mut component = Component::mock();
    let array = component.add("Array", EntityKind::untyped_data_type());
    let int = component.add("Int", EntityKind::untyped_data_type());
    let container = component.add("Container", EntityKind::untyped_data_type());
    let alpha = Identifier::parameter("alpha");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "(alpha: topmost.Array topmost.Int) -> topmost.Container alpha",
        &Expression::bare(
            hir::PiType {
                explicitness: Explicit,
                binder: Some(alpha.clone()),
                domain: Expression::bare(
                    hir::Application {
                        callee: array.into_item(),
                        argument: int.into_item(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                codomain: Expression::bare(
                    hir::Application {
                        callee: container.into_item(),
                        argument: alpha.into_item(),
                        explicitness: Explicit,
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
        .into_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "'(whatever: topmost.Type) -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                explicitness: Implicit,
                binder: Some(Identifier::parameter("whatever")),
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
        .into_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "(topmost.Int -> topmost.Int) -> topmost.Int",
        &Expression::bare(
            hir::PiType {
                explicitness: Explicit,
                binder: None,
                domain: Expression::bare(
                    hir::PiType {
                        explicitness: Explicit,
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
        .into_item();
    let text = component
        .add("Text", EntityKind::untyped_data_type())
        .into_item();
    let type_ = component
        .add("Type", EntityKind::untyped_data_type())
        .into_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "topmost.Int -> topmost.Text -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                explicitness: Explicit,
                binder: None,
                domain: int,
                codomain: Expression::bare(
                    hir::PiType {
                        explicitness: Explicit,
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
        .into_item();
    let x = Identifier::parameter("x");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"(\x => x) -> topmost.Type",
        &Expression::bare(
            hir::PiType {
                explicitness: Explicit,
                binder: None,
                domain: Expression::bare(
                    hir::Lambda {
                        binder: x.clone(),
                        domain: None,
                        codomain: None,
                        body: x.into_item(),
                        explicitness: Explicit,
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
        .into_item();

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
                                callee: Identifier::parameter("alpha").into_item(),
                                argument: beta.into_item(),
                                explicitness: Explicit,
                            }
                            .into(),
                        ),
                        argument: Expression::bare(
                            hir::Application {
                                callee: Identifier::parameter("gamma").into_item(),
                                argument: type_,
                                explicitness: Explicit,
                            }
                            .into(),
                        ),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::bare(Number::Nat(0u8.into()).into()),
                explicitness: Explicit,
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
    let it = Identifier::parameter("it");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    // we might want to format this special case as `topmost.take \it => it` in the future
    assert_format(
        r"topmost.take (\it => it)",
        &Expression::bare(
            hir::Application {
                callee: take.into_item(),
                argument: Expression::bare(
                    hir::Lambda {
                        binder: it.clone(),
                        domain: None,
                        codomain: None,
                        // technically not correct
                        body: it.into_item(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                explicitness: Explicit,
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
    let it = Identifier::parameter("it");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r#"topmost.take (\it => it) "who""#,
        &Expression::bare(
            hir::Application {
                callee: Expression::bare(
                    hir::Application {
                        callee: take.into_item(),
                        argument: Expression::bare(
                            hir::Lambda {
                                binder: it.clone(),
                                domain: None,
                                codomain: None,
                                // technically not correct
                                body: it.into_item(),
                                explicitness: Explicit,
                            }
                            .into(),
                        ),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                argument: Expression::bare(Text::Text("who".into()).into()),
                explicitness: Explicit,
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
        .into_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"topmost.identity 'topmost.Type",
        &Expression::bare(
            hir::Application {
                callee: identity.into_item(),
                argument: type_,
                explicitness: Implicit,
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
                callee: identity.into_item(),
                argument: Expression::bare(
                    hir::Application {
                        callee: Identifier::parameter("prepare").into_item(),
                        argument: text.into_item(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                explicitness: Implicit,
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
                        callee: Identifier::parameter("eta"),
                        arguments: vec![Expression::bare(Number::Nat(10u8.into()).into())],
                    }
                    .into(),
                ),
                argument: Identifier::parameter("omicron").into_item(),
                explicitness: Explicit,
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
        r"\input: topmost.Output => 0",
        &Expression::bare(
            hir::Lambda {
                binder: Identifier::parameter("input"),
                domain: None,
                codomain: Some(output.into_item()),
                body: Expression::bare(Number::Nat(0u8.into()).into()),
                explicitness: Explicit,
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
        .into_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"\(input: topmost.Input): topmost.Output => topmost.Type",
        &Expression::bare(
            hir::Lambda {
                binder: Identifier::parameter("input"),
                domain: Some(input.into_item()),
                codomain: Some(output.into_item()),
                body: type_,
                explicitness: Explicit,
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
        .into_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"\'(Input: topmost.Type) => topmost.Type",
        &Expression::bare(
            hir::Lambda {
                binder: Identifier::parameter("Input"),
                domain: Some(type_.clone()),
                codomain: None,
                body: type_,
                explicitness: Implicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_implicit_unannotated_parameter() {
    let a = Identifier::parameter("a");

    let mut context = Context::mock();
    let session = Session::new(Component::mock(), &mut context);

    assert_format(
        r"\'A => \a => a",
        &Expression::bare(
            hir::Lambda {
                binder: Identifier::parameter("A"),
                domain: None,
                codomain: None,
                body: Expression::bare(
                    hir::Lambda {
                        binder: a.clone(),
                        domain: None,
                        codomain: None,
                        body: a.into_item(),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                explicitness: Implicit,
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
        .into_item();
    let x = Identifier::parameter("x");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"\x => x -> topmost.Type",
        &Expression::bare(
            hir::Lambda {
                binder: x.clone(),
                domain: None,
                codomain: None,
                body: Expression::bare(
                    hir::PiType {
                        explicitness: Explicit,
                        binder: None,
                        domain: x.into_item(),
                        codomain: type_,
                    }
                    .into(),
                ),
                explicitness: Explicit,
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
                callee: add.clone(),
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
                        callee: Identifier::parameter("==").into_item(),
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
                        callee: Identifier::parameter("increment").into_item(),
                        argument: Expression::bare(Number::Nat(1u8.into()).into()),
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                explicitness: Explicit,
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

    assert_format(
        "topmost.overarching.middle.sink",
        &sink.into_item(),
        &session,
    );
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
        &sink.into_item(),
        &session,
    );
}
