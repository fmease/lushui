use crate::Display;
use ast::ParamKind::*;
use hir::{
    Attr, Attrs, BareAttr, Entity, EntityKind, Exposure, Expr, Ident, LocalDeclIdx, NumLit, TextLit,
};
use session::{
    component::{Comp, IdentExt, LocalDeclIdxExt},
    Context, Session,
};
use span::Span;
use utility::{
    default, displayed,
    paint::{epaint, ColorChoice},
    Changeset, ChangesetExt,
};

// @Beacon @Task do something smart if spaces differ (which cannot have color)
// like replacing them with a different character like the Unicode space symbol
// or however it is called ("SP")
fn assert_format(expected: &str, actual: &Expr, sess: &Session<'_>) {
    let actual = displayed(|f| actual.write(sess, f)).to_string();

    if actual != expected {
        // We also lock stdout since the test runner would otherwise interfere.
        let stdout = std::io::stdout().lock();
        epaint(
            |painter| Changeset::new(expected, &actual, "").render_with_ledge(painter),
            ColorChoice::Auto,
        )
        .unwrap();
        drop(stdout);

        panic!("the actual textual representation of the HIR node does not match the expected one");
    }
}

trait ComponentExt {
    fn add(&mut self, name: &str, kind: EntityKind) -> Ident;

    fn add_below(&mut self, name: &str, kind: EntityKind, parent: LocalDeclIdx) -> Ident;
}

impl ComponentExt for Comp {
    fn add(&mut self, name: &str, kind: EntityKind) -> Ident {
        self.add_below(name, kind, self.root_local())
    }

    fn add_below(&mut self, name: &str, kind: EntityKind, parent: LocalDeclIdx) -> Ident {
        let ident = ast::Ident::new_unchecked(default(), name.into());
        let entity = Entity {
            src: ident,
            parent: Some(parent),
            exp: Exposure::Unrestricted,
            attrs: default(),
            kind,
        };
        let index = self.bindings.insert(entity);
        Ident::new(index.global(self), ident)
    }
}

fn param(name: &str) -> Ident {
    Ident::new(
        hir::Index::Param,
        ast::Ident::new_unchecked(default(), name.into()),
    )
}

#[test]
fn pi_ty_app_arg() {
    let mut comp = Comp::mock();
    let array = comp.add("Array", EntityKind::untyped_data_ty()).to_item();
    let int = comp.add("Int", EntityKind::untyped_data_ty()).to_item();
    let ty = comp.add("Type", EntityKind::untyped_data_ty()).to_item();

    let mut context = Context::mock();
    let sess = Session::new(comp, &mut context);

    assert_format(
        "topmost.Array topmost.Int -> topmost.Type",
        &Expr::bare(
            hir::PiTy {
                kind: Explicit,
                binder: None,
                domain: Expr::bare(
                    hir::App {
                        callee: array,
                        arg: int,
                        kind: Explicit,
                    }
                    .into(),
                ),
                codomain: ty,
            }
            .into(),
        ),
        &sess,
    );
}

#[test]
fn pi_type_named_parameter() {
    let mut comp = Comp::mock();
    let array = comp.add("Array", EntityKind::untyped_data_ty());
    let int = comp.add("Int", EntityKind::untyped_data_ty());
    let container = comp.add("Container", EntityKind::untyped_data_ty());
    let alpha = param("alpha");

    let mut cx = Context::mock();
    let sess = Session::new(comp, &mut cx);

    assert_format(
        "For (alpha: topmost.Array topmost.Int) -> topmost.Container alpha",
        &Expr::bare(
            hir::PiTy {
                kind: Explicit,
                binder: Some(alpha),
                domain: Expr::bare(
                    hir::App {
                        callee: array.to_item(),
                        arg: int.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
                codomain: Expr::bare(
                    hir::App {
                        callee: container.to_item(),
                        arg: alpha.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
            }
            .into(),
        ),
        &sess,
    );
}

#[test]
fn pi_type_implicit_parameter() {
    let mut comp = Comp::mock();
    let ty = comp.add("Type", EntityKind::untyped_data_ty()).to_item();

    let mut cx = Context::mock();
    let sess = Session::new(comp, &mut cx);

    assert_format(
        "For '(whatever: topmost.Type) -> topmost.Type",
        &Expr::bare(
            hir::PiTy {
                kind: Implicit,
                binder: Some(param("whatever")),
                domain: ty.clone(),
                codomain: ty,
            }
            .into(),
        ),
        &sess,
    );
}

/// Compare with [`pi_type_two_curried_arguments`].
#[test]
fn pi_type_higher_order_argument() {
    let mut comp = Comp::mock();
    let int = comp.add("Int", EntityKind::untyped_data_ty()).to_item();

    let mut cx = Context::mock();
    let sess = Session::new(comp, &mut cx);

    assert_format(
        "(topmost.Int -> topmost.Int) -> topmost.Int",
        &Expr::bare(
            hir::PiTy {
                kind: Explicit,
                binder: None,
                domain: Expr::bare(
                    hir::PiTy {
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
        &sess,
    );
}

/// Compare with [`pi_type_higher_order_argument`].
#[test]
fn pi_type_two_curried_arguments() {
    let mut component = Comp::mock();
    let int = component
        .add("Int", EntityKind::untyped_data_ty())
        .to_item();
    let text = component
        .add("Text", EntityKind::untyped_data_ty())
        .to_item();
    let ty = component
        .add("Type", EntityKind::untyped_data_ty())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "topmost.Int -> topmost.Text -> topmost.Type",
        &Expr::bare(
            hir::PiTy {
                kind: Explicit,
                binder: None,
                domain: int,
                codomain: Expr::bare(
                    hir::PiTy {
                        kind: Explicit,
                        binder: None,
                        domain: text,
                        codomain: ty,
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
    let mut component = Comp::mock();
    let ty = component
        .add("Type", EntityKind::untyped_data_ty())
        .to_item();
    let x = param("x");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "(for x => x) -> topmost.Type",
        &Expr::bare(
            hir::PiTy {
                kind: Explicit,
                binder: None,
                domain: Expr::bare(
                    hir::LamLit {
                        binder: Some(x),
                        domain: None,
                        codomain: None,
                        body: x.to_item(),
                        kind: Explicit,
                    }
                    .into(),
                ),
                codomain: ty,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn application_three_curried_arguments() {
    let mut component = Comp::mock();
    let beta = component.add("beta", EntityKind::FuncUntyped);
    let ty = component
        .add("Type", EntityKind::untyped_data_ty())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "alpha topmost.beta (gamma topmost.Type) 0",
        &Expr::bare(
            hir::App {
                callee: Expr::bare(
                    hir::App {
                        callee: Expr::bare(
                            hir::App {
                                callee: param("alpha").to_item(),
                                arg: beta.to_item(),
                                kind: Explicit,
                            }
                            .into(),
                        ),
                        arg: Expr::bare(
                            hir::App {
                                callee: param("gamma").to_item(),
                                arg: ty,
                                kind: Explicit,
                            }
                            .into(),
                        ),
                        kind: Explicit,
                    }
                    .into(),
                ),
                arg: Expr::bare(NumLit::Nat(0u8.into()).into()),
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
    let mut component = Comp::mock();
    let take = component.add("take", EntityKind::FuncUntyped);
    let it = param("it");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    // we might want to format this special case as `topmost.take for it => it` in the future
    assert_format(
        "topmost.take (for it => it)",
        &Expr::bare(
            hir::App {
                callee: take.to_item(),
                arg: Expr::bare(
                    hir::LamLit {
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
    let mut component = Comp::mock();
    let take = component.add("take", EntityKind::FuncUntyped);
    let it = param("it");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r#"topmost.take (for it => it) "who""#,
        &Expr::bare(
            hir::App {
                callee: Expr::bare(
                    hir::App {
                        callee: take.to_item(),
                        arg: Expr::bare(
                            hir::LamLit {
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
                arg: Expr::bare(TextLit::Text("who".into()).into()),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn application_implicit_argument() {
    let mut component = Comp::mock();
    let identity = component.add("identity", EntityKind::FuncUntyped);
    let ty = component
        .add("Type", EntityKind::untyped_data_ty())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"topmost.identity 'topmost.Type",
        &Expr::bare(
            hir::App {
                callee: identity.to_item(),
                arg: ty,
                kind: Implicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn application_complex_implicit_argument() {
    let mut component = Comp::mock();
    let identity = component.add("identity", EntityKind::FuncUntyped);
    let text = component.add("Text", EntityKind::untyped_data_ty());

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"topmost.identity '(prepare topmost.Text)",
        &Expr::bare(
            hir::App {
                callee: identity.to_item(),
                arg: Expr::bare(
                    hir::App {
                        callee: param("prepare").to_item(),
                        arg: text.to_item(),
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
    let session = Session::new(Comp::mock(), &mut context);

    assert_format(
        "eta 10 omicron",
        &Expr::bare(
            hir::App {
                callee: Expr::bare(
                    hir::IntrApp {
                        callee: param("eta"),
                        args: vec![Expr::bare(NumLit::Nat(10u8.into()).into())],
                    }
                    .into(),
                ),
                arg: param("omicron").to_item(),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_body_type_annotation() {
    let mut component = Comp::mock();
    let output = component.add("Output", EntityKind::untyped_data_ty());

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "for input: topmost.Output => 0",
        &Expr::bare(
            hir::LamLit {
                binder: Some(param("input")),
                domain: None,
                codomain: Some(output.to_item()),
                body: Expr::bare(NumLit::Nat(0u8.into()).into()),
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_parameter_type_annotation_body_type_annotation() {
    let mut component = Comp::mock();
    let input = component.add("Input", EntityKind::untyped_data_ty());
    let output = component.add("Output", EntityKind::untyped_data_ty());
    let ty = component
        .add("Type", EntityKind::untyped_data_ty())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "for (input: topmost.Input): topmost.Output => topmost.Type",
        &Expr::bare(
            hir::LamLit {
                binder: Some(param("input")),
                domain: Some(input.to_item()),
                codomain: Some(output.to_item()),
                body: ty,
                kind: Explicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_implicit_parameter() {
    let mut component = Comp::mock();
    let ty = component
        .add("Type", EntityKind::untyped_data_ty())
        .to_item();

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "for '(Input: topmost.Type) => topmost.Type",
        &Expr::bare(
            hir::LamLit {
                binder: Some(param("Input")),
                domain: Some(ty.clone()),
                codomain: None,
                body: ty,
                kind: Implicit,
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn lambda_implicit_unannotated_parameter() {
    let a = param("a");

    let mut context = Context::mock();
    let session = Session::new(Comp::mock(), &mut context);

    assert_format(
        "for 'A => for a => a",
        &Expr::bare(
            hir::LamLit {
                binder: Some(param("A")),
                domain: None,
                codomain: None,
                body: Expr::bare(
                    hir::LamLit {
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
    let mut component = Comp::mock();
    let ty = component
        .add("Type", EntityKind::untyped_data_ty())
        .to_item();
    let x = param("x");

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        r"for x => x -> topmost.Type",
        &Expr::bare(
            hir::LamLit {
                binder: Some(x),
                domain: None,
                codomain: None,
                body: Expr::bare(
                    hir::PiTy {
                        kind: Explicit,
                        binder: None,
                        domain: x.to_item(),
                        codomain: ty,
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
    let mut component = Comp::mock();
    let add = component.add("add", EntityKind::FuncUntyped);

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "add",
        &Expr::bare(
            hir::IntrApp {
                callee: add,
                args: Vec::new(),
            }
            .into(),
        ),
        &session,
    );
}

#[test]
fn intrinsic_application_two_arguments() {
    let mut component = Comp::mock();
    let add = component.add("add", EntityKind::FuncUntyped);

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "add (add 1 3000) 0",
        &Expr::bare(
            hir::IntrApp {
                callee: add,
                args: vec![
                    Expr::bare(
                        hir::IntrApp {
                            callee: add,
                            args: vec![
                                Expr::bare(NumLit::Nat(1u8.into()).into()),
                                Expr::bare(NumLit::Nat(3000u16.into()).into()),
                            ],
                        }
                        .into(),
                    ),
                    Expr::bare(NumLit::Nat(0u8.into()).into()),
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
    let session = Session::new(Comp::mock(), &mut context);

    assert_format(
        "== @static @unsafe 3 @static (increment 1)",
        &Expr::bare(
            hir::App {
                callee: Expr::bare(
                    hir::App {
                        callee: param("==").to_item(),
                        arg: Expr::new(
                            Attrs(vec![
                                Attr::new(default(), BareAttr::Static),
                                Attr::new(default(), BareAttr::Unsafe),
                            ]),
                            Span::default(),
                            NumLit::Nat(3u8.into()).into(),
                        ),
                        kind: Explicit,
                    }
                    .into(),
                ),
                arg: Expr::new(
                    Attrs(vec![Attr::new(default(), BareAttr::Static)]),
                    default(),
                    hir::App {
                        callee: param("increment").to_item(),
                        arg: Expr::bare(NumLit::Nat(1u8.into()).into()),
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

#[test]
fn path() {
    let mut component = Comp::mock();
    let overarching = component.add("overarching", EntityKind::module());
    let middle = component.add_below(
        "middle",
        EntityKind::module(),
        overarching.local_decl_idx(&component).unwrap(),
    );
    let sink = component.add_below(
        "sink",
        EntityKind::FuncUntyped,
        middle.local_decl_idx(&component).unwrap(),
    );

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format("topmost.overarching.middle.sink", &sink.to_item(), &session);
}

#[test]
fn path_ident_symbol_symbol_ident_segments() {
    let mut component = Comp::mock();
    let overarching = component.add("overarching", EntityKind::module());
    let noisy = component.add_below(
        "&/.~##",
        EntityKind::module(),
        overarching.local_decl_idx(&component).unwrap(),
    );
    let zickzack = component.add_below(
        "^^^",
        EntityKind::module(),
        noisy.local_decl_idx(&component).unwrap(),
    );
    let sink = component.add_below(
        "sink",
        EntityKind::FuncUntyped,
        zickzack.local_decl_idx(&component).unwrap(),
    );

    let mut context = Context::mock();
    let session = Session::new(component, &mut context);

    assert_format(
        "topmost.overarching.&/.~## . ^^^ .sink",
        &sink.to_item(),
        &session,
    );
}
