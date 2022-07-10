//! The LLVM backend.

use crate::{
    component::Component, diagnostics::Diagnostic, error::Result, hir, session::BuildSession,
};
use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

// @Question do we really need to split this off into a sep. module???
mod generator;

/// Compile and link.
pub fn compile(
    options: Options,
    component_root: &hir::Declaration,
    component: &Component,
    session: &BuildSession,
) -> Result<()> {
    if !component.is_goal(session) {
        return Err(Diagnostic::error()
            .message("extern components cannot be built yet with the LLVM backend")
            .report(session.reporter()));
    }

    let context = inkwell::context::Context::create();
    let module = generator::compile(component_root, &context, options, component, session);

    if options.emit_llvm_ir {
        // @Question shouldn't we print to stdout??
        module.print_to_stderr();
    }

    if options.verify_llvm_ir && let Err(message) = module.verify() {
        return Err(Diagnostic::bug()
            .message("the generated LLVM-IR is invalid")
            .note(message.to_string())
            .report(session.reporter()));
    }

    let buffer = module.write_bitcode_to_memory();

    // @Task allow compilation via llc + linking with custom linker e.g. `gcc`, `cc`, `clang`
    // @Task error handling!
    // @Task don't do this with extern components (non-goal components)!
    let mut compiler = Command::new("clang")
        .args(["-x", "ir", "-", "-o"])
        .arg(match session.goal_package() {
            // @Task ensure that the build folder exists
            Some(package) => session.build_folder().join(session[package].name.as_str()),
            None => PathBuf::from(component.name().as_str()),
        })
        .stdin(Stdio::piped())
        // @Tsk smh store stderr for reporting later
        .spawn()
        .unwrap();

    compiler
        .stdin
        .take()
        .unwrap()
        .write_all(buffer.as_slice())
        .unwrap();

    let status = compiler.wait().unwrap();

    if !status.success() {
        // @Question user error or ICE?
        return Err(Diagnostic::error()
            .message("linking failed")
            .report(session.reporter()));
    }

    Ok(())
}

#[derive(Clone, Copy, Default)]
pub struct Options {
    pub emit_llvm_ir: bool,
    pub verify_llvm_ir: bool,
    // @Task
    // pub linker: Option<PathBuf>,
}
