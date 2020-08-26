//! The documenter — documentation generator.

// @Temporary
#![allow(dead_code, unused_imports)]

use crate::{
    ast::{Declaration, DeclarationKind},
    diagnostic::Diagnostics,
    span::SourceMap,
};
use std::io::Result;
use std::io::Write;
use v_htmlescape::escape;
// use typed_html::{dom::DOMTree, html, text};

// @Note ideally, we wouldf perform some AST verification and desugaring *before* we try to document it
// e.g. verify attributes, deusugar attribute groups, (maybe) disallow top-level bindings w/o type annotation
// currently, the desugarer does all this, we should somehow add desugarer options (either comptime or runtime)
// to this or split the desugarer in more phases (not in favor)
// @Update @Beacon and we won't be able to look into extern modules since they need to be desugared first
// @Task so it's a must to add some options to the Desugarer

pub struct Documenter<'a, W: Write> {
    map: &'a SourceMap,
    warnings: &'a mut Diagnostics,
    output: &'a mut W,
}

impl<'a, W: Write> Documenter<'a, W> {
    pub fn new(output: &'a mut W, map: &'a SourceMap, warnings: &'a mut Diagnostics) -> Self {
        Self {
            map,
            warnings,
            output,
        }
    }

    pub fn document(mut self, declaration: &Declaration) -> Result<()> {
        write!(
            self.output,
            "<!doctype html><html><head><title>lushui crate documentation</title></head><body>"
        )?;
        self.document_declaration(declaration)?;
        write!(self.output, "</body></html>")
    }

    // @Temporary
    fn document_declaration(&mut self, declaration: &Declaration) -> Result<()> {
        use DeclarationKind::*;

        match &declaration.kind {
            Value(value) => {
                write!(
                    self.output,
                    "<code>{}</code><div>",
                    value.binder,
                    // escape(&format!("{:?}", value.parameters))
                )?;
                // @Task instead of iterating here, merge things and parse documentation language (markdown variant)
                for attribute in &*declaration.attributes {
                    write!(
                        self.output,
                        "{}",
                        self.map
                            .resolve_span_to_snippet(attribute.span.trim_start(2))
                    )?;
                }
                write!(self.output, "</div>")?;
            }
            Data(_) => todo!(),
            Constructor(_) => todo!(),
            // @Bug cannot handle extern modules yet (would need desugaring first)
            Module(module) => {
                write!(self.output, "<ul>")?;
                for declaration in module.declarations.as_ref().unwrap() {
                    write!(self.output, "<li>")?;
                    self.document_declaration(declaration)?;
                    write!(self.output, "</li>")?;
                }
                write!(self.output, "</ul>")?;
            }
            Crate(_) => todo!(),
            Header(_) => todo!(),
            Group(_) => todo!(),
            Use(_) => todo!(),
        }

        Ok(())
    }
}
