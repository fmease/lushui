//! The diagnostic handler.

use std::{cell::RefCell, collections::BTreeSet, default::default, rc::Rc};

use crate::{
    error::Health,
    format::{ordered_listing, pluralize, Conjunction},
    span::SourceMap,
};

use super::{Diagnostic, Level};

/// The diagnostic handler.
pub struct Handler {
    emitter: Emitter,
}

impl Handler {
    pub fn silent() -> Self {
        Self {
            emitter: Emitter::Silent,
        }
    }

    pub fn buffered_stderr(map: Rc<RefCell<SourceMap>>) -> Self {
        Self {
            emitter: Emitter::BufferedStderr {
                errors: default(),
                warnings: default(),
                map,
            },
        }
    }

    // @Task don't emit warnings unconditionally once we support
    // `@allow`ing certain warnings (lint support)
    pub(super) fn emit(&self, diagnostic: Diagnostic) {
        match &self.emitter {
            Emitter::Silent => {}
            Emitter::BufferedStderr {
                errors,
                warnings,
                map,
            } => {
                match diagnostic.0.level {
                    Level::Bug | Level::Error => {
                        errors.borrow_mut().insert(diagnostic);
                    }
                    Level::Warning => {
                        warnings.borrow_mut().insert(diagnostic);
                    }
                    Level::Debug => diagnostic.emit_to_stderr(Some(&map.borrow())),
                };
            }
        }
    }

    pub fn system_health(&self) -> Health {
        match &self.emitter {
            Emitter::BufferedStderr { errors, .. } if !errors.borrow().is_empty() => {
                Health::Tainted
            }
            _ => Health::Untainted,
        }
    }

    pub fn emit_buffered_diagnostics(&self) {
        match &self.emitter {
            Emitter::Silent => {}
            Emitter::BufferedStderr {
                errors,
                warnings,
                map,
            } => {
                {
                    let warnings = warnings.borrow();

                    const MINIMUM_AMOUNT_WARNINGS_FOR_SUMMARY: usize = 1;

                    for warning in &*warnings {
                        warning.emit_to_stderr(Some(&map.borrow()));
                    }

                    if warnings.len() >= MINIMUM_AMOUNT_WARNINGS_FOR_SUMMARY {
                        Diagnostic::warning()
                            .message(format!(
                                "emitted {} {}",
                                warnings.len(),
                                pluralize!(warnings.len(), "warning")
                            ))
                            .emit_to_stderr(Some(&map.borrow()));
                    }
                }

                {
                    let errors = errors.borrow();

                    for error in &*errors {
                        error.emit_to_stderr(Some(&map.borrow()));
                    }

                    if !errors.is_empty() {
                        let codes: BTreeSet<_> =
                            errors.iter().flat_map(|error| error.0.code).collect();

                        Diagnostic::error()
                            .message(pluralize!(
                                errors.len(),
                                "aborting due to previous error",
                                format!("aborting due to {} previous errors", errors.len()),
                            ))
                            // @Note this not actually implemented yet
                            .when(!codes.is_empty(), |this| {
                                this.note(format!(
                                    "the {errors} {codes} {have} a detailed explanation",
                                    errors = pluralize!(codes.len(), "error"),
                                    codes = ordered_listing(codes.iter(), Conjunction::And),
                                    have = pluralize!(codes.len(), "has", "have"),
                                ))
                                .help(pluralize!(
                                    codes.len(),
                                    format!(
                                        "run `lushui explain {}` to view it",
                                        codes.iter().next().unwrap(),
                                    ),
                                    "run `lushui explain <codes...>` to view a selection of them"
                                ))
                            })
                            .emit_to_stderr(Some(&map.borrow()));
                    }
                }
            }
        }
    }
}

enum Emitter {
    Silent,
    BufferedStderr {
        errors: RefCell<BTreeSet<Diagnostic>>,
        warnings: RefCell<BTreeSet<Diagnostic>>,
        map: Rc<RefCell<SourceMap>>,
    },
}
