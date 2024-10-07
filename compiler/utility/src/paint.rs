// FIXME: This could use a bit more documentation.

use crate::SmallVec;
use derivation::{Elements, FromStr, Str};
use std::{
    io::{self, BufWriter, StderrLock, StdoutLock, Write},
    string::FromUtf8Error,
};
use supports_color::Stream;

pub use anstyle::{Ansi256Color, AnsiColor, Color, Effects, RgbColor, Style};

/// Paint to a `String`.
///
/// Convenience function that calls [`Painter::bytes`] under the hood.
pub fn paint_to_string(
    paint: impl FnOnce(&mut Painter) -> io::Result<()>,
    choice: ColorChoice,
) -> Result<String, FromUtf8Error> {
    let mut painter = Painter::bytes(choice);
    paint(&mut painter).unwrap(); // Writing to bytes should never fail.
    String::from_utf8(painter.buffer())
}

/// Paint to locked and buffered stdout.
///
/// Convenience function that calls [`Painter::stdout`] under the hood.
pub fn paint(
    paint: impl FnOnce(&mut Painter) -> io::Result<()>,
    choice: ColorChoice,
) -> io::Result<()> {
    let mut painter = Painter::stdout(choice);
    paint(&mut painter)?;
    painter.flush()
}

/// Paint to locked and buffered stderr.
///
/// Convenience function that calls [`Painter::stderr`] under the hood.
pub fn epaint(
    paint: impl FnOnce(&mut Painter) -> io::Result<()>,
    choice: ColorChoice,
) -> io::Result<()> {
    let mut painter = Painter::stderr(choice);
    paint(&mut painter)?;
    painter.flush()
}

pub struct Painter {
    // Instead of making the writer generic via `dyn io::Write`, we use an enum of
    // writers common to this workspace to avoid dynamic dispatch. Adding a type
    // parameter to the painter is not an option since renderers using it typically
    // contain a lot of code which would lead to the binary size exploding due to
    // monomorphization.
    writer: Writer,
    colorize: bool,
    stack: SmallVec<Style, 3>,
}

impl Painter {
    pub fn bytes(choice: ColorChoice) -> Self {
        let colorize = choice.resolve(None);
        let writer = Writer::Bytes(Vec::new());

        Self::new(writer, colorize)
    }

    pub fn stdout(choice: ColorChoice) -> Self {
        let colorize = choice.resolve(Some(Stream::Stdout));
        let writer = Writer::Stdout(BufWriter::new(std::io::stdout().lock()));

        Self::new(writer, colorize)
    }

    pub fn stderr(choice: ColorChoice) -> Self {
        let colorize = choice.resolve(Some(Stream::Stderr));
        let writer = Writer::Stderr(BufWriter::new(std::io::stderr().lock()));

        Self::new(writer, colorize)
    }

    fn new(writer: Writer, colorize: bool) -> Self {
        Self { writer, colorize, stack: SmallVec::new() }
    }

    pub fn set(&mut self, style: impl IntoStyle) -> io::Result<()> {
        if !self.colorize {
            return Ok(());
        }

        let style = style.into_style();
        self.stack.push(style);
        style.write_to(&mut self.writer)
    }

    pub fn unset(&mut self) -> io::Result<()> {
        if !self.colorize {
            return Ok(());
        }

        if let Some(style) = self.stack.pop() {
            style.write_reset_to(&mut self.writer)?;
        }

        for style in &self.stack {
            style.write_to(&mut self.writer)?;
        }

        Ok(())
    }

    pub fn buffer(self) -> Vec<u8> {
        match self.writer {
            Writer::Bytes(bytes) => bytes,
            Writer::Stdout(_) | Writer::Stderr(_) => Vec::new(),
        }
    }
}

impl Write for Painter {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        self.writer.write(buffer)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

enum Writer {
    Stdout(BufWriter<StdoutLock<'static>>),
    Stderr(BufWriter<StderrLock<'static>>),
    Bytes(Vec<u8>),
}

impl Write for Writer {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        match self {
            Self::Stdout(stdout) => stdout.write(buffer),
            Self::Stderr(stderr) => stderr.write(buffer),
            Self::Bytes(bytes) => bytes.write(buffer),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Self::Stdout(stdout) => stdout.flush(),
            Self::Stderr(stderr) => stderr.flush(),
            Self::Bytes(bytes) => bytes.flush(),
        }
    }
}

#[derive(Default, Clone, Copy, FromStr, Str, Elements)]
#[format(dash_case)]
pub enum ColorChoice {
    #[default]
    Auto,
    Never,
    Always,
}

impl ColorChoice {
    fn resolve(self, stream: Option<Stream>) -> bool /*colorize*/ {
        match (self, stream) {
            (Self::Auto, Some(stream)) => {
                supports_color::on_cached(stream).is_some_and(|level| level.has_basic)
            }
            (Self::Never, _) | (Self::Auto, None) => false,
            (Self::Always, _) => true,
        }
    }
}

pub trait IntoStyle {
    fn into_style(self) -> Style;
}

impl IntoStyle for Style {
    fn into_style(self) -> Style {
        self
    }
}

impl IntoStyle for AnsiColor {
    fn into_style(self) -> Style {
        self.on_default()
    }
}

impl IntoStyle for Effects {
    fn into_style(self) -> Style {
        Style::new().effects(self)
    }
}

pub trait ColorExt {
    fn to_bg(self) -> Style;
}

impl ColorExt for AnsiColor {
    fn to_bg(self) -> Style {
        Style::new().bg_color(Some(self.into()))
    }
}
