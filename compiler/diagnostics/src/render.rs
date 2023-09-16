//! Diagnostic formatting.

// @Task make absolute paths relative to the current working directory
// @Bug tabs in snippets (and others?) mess up the alignment!
// @Task replace them with N spaces when rendering (don't forget to readjust the rest)!
// @Task dedent long lines of highlighted source code by collapsing long sequences of whitespace
// (or any character?) at the start of a snippet and adding frame-colored ellipses `...` or `…` as a marker
// @Task special-case trailing line breaks in snippets

// @Task :span_path_omission
// * (optional) sort highlights such that those whose file contains primary highlights, they come before
//   those that don't contain any
// * for the first secondary highlight of a file containing primary highlights, show the path of the next primary highlight
// * for any other secondary highlight of a file containing primary highlights, omit the file path
// * for primary highlights, omit the file path if all previous highlights in the same file are secondary ones
// * for any other primary highlight, always show the file path
// * (imprecise) for suggestions, do something very similar to omit file paths if they aren't "necessary"

use super::{
    Role, Severity, Subseverity, Substitution, SubstitutionPart, UnboxedUntaggedDiagnostic,
};
use span::{
    source_map::{LineWithHighlight, LinesWithHighlight},
    FileName, SourceMap,
};
use std::io::{self, Write};
use unicode_width::UnicodeWidthStr;
use utility::paint::{AnsiColor, Effects, Painter};

#[cfg(test)]
mod test;

impl UnboxedUntaggedDiagnostic {
    pub fn render(&self, map: Option<&SourceMap>, painter: &mut Painter) -> io::Result<()> {
        render_header(self, painter)?;

        let (padding, highlights, suggestions) = resolve_spans(self, map);

        let mut renderer = Renderer {
            diagnostic: self,
            padding,
            painter,
        };

        renderer.render_path()?;
        renderer.render_highlights(&highlights)?;

        for subdiagnostic in &self.subdiagnostics {
            renderer.render_subdiagnostic(subdiagnostic.severity, &subdiagnostic.message)?;
        }

        for (index, suggestion) in suggestions.iter().enumerate() {
            renderer.render_suggestion(suggestion)?;

            if index < suggestions.len() - 1 {
                renderer.render_bar()?;
            }
        }

        Ok(())
    }
}

fn render_header(diagnostic: &UnboxedUntaggedDiagnostic, painter: &mut Painter) -> io::Result<()> {
    diagnostic.severity.render(painter)?;

    if let Some(code) = diagnostic.code {
        painter.set(diagnostic.severity.color())?;
        write!(painter, "[{code}]")?;
        painter.unset()?;
    }

    if let Some(message) = &diagnostic.message {
        write!(painter, ": ")?;
        painter.set(Effects::BOLD)?;
        write!(painter, "{message}")?;
        painter.unset()?;
    }

    Ok(())
}

fn resolve_spans<'a>(
    diagnostic: &'a UnboxedUntaggedDiagnostic,
    map: Option<&'a SourceMap>,
) -> (
    String,
    Vec<ResolvedHighlight<'a>>,
    Vec<ResolvedSuggestion<'a>>,
) {
    if diagnostic.highlights.is_empty() && diagnostic.suggestions.is_empty() {
        return (" ".into(), Vec::new(), Vec::new());
    }

    let map = map.expect(
        "missing source map for rendering a \
        diagnostic which references source code",
    );

    let highlights: Vec<_> = diagnostic
        .highlights
        .iter()
        .map(|highlight| ResolvedHighlight {
            lines: map.lines_with_highlight(highlight.span),
            role: highlight.role,
            label: highlight.label.as_deref(),
        })
        .collect();

    let suggestions: Vec<_> = diagnostic
        .suggestions
        .iter()
        .map(|suggestion| ResolvedSuggestion {
            message: &suggestion.message,
            lines: map.lines_with_highlight(suggestion.span),
            substitution: &suggestion.substitution,
        })
        .collect();

    let widest_line_number = highlights
        .iter()
        .map(|highlight| &highlight.lines)
        .chain(suggestions.iter().map(|suggestion| &suggestion.lines))
        .map(|lines| match &lines.last {
            Some(line) => lines.first.number.max(line.number),
            None => lines.first.number,
        })
        .max()
        .unwrap();

    let number_of_digits = widest_line_number.ilog10() + 1;
    let padding = " ".repeat(number_of_digits as _);

    (padding, highlights, suggestions)
}

struct Renderer<'a> {
    diagnostic: &'a UnboxedUntaggedDiagnostic,
    padding: String,
    painter: &'a mut Painter,
}

impl Renderer<'_> {
    const BAR: &'static str = Line::Vertical.single();

    fn render_path(&mut self) -> io::Result<()> {
        let Some(path) = &self.diagnostic.path else {
            return Ok(());
        };

        let needs_downward_connection =
            !self.diagnostic.highlights.is_empty() || !self.diagnostic.subdiagnostics.is_empty();

        let connector = if needs_downward_connection {
            Line::DownAndRight
        } else {
            Line::Horizontal
        }
        .single();

        writeln!(self.painter)?;
        self.painter.set(palette::FRAME)?;
        write!(
            self.painter,
            "{} {}{} ",
            self.padding,
            connector,
            Line::Horizontal.single(),
        )?;
        self.painter
            .write_all(path.as_os_str().as_encoded_bytes())?;
        self.painter.unset()?;

        if needs_downward_connection {
            self.render_bar()?;
        }

        Ok(())
    }

    fn render_highlights(&mut self, highlights: &[ResolvedHighlight<'_>]) -> io::Result<()> {
        let mut needs_upward_connection = self.diagnostic.path.is_some();
        let needs_downward_connection =
            !self.diagnostic.subdiagnostics.is_empty() || !self.diagnostic.suggestions.is_empty();

        for (index, highlight) in highlights.iter().enumerate() {
            // @Task omit in certain cases :span_path_omission
            self.render_location(&highlight.lines, needs_upward_connection)?;

            match &highlight.lines.last {
                None => self.render_single_line_highlight(
                    &highlight.lines.first,
                    highlight.role,
                    highlight.label,
                ),
                Some(final_line) => self.render_multi_line_highlight(
                    &highlight.lines.first,
                    final_line,
                    highlight.role,
                    highlight.label,
                ),
            }?;

            if needs_downward_connection || index < highlights.len() - 1 {
                self.render_bar()?;
            }

            if !needs_upward_connection {
                needs_upward_connection = true;
            }
        }

        Ok(())
    }

    fn render_location(
        &mut self,
        lines: &LinesWithHighlight<'_>,
        needs_upward_connection: bool,
    ) -> io::Result<()> {
        let line = lines.first.number;
        let column = lines.first.highlight.start;

        let connector = if needs_upward_connection {
            Line::VerticalAndRight
        } else {
            Line::DownAndRight
        }
        .single();

        writeln!(self.painter)?;
        self.painter.set(palette::FRAME)?;
        write!(
            self.painter,
            "{} {connector}{} ",
            self.padding,
            Line::Horizontal.single()
        )?;
        // FIXME: Ensue that the painter.reset inside doesn't reset FRAME (I bet it does tho, `Painter` needs to support nesting)
        render_file_name(lines.file, self.painter)?;
        write!(self.painter, ":{line}:{column}")?;
        self.painter.unset()
    }

    fn render_single_line_highlight(
        &mut self,
        line: &LineWithHighlight<'_>,
        role: Role,
        label: Option<&str>,
    ) -> io::Result<()> {
        let color = role.color(self.diagnostic.severity.color());

        self.render_bar()?;
        writeln!(self.painter)?;
        self.render_line_number(line.number)?;
        write!(self.painter, " ")?;

        let highlight_prefix_width = line.highlight.prefix_width;
        let zero_length_highlight = line.highlight.width == 0;

        if zero_length_highlight && highlight_prefix_width == 0 {
            write!(self.painter, " ")?;
        }

        writeln!(self.painter, "{}", line.content)?;

        // The underline and the label.
        {
            self.painter.set(palette::FRAME)?;
            write!(self.painter, "{} {}", self.padding, Self::BAR)?;
            self.painter.unset()?;

            let underline_padding = " ".repeat(match zero_length_highlight {
                true => highlight_prefix_width.saturating_sub(1),
                false => highlight_prefix_width,
            });
            self.painter.set(color)?;
            write!(self.painter, " {underline_padding}")?;
            if !zero_length_highlight {
                write!(
                    self.painter,
                    "{}",
                    Line::Horizontal.to_str(role).repeat(line.highlight.width)
                )
            } else {
                write!(
                    self.painter,
                    "{}{}",
                    Line::RightAngleBracket.to_str(role),
                    Line::LeftAngleBracket.to_str(role),
                )
            }?;
            self.painter.unset()?;

            let mut lines_of_label = label.iter().flat_map(|label| label.split('\n'));

            if let Some(line_of_label) = lines_of_label.next() {
                self.painter.set(color)?;
                write!(self.painter, " {line_of_label}")?;
                self.painter.unset()?;
            }

            let spacing = " ".repeat(
                line.highlight.prefix_width
                    + if zero_length_highlight {
                        1
                    } else {
                        line.highlight.width
                    },
            );

            for line_of_label in lines_of_label {
                self.render_bar()?;

                if !line_of_label.is_empty() {
                    self.painter.set(color)?;
                    write!(self.painter, " {spacing} {line_of_label}")?;
                    self.painter.unset()?;
                }
            }
        }

        Ok(())
    }

    fn render_multi_line_highlight(
        &mut self,
        first_line: &LineWithHighlight<'_>,
        final_line: &LineWithHighlight<'_>,
        role: Role,
        label: Option<&str>,
    ) -> io::Result<()> {
        let color = role.color(self.diagnostic.severity.color());

        let hand = Line::UpAndLeft.to_str(role);

        // The upper arm.
        {
            self.render_bar()?;
            writeln!(self.painter)?;
            self.render_line_number(first_line.number)?;

            writeln!(self.painter, "   {}", first_line.content)?;

            write!(self.painter, "{} ", self.padding)?;

            {
                // If the first and the final line are futher apart than one,
                // render a (stylized) ellipsis instead of a bar.
                let bar = if final_line.number - first_line.number > 1 {
                    ELLIPSIS
                } else {
                    Self::BAR
                };

                self.painter.set(palette::FRAME)?;

                write!(self.painter, "{bar} ")?;
                self.painter.unset()?;
            }

            // The size of the hand is fixed and does not depend on the Unicode width of
            // the first character of the highlight. This should be fine.
            let joint = Line::DownAndRight.to_str(role);
            let horizontal_arm = Line::Horizontal
                .to_str(role)
                .repeat(first_line.highlight.prefix_width + 1);
            self.painter.set(color)?;
            writeln!(self.painter, "{joint}{horizontal_arm}{hand}")?;
            self.painter.unset()?;
        }

        // The connector and the lower arm.
        {
            self.render_line_number(final_line.number)?;
            self.painter.set(color)?;
            let vertical_arm = Line::Vertical.to_str(role);
            writeln!(self.painter, " {vertical_arm} {}", final_line.content)?;
            self.painter.unset()?;

            // The lower arm and the label.
            {
                self.painter.set(palette::FRAME)?;
                write!(self.painter, "{} {}", self.padding, Self::BAR)?;
                self.painter.unset()?;

                let joint = Line::UpAndRight.to_str(role);
                // FIXME: The length of the arm does not depend on the Unicode width of
                //        the last character of the highlight.
                let horizontal_arm = Line::Horizontal
                    .to_str(role)
                    .repeat(final_line.highlight.width);
                self.painter.set(color)?;
                write!(self.painter, " {joint}{horizontal_arm}{hand}")?;
                self.painter.unset()?;

                let mut lines_of_label = label.iter().flat_map(|label| label.split('\n'));

                if let Some(line_of_label) = lines_of_label.next() {
                    if !line_of_label.is_empty() {
                        self.painter.set(color)?;
                        write!(self.painter, " {line_of_label}")?;
                        self.painter.unset()?;
                    }
                }

                let spacing = " ".repeat(1 + final_line.highlight.width + 1);

                for line_of_label in lines_of_label {
                    self.render_bar()?;

                    if !line_of_label.is_empty() {
                        self.painter.set(color)?;
                        write!(self.painter, " {spacing} {line_of_label}")?;
                        self.painter.unset()?;
                    }
                }
            }
        }

        Ok(())
    }

    fn render_subdiagnostic(&mut self, severity: Subseverity, message: &str) -> io::Result<()> {
        writeln!(self.painter)?;
        write!(self.painter, "{}", self.padding)?;
        severity.render(self.painter)?;
        write!(self.painter, ": ")?;

        let mut lines = message.split('\n');

        if let Some(line) = lines.next() {
            write!(self.painter, "{line}")?;
        }

        let severity_spacing = " ".repeat(severity.name().width() + 1);

        for line in lines {
            if !line.is_empty() {
                writeln!(self.painter)?;
                write!(self.painter, "{}{severity_spacing} {line}", self.padding)?;
            }
        }

        Ok(())
    }

    // @Task support multi-line substitutions
    // FIXME: If possible extract the common parts of `render_{single_line_highlight,suggestion}`.
    // @Note this is an MVP
    // @Task write unit tests for this!
    fn render_suggestion(&mut self, suggestion: &ResolvedSuggestion<'_>) -> io::Result<()> {
        self.render_subdiagnostic(Subseverity::Help, suggestion.message)?;
        // FIXME: Omit this in certain cases.
        self.render_location(&suggestion.lines, false)?;

        self.render_bar()?;
        writeln!(self.painter)?;
        let line = &suggestion.lines.first;
        self.render_line_number(line.number)?;

        let substitution_width = suggestion
            .substitution
            .parts
            .iter()
            .map(|part| match part {
                SubstitutionPart::Str(value) => value.width(),
                SubstitutionPart::Placeholder(name) => name.width(),
            })
            .sum();
        let zero_length_highlight = substitution_width == 0;
        let highlight_prefix_width = line.highlight.prefix_width;

        if zero_length_highlight && highlight_prefix_width == 0 {
            write!(self.painter, " ")?;
        }

        // FIXME: Ensure that this is Unicode aware.
        let before = &line.content[..line.highlight.start as usize - 1];
        write!(self.painter, " {before}")?;

        self.painter.set(palette::SUBSTITUTION)?;
        for part in &suggestion.substitution.parts {
            match part {
                SubstitutionPart::Str(value) => write!(self.painter, "{value}")?,
                SubstitutionPart::Placeholder(name) => {
                    self.painter.set(Effects::ITALIC)?;
                    write!(self.painter, "{name}")?;
                    self.painter.unset()?;
                }
            }
        }
        self.painter.unset()?;

        // FIXME: Ensure that this is Unicode aware.
        let after = &line.content[line.highlight.end as usize - 1..];
        writeln!(self.painter, "{after}")?;

        self.painter.set(palette::FRAME)?;
        write!(self.painter, "{} {}", self.padding, Self::BAR)?;
        self.painter.unset()?;

        let underline_padding = " ".repeat(match zero_length_highlight {
            true => highlight_prefix_width.saturating_sub(1),
            false => highlight_prefix_width,
        });
        write!(self.painter, " {underline_padding}")?;

        // The underline or the cursor in case of a zero-length highlight.
        self.painter.set(palette::SUBSTITUTION)?;
        if !zero_length_highlight {
            write!(self.painter, "{}", SUBSTITUTION.repeat(substitution_width))
        } else {
            write!(
                self.painter,
                "{}{}",
                Line::RightAngleBracket.single(),
                Line::LeftAngleBracket.single(),
            )
        }?;
        self.painter.unset()
    }

    fn render_bar(&mut self) -> io::Result<()> {
        writeln!(self.painter)?;

        self.painter.set(palette::FRAME)?;
        write!(self.painter, "{} {}", self.padding, Self::BAR)?;
        self.painter.unset()
    }

    fn render_line_number(&mut self, number: u32) -> io::Result<()> {
        let padding = self.padding.len();

        self.painter.set(palette::FRAME)?;
        write!(self.painter, "{number:>padding$} {}", Self::BAR)?;
        self.painter.unset()
    }
}

struct ResolvedHighlight<'a> {
    lines: LinesWithHighlight<'a>,
    role: Role,
    label: Option<&'a str>,
}

struct ResolvedSuggestion<'a> {
    message: &'a str,
    lines: LinesWithHighlight<'a>,
    substitution: &'a Substitution,
}

impl Severity {
    const fn name(self) -> &'static str {
        match self {
            Self::Bug => "internal compiler error",
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Debug => "internal debugging message",
        }
    }

    pub(super) const fn color(self) -> AnsiColor {
        match self {
            Self::Bug | Self::Error => palette::ERROR,
            Self::Warning => palette::WARNING,
            Self::Debug => palette::DEBUG,
        }
    }
}

impl Severity {
    fn render(self, painter: &mut Painter) -> io::Result<()> {
        painter.set(self.color().on_default().bold())?;
        write!(painter, "{}", self.name())?;
        painter.unset()
    }
}

impl Subseverity {
    const COLOR: AnsiColor = palette::HELP;

    fn render(self, painter: &mut Painter) -> io::Result<()> {
        painter.set(Self::COLOR.on_default().bold())?;
        write!(painter, "{}", self.name())?;
        painter.unset()
    }
}

impl Role {
    const fn color(self, primary: AnsiColor) -> AnsiColor {
        match self {
            Self::Primary => primary,
            Self::Secondary => palette::HELP,
        }
    }
}

#[derive(Clone, Copy)]
enum Line {
    Horizontal,
    Vertical,
    DownAndRight,
    VerticalAndRight,
    UpAndLeft,
    UpAndRight,
    RightAngleBracket,
    LeftAngleBracket,
}

impl Line {
    const fn single(self) -> &'static str {
        match self {
            Self::Horizontal => "─",
            Self::Vertical => "│",
            Self::DownAndRight => "┌",
            Self::VerticalAndRight => "├",
            Self::UpAndLeft => "┘",
            Self::UpAndRight => "└",
            Self::LeftAngleBracket => "⟨",
            Self::RightAngleBracket => "⟩",
        }
    }

    const fn double(self) -> &'static str {
        match self {
            Self::Horizontal => "═",
            Self::Vertical => "║",
            Self::DownAndRight => "╔",
            Self::VerticalAndRight => "╠",
            Self::UpAndLeft => "╝",
            Self::UpAndRight => "╚",
            Self::LeftAngleBracket => "⟪",
            Self::RightAngleBracket => "⟫",
        }
    }

    const fn to_str(self, role: Role) -> &'static str {
        match role {
            Role::Primary => self.double(),
            Role::Secondary => self.single(),
        }
    }
}

fn render_file_name(name: &FileName, painter: &mut Painter) -> io::Result<()> {
    match name {
        FileName::Anonymous => {
            painter.set(Effects::ITALIC)?;
            write!(painter, "⟨anonymous⟩")?;
            painter.unset()
        }
        FileName::Stdin => {
            painter.set(Effects::ITALIC)?;
            write!(painter, "⟨stdin⟩")?;
            painter.unset()
        }
        FileName::Path(path) => painter.write_all(path.as_os_str().as_encoded_bytes()),
        FileName::Virtual(name) => write!(painter, "{name}"),
    }
}

const ELLIPSIS: &str = "·";
const SUBSTITUTION: &str = "~";

mod palette {
    use utility::paint::AnsiColor;

    pub(super) const FRAME: AnsiColor = AnsiColor::BrightBlue;
    pub(super) const ERROR: AnsiColor = AnsiColor::BrightRed;
    pub(super) const WARNING: AnsiColor = AnsiColor::BrightYellow;
    pub(super) const HELP: AnsiColor = AnsiColor::BrightCyan;
    pub(super) const DEBUG: AnsiColor = AnsiColor::BrightMagenta;
    pub(super) const SUBSTITUTION: AnsiColor = AnsiColor::Green;
}
