//! The code responsible for rendering diagnostics.

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

use super::{Role, Severity, Subseverity, Substitution, SubstitutionPart, UnboxedUntaggedDiag};
use span::{
    source_map::{LineWithHighlight, LinesWithHighlight},
    FileName, SourceMap,
};
use std::io::{self, Write};
use unicode_width::UnicodeWidthStr;
use utility::paint::{AnsiColor, Effects, Painter};

#[cfg(test)]
mod test;

impl UnboxedUntaggedDiag {
    pub fn render(&self, map: Option<&SourceMap>, p: &mut Painter) -> io::Result<()> {
        render_header(self, p)?;

        let (padding, highlights, suggestions) = resolve_spans(self, map);

        let mut renderer = Renderer {
            diag: self,
            padding,
            p,
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

fn render_header(diag: &UnboxedUntaggedDiag, p: &mut Painter) -> io::Result<()> {
    diag.severity.render(p)?;

    if let Some(code) = diag.code {
        p.set(diag.severity.color())?;
        write!(p, "[{code}]")?;
        p.unset()?;
    }

    if let Some(message) = &diag.message {
        write!(p, ": ")?;
        p.set(Effects::BOLD)?;
        write!(p, "{message}")?;
        p.unset()?;
    }

    Ok(())
}

fn resolve_spans<'a>(
    diag: &'a UnboxedUntaggedDiag,
    map: Option<&'a SourceMap>,
) -> (
    String,
    Vec<ResolvedHighlight<'a>>,
    Vec<ResolvedSuggestion<'a>>,
) {
    if diag.highlights.is_empty() && diag.suggestions.is_empty() {
        return (" ".into(), Vec::new(), Vec::new());
    }

    let map = map.expect(
        "missing source map for rendering a \
        diagnostic which references source code",
    );

    let highlights: Vec<_> = diag
        .highlights
        .iter()
        .map(|highlight| ResolvedHighlight {
            lines: map.lines_with_highlight(highlight.span),
            role: highlight.role,
            label: highlight.label.as_deref(),
        })
        .collect();

    let suggestions: Vec<_> = diag
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
    diag: &'a UnboxedUntaggedDiag,
    padding: String,
    p: &'a mut Painter,
}

impl Renderer<'_> {
    const BAR: &'static str = Line::Vertical.single();

    fn render_path(&mut self) -> io::Result<()> {
        let Some(path) = &self.diag.path else {
            return Ok(());
        };

        let needs_downward_connection =
            !self.diag.highlights.is_empty() || !self.diag.subdiagnostics.is_empty();

        let connector = if needs_downward_connection {
            Line::DownAndRight
        } else {
            Line::Horizontal
        }
        .single();

        writeln!(self.p)?;
        self.p.set(palette::FRAME)?;
        write!(
            self.p,
            "{} {}{} ",
            self.padding,
            connector,
            Line::Horizontal.single(),
        )?;
        self.p.write_all(path.as_os_str().as_encoded_bytes())?;
        self.p.unset()?;

        if needs_downward_connection {
            self.render_bar()?;
        }

        Ok(())
    }

    fn render_highlights(&mut self, highlights: &[ResolvedHighlight<'_>]) -> io::Result<()> {
        let mut needs_upward_connection = self.diag.path.is_some();
        let needs_downward_connection =
            !self.diag.subdiagnostics.is_empty() || !self.diag.suggestions.is_empty();

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

        writeln!(self.p)?;
        self.p.set(palette::FRAME)?;
        write!(
            self.p,
            "{} {connector}{} ",
            self.padding,
            Line::Horizontal.single()
        )?;
        // FIXME: Ensue that the painter.reset inside doesn't reset FRAME (I bet it does tho, `Painter` needs to support nesting)
        render_file_name(lines.file, self.p)?;
        write!(self.p, ":{line}:{column}")?;
        self.p.unset()
    }

    fn render_single_line_highlight(
        &mut self,
        line: &LineWithHighlight<'_>,
        role: Role,
        label: Option<&str>,
    ) -> io::Result<()> {
        let color = role.color(self.diag.severity.color());

        self.render_bar()?;
        writeln!(self.p)?;
        self.render_line_number(line.number)?;
        write!(self.p, " ")?;

        let highlight_prefix_width = line.highlight.prefix_width;
        let zero_length_highlight = line.highlight.width == 0;

        if zero_length_highlight && highlight_prefix_width == 0 {
            write!(self.p, " ")?;
        }

        writeln!(self.p, "{}", line.content)?;

        // The underline and the label.
        {
            self.p.set(palette::FRAME)?;
            write!(self.p, "{} {}", self.padding, Self::BAR)?;
            self.p.unset()?;

            let underline_padding = " ".repeat(match zero_length_highlight {
                true => highlight_prefix_width.saturating_sub(1),
                false => highlight_prefix_width,
            });
            self.p.set(color)?;
            write!(self.p, " {underline_padding}")?;
            if !zero_length_highlight {
                write!(
                    self.p,
                    "{}",
                    Line::Horizontal.to_str(role).repeat(line.highlight.width)
                )
            } else {
                write!(
                    self.p,
                    "{}{}",
                    Line::RightAngleBracket.to_str(role),
                    Line::LeftAngleBracket.to_str(role),
                )
            }?;
            self.p.unset()?;

            let mut lines_of_label = label.iter().flat_map(|label| label.split('\n'));

            if let Some(line_of_label) = lines_of_label.next() {
                self.p.set(color)?;
                write!(self.p, " {line_of_label}")?;
                self.p.unset()?;
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
                    self.p.set(color)?;
                    write!(self.p, " {spacing} {line_of_label}")?;
                    self.p.unset()?;
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
        let color = role.color(self.diag.severity.color());

        let hand = Line::UpAndLeft.to_str(role);

        // The upper arm.
        {
            self.render_bar()?;
            writeln!(self.p)?;
            self.render_line_number(first_line.number)?;

            writeln!(self.p, "   {}", first_line.content)?;

            write!(self.p, "{} ", self.padding)?;

            {
                // If the first and the final line are futher apart than one,
                // render a (stylized) ellipsis instead of a bar.
                let bar = if final_line.number - first_line.number > 1 {
                    ELLIPSIS
                } else {
                    Self::BAR
                };

                self.p.set(palette::FRAME)?;

                write!(self.p, "{bar} ")?;
                self.p.unset()?;
            }

            // The size of the hand is fixed and does not depend on the Unicode width of
            // the first character of the highlight. This should be fine.
            let joint = Line::DownAndRight.to_str(role);
            let horizontal_arm = Line::Horizontal
                .to_str(role)
                .repeat(first_line.highlight.prefix_width + 1);
            self.p.set(color)?;
            writeln!(self.p, "{joint}{horizontal_arm}{hand}")?;
            self.p.unset()?;
        }

        // The connector and the lower arm.
        {
            self.render_line_number(final_line.number)?;
            self.p.set(color)?;
            let vertical_arm = Line::Vertical.to_str(role);
            writeln!(self.p, " {vertical_arm} {}", final_line.content)?;
            self.p.unset()?;

            // The lower arm and the label.
            {
                self.p.set(palette::FRAME)?;
                write!(self.p, "{} {}", self.padding, Self::BAR)?;
                self.p.unset()?;

                let joint = Line::UpAndRight.to_str(role);
                // FIXME: The length of the arm does not depend on the Unicode width of
                //        the last character of the highlight.
                let horizontal_arm = Line::Horizontal
                    .to_str(role)
                    .repeat(final_line.highlight.width);
                self.p.set(color)?;
                write!(self.p, " {joint}{horizontal_arm}{hand}")?;
                self.p.unset()?;

                let mut lines_of_label = label.iter().flat_map(|label| label.split('\n'));

                if let Some(line_of_label) = lines_of_label.next() {
                    if !line_of_label.is_empty() {
                        self.p.set(color)?;
                        write!(self.p, " {line_of_label}")?;
                        self.p.unset()?;
                    }
                }

                let spacing = " ".repeat(1 + final_line.highlight.width + 1);

                for line_of_label in lines_of_label {
                    self.render_bar()?;

                    if !line_of_label.is_empty() {
                        self.p.set(color)?;
                        write!(self.p, " {spacing} {line_of_label}")?;
                        self.p.unset()?;
                    }
                }
            }
        }

        Ok(())
    }

    fn render_subdiagnostic(&mut self, severity: Subseverity, message: &str) -> io::Result<()> {
        writeln!(self.p)?;
        write!(self.p, "{}", self.padding)?;
        severity.render(self.p)?;
        write!(self.p, ": ")?;

        let mut lines = message.split('\n');

        if let Some(line) = lines.next() {
            write!(self.p, "{line}")?;
        }

        let severity_spacing = " ".repeat(severity.name().width() + 1);

        for line in lines {
            if !line.is_empty() {
                writeln!(self.p)?;
                write!(self.p, "{}{severity_spacing} {line}", self.padding)?;
            }
        }

        Ok(())
    }

    // @Task support multi-line substitutions
    // FIXME: If possible extract the common parts of `render_{single_line_highlight,suggestion}`.
    // @Note this is an MVP
    // @Task write unit tests for this!
    fn render_suggestion(&mut self, sugg: &ResolvedSuggestion<'_>) -> io::Result<()> {
        self.render_subdiagnostic(Subseverity::Help, sugg.message)?;
        // FIXME: Omit this in certain cases.
        self.render_location(&sugg.lines, false)?;

        self.render_bar()?;
        writeln!(self.p)?;
        let line = &sugg.lines.first;
        self.render_line_number(line.number)?;

        let substitution_width = sugg
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
            write!(self.p, " ")?;
        }

        // FIXME: Ensure that this is Unicode aware.
        let before = &line.content[..line.highlight.start as usize - 1];
        write!(self.p, " {before}")?;

        self.p.set(palette::SUBSTITUTION)?;
        for part in &sugg.substitution.parts {
            match part {
                SubstitutionPart::Str(value) => write!(self.p, "{value}")?,
                SubstitutionPart::Placeholder(name) => {
                    self.p.set(Effects::ITALIC)?;
                    write!(self.p, "{name}")?;
                    self.p.unset()?;
                }
            }
        }
        self.p.unset()?;

        // FIXME: Ensure that this is Unicode aware.
        let after = &line.content[line.highlight.end as usize - 1..];
        writeln!(self.p, "{after}")?;

        self.p.set(palette::FRAME)?;
        write!(self.p, "{} {}", self.padding, Self::BAR)?;
        self.p.unset()?;

        let underline_padding = " ".repeat(match zero_length_highlight {
            true => highlight_prefix_width.saturating_sub(1),
            false => highlight_prefix_width,
        });
        write!(self.p, " {underline_padding}")?;

        // The underline or the cursor in case of a zero-length highlight.
        self.p.set(palette::SUBSTITUTION)?;
        if !zero_length_highlight {
            write!(self.p, "{}", SUBSTITUTION.repeat(substitution_width))
        } else {
            write!(
                self.p,
                "{}{}",
                Line::RightAngleBracket.single(),
                Line::LeftAngleBracket.single(),
            )
        }?;
        self.p.unset()
    }

    fn render_bar(&mut self) -> io::Result<()> {
        writeln!(self.p)?;

        self.p.set(palette::FRAME)?;
        write!(self.p, "{} {}", self.padding, Self::BAR)?;
        self.p.unset()
    }

    fn render_line_number(&mut self, number: u32) -> io::Result<()> {
        let padding = self.padding.len();

        self.p.set(palette::FRAME)?;
        write!(self.p, "{number:>padding$} {}", Self::BAR)?;
        self.p.unset()
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
    fn render(self, p: &mut Painter) -> io::Result<()> {
        p.set(self.color().on_default().bold())?;
        write!(p, "{}", self.name())?;
        p.unset()
    }
}

impl Subseverity {
    const COLOR: AnsiColor = palette::HELP;

    fn render(self, p: &mut Painter) -> io::Result<()> {
        p.set(Self::COLOR.on_default().bold())?;
        write!(p, "{}", self.name())?;
        p.unset()
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

fn render_file_name(name: &FileName, p: &mut Painter) -> io::Result<()> {
    match name {
        FileName::Anon => {
            p.set(Effects::ITALIC)?;
            write!(p, "⟨anonymous⟩")?;
            p.unset()
        }
        FileName::Stdin => {
            p.set(Effects::ITALIC)?;
            write!(p, "⟨stdin⟩")?;
            p.unset()
        }
        FileName::Path(path) => p.write_all(path.as_os_str().as_encoded_bytes()),
        FileName::Virtual(name) => write!(p, "{name}"),
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
