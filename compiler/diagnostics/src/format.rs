//! Diagnostic formatting.

// @Note Diagnostics with primary and secondary spans are not *that* readable because of
// all those lengthy paths. That didn't use to be the case.
// @Task make absolute paths relative to the current working directory
// @Task if there is only a single primary highlight, hide the paths for all secondary
// highlights that coincide with the one of the primary one
// @Note tabs in snippets (and others?) mess up the alignment!
// @Task replace them with N spaces when rendering (don't forget to readjust the rest)!
// @Task dedent long lines of highlighted source code by collapsing long sequences of whitespace
// at the start of a snippet and adding frame-colored ellipses `...` or `…` as a marker
// @Task special-case trailing line breaks in snippets

use super::{
    Role, Severity, Subseverity, Substitution, SubstitutionPart, UnboxedUntaggedDiagnostic,
};
use colored::{Color, ColoredString, Colorize};
use span::{
    source_map::{LineWithHighlight, LinesWithHighlight},
    FileName, SourceMap,
};
use std::fmt;
use unicode_width::UnicodeWidthStr;
use utilities::displayed;

#[cfg(test)]
mod test;

pub(super) fn format(diagnostic: &UnboxedUntaggedDiagnostic, map: Option<&SourceMap>) -> String {
    displayed(|f| write(diagnostic, map, f)).to_string()
}

fn write(
    diagnostic: &UnboxedUntaggedDiagnostic,
    map: Option<&SourceMap>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    write_header(diagnostic, f)?;

    let (padding, highlights, suggestions) = resolve_spans(diagnostic, map);

    let mut f = Formatter {
        diagnostic,
        bar: Line::Vertical.single().color(palette::FRAME),
        padding,
        f,
    };

    f.write_path()?;

    f.write_highlights(&highlights)?;

    for subdiagnostic in &diagnostic.subdiagnostics {
        f.write_subdiagnostic(subdiagnostic.severity, &subdiagnostic.message)?;
    }
    for (index, suggestion) in suggestions.iter().enumerate() {
        f.write_suggestion(suggestion)?;

        if index < suggestions.len() - 1 {
            f.write_bar()?;
        }
    }

    Ok(())
}

fn write_header(diagnostic: &UnboxedUntaggedDiagnostic, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", diagnostic.severity)?;

    let code = diagnostic
        .code
        .map(|code| format!("[{code}]").color(diagnostic.severity.color()))
        .unwrap_or_default();

    write!(f, "{code}")?;

    if let Some(message) = &diagnostic.message {
        write!(f, ": {}", message.bold())?;
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

struct Formatter<'a, 'f> {
    diagnostic: &'a UnboxedUntaggedDiagnostic,
    bar: ColoredString,
    padding: String,
    f: &'a mut fmt::Formatter<'f>,
}

impl Formatter<'_, '_> {
    fn write_path(&mut self) -> fmt::Result {
        let Some(path) = &self.diagnostic.path else { return Ok(()); };

        let needs_downward_connection =
            !self.diagnostic.highlights.is_empty() || !self.diagnostic.subdiagnostics.is_empty();

        let connector = if needs_downward_connection {
            Line::DownAndRight
        } else {
            Line::Horizontal
        }
        .single();

        writeln!(self.f)?;
        write!(
            self.f,
            "{} {}",
            self.padding,
            format!(
                "{}{} {}",
                connector,
                Line::Horizontal.single(),
                path.display()
            )
            .color(palette::FRAME),
        )?;

        if needs_downward_connection {
            self.write_bar()?;
        }

        Ok(())
    }

    fn write_highlights(&mut self, highlights: &[ResolvedHighlight<'_>]) -> fmt::Result {
        let mut needs_upward_connection = self.diagnostic.path.is_some();
        let needs_downward_connection =
            !self.diagnostic.subdiagnostics.is_empty() || !self.diagnostic.suggestions.is_empty();

        for (index, highlight) in highlights.iter().enumerate() {
            let file = displayed(|f| format_file_name(highlight.lines.file, f));
            let line = highlight.lines.first.number;
            let column = highlight.lines.first.highlight.start;

            let connector = if needs_upward_connection {
                Line::VerticalAndRight
            } else {
                Line::DownAndRight
            }
            .single();

            writeln!(self.f)?;
            write!(
                self.f,
                "{} {}",
                self.padding,
                format!(
                    "{connector}{} {file}:{line}:{column}",
                    Line::Horizontal.single()
                )
                .color(palette::FRAME)
            )?;

            match &highlight.lines.last {
                None => self.write_single_line_highlight(
                    &highlight.lines.first,
                    highlight.role,
                    highlight.label,
                ),
                Some(final_line) => self.write_multi_line_highlight(
                    &highlight.lines.first,
                    final_line,
                    highlight.role,
                    highlight.label,
                ),
            }?;

            if needs_downward_connection || index < highlights.len() - 1 {
                self.write_bar()?;
            }

            if !needs_upward_connection {
                needs_upward_connection = true;
            }
        }

        Ok(())
    }

    fn write_single_line_highlight(
        &mut self,
        line: &LineWithHighlight<'_>,
        role: Role,
        label: Option<&str>,
    ) -> fmt::Result {
        let snippet = line.content;
        let highlight_prefix_width = line.highlight.prefix_width;
        let zero_length_highlight = line.highlight.width == 0;
        let color = role.color(self.diagnostic.severity.color());
        let mut lines_of_label = label.iter().flat_map(|label| label.split('\n'));

        let snippet_padding = match zero_length_highlight && highlight_prefix_width == 0 {
            true => " ",
            false => "",
        };

        self.write_bar()?;
        writeln!(self.f)?;
        self.write_line_number(line.number)?;
        writeln!(self.f, " {snippet_padding}{snippet}")?;

        let underline_padding = " ".repeat(match zero_length_highlight {
            true => highlight_prefix_width.saturating_sub(1),
            false => highlight_prefix_width,
        });
        let underline = if !zero_length_highlight {
            Line::Horizontal.to_str(role).repeat(line.highlight.width)
        } else {
            format!(
                "{}{}",
                Line::RightAngleBracket.to_str(role),
                Line::LeftAngleBracket.to_str(role),
            )
        };
        let underline = underline.color(color);

        // The underline and the label.
        {
            write!(
                self.f,
                "{} {} {underline_padding}{underline}",
                self.padding, self.bar
            )?;

            if let Some(line_of_label) = lines_of_label.next() {
                write!(self.f, " {}", line_of_label.color(color))?;
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
                self.write_bar()?;

                if !line_of_label.is_empty() {
                    write!(self.f, " {spacing} {}", line_of_label.color(color))?;
                }
            }
        }

        Ok(())
    }

    fn write_multi_line_highlight(
        &mut self,
        first_line: &LineWithHighlight<'_>,
        final_line: &LineWithHighlight<'_>,
        role: Role,
        label: Option<&str>,
    ) -> fmt::Result {
        let color = role.color(self.diagnostic.severity.color());
        let mut lines_of_label = label.iter().flat_map(|label| label.split('\n'));
        // @Bug the hand is currently not dependent on the Unicode width of the first character
        let hand = Line::UpAndLeft.to_str(role).color(color);

        // The upper arm.
        {
            let snippet = first_line.content;
            let joint = Line::DownAndRight.to_str(role).color(color);
            let horizontal_arm = Line::Horizontal
                .to_str(role)
                .repeat(first_line.highlight.prefix_width + 1)
                .color(color);

            // If the first and the final line are futher apart than one,
            // write out a stylized ellipsis instead of a bar.
            let ellipsis_or_bar = if final_line.number - first_line.number > 1 {
                ELLIPSIS.color(palette::FRAME)
            } else {
                self.bar.clone()
            };

            self.write_bar()?;
            writeln!(self.f)?;
            self.write_line_number(first_line.number)?;
            writeln!(self.f, "   {snippet}")?;
            writeln!(
                self.f,
                "{} {ellipsis_or_bar} {joint}{horizontal_arm}{hand}",
                self.padding,
            )?;
        }

        // The connector and the lower arm.
        {
            let snippet = &final_line.content;
            // @Bug the arm is currently not dependent on the Unicode width of the last character
            let horizontal_arm = Line::Horizontal
                .to_str(role)
                .repeat(final_line.highlight.width)
                .color(color);
            let vertical_arm = Line::Vertical.to_str(role).color(color);
            let joint = Line::UpAndRight.to_str(role).color(color);
            self.write_line_number(final_line.number)?;
            writeln!(self.f, " {vertical_arm} {snippet}")?;

            // The lower arm and the label.
            {
                write!(
                    self.f,
                    "{} {} {joint}{horizontal_arm}{hand}",
                    self.padding, self.bar
                )?;

                if let Some(line_of_label) = lines_of_label.next() {
                    if !line_of_label.is_empty() {
                        write!(self.f, " {}", line_of_label.color(color))?;
                    }
                }

                let spacing = " ".repeat(1 + final_line.highlight.width + 1);

                for line_of_label in lines_of_label {
                    self.write_bar()?;

                    if !line_of_label.is_empty() {
                        write!(self.f, " {spacing} {}", line_of_label.color(color))?;
                    }
                }
            }
        }

        Ok(())
    }

    fn write_subdiagnostic(&mut self, severity: Subseverity, message: &str) -> fmt::Result {
        writeln!(self.f)?;
        write!(self.f, "{}{severity}: ", self.padding)?;

        let mut lines = message.split('\n');

        if let Some(line) = lines.next() {
            write!(self.f, "{line}")?;
        }

        let severity_spacing = " ".repeat(severity.name().width() + 1);

        for line in lines {
            if !line.is_empty() {
                writeln!(self.f)?;
                write!(self.f, "{}{severity_spacing} {line}", self.padding)?;
            }
        }

        Ok(())
    }

    // @Task support multi-line substitutions
    // @Task dedup w/ write_single_line_highlight
    // @Note this is an MVP
    // @Task write unit tests for this!
    fn write_suggestion(&mut self, suggestion: &ResolvedSuggestion<'_>) -> fmt::Result {
        self.write_subdiagnostic(Subseverity::Help, suggestion.message)?;

        let line = &suggestion.lines.first;
        let snippet = line.content;
        let highlight_prefix_width = line.highlight.prefix_width;
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

        let snippet_padding = match zero_length_highlight && highlight_prefix_width == 0 {
            true => " ",
            false => "",
        };

        self.write_bar()?;
        writeln!(self.f)?;
        self.write_line_number(line.number)?;

        // @Task but Unicode aware (no - 1 plz)
        let before = &snippet[..line.highlight.start as usize - 1];
        let after = &snippet[line.highlight.end as usize - 1..];

        // @Task write this more efficiently once we've replaced ‘colored’
        writeln!(
            self.f,
            " {snippet_padding}{before}{}{after}",
            suggestion
                .substitution
                .parts
                .iter()
                .map(|part| match part {
                    SubstitutionPart::Str(value) => value.to_string(),
                    SubstitutionPart::Placeholder(name) => name.italic().to_string(),
                })
                .collect::<String>()
                .color(palette::SUBSTITUTION)
        )?;

        let underline_padding = " ".repeat(match zero_length_highlight {
            true => highlight_prefix_width.saturating_sub(1),
            false => highlight_prefix_width,
        });
        let underline = if !zero_length_highlight {
            SUBSTITUTION.repeat(substitution_width)
        } else {
            format!(
                "{}{}",
                Line::RightAngleBracket.single(),
                Line::LeftAngleBracket.single(),
            )
        };
        let underline = underline.color(palette::SUBSTITUTION);

        write!(
            self.f,
            "{} {} {underline_padding}{underline}",
            self.padding, self.bar
        )?;

        Ok(())
    }

    fn write_bar(&mut self) -> fmt::Result {
        writeln!(self.f)?;
        write!(self.f, "{} {}", self.padding, self.bar)
    }

    fn write_line_number(&mut self, number: u32) -> fmt::Result {
        let number = format!("{0:>1$}", number, self.padding.len()).color(palette::FRAME);

        write!(self.f, "{number} {}", self.bar)
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

    pub(super) const fn color(self) -> Color {
        match self {
            Self::Bug | Self::Error => palette::ERROR,
            Self::Warning => palette::WARNING,
            Self::Debug => palette::DEBUG,
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name().color(self.color()).bold())
    }
}

impl Subseverity {
    const COLOR: Color = palette::HELP;
}

impl fmt::Display for Subseverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name().color(Self::COLOR).bold())
    }
}

impl Role {
    const fn color(self, primary: Color) -> Color {
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

fn format_file_name(name: &FileName, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match name {
        FileName::Anonymous => write!(f, "{}", "⟨anonymous⟩".italic()),
        FileName::Stdin => write!(f, "{}", "⟨stdin⟩".italic()),
        FileName::Path(path) => write!(f, "{}", path.display()),
        FileName::Str(name) => write!(f, "{name}"),
    }
}

const ELLIPSIS: &str = "·";
const SUBSTITUTION: &str = "~";

mod palette {
    use colored::Color;

    pub(super) const FRAME: Color = Color::BrightBlue;
    pub(super) const ERROR: Color = Color::BrightRed;
    pub(super) const WARNING: Color = Color::BrightYellow;
    pub(super) const HELP: Color = Color::BrightCyan;
    pub(super) const DEBUG: Color = Color::BrightMagenta;
    pub(super) const SUBSTITUTION: Color = Color::Green;
}
