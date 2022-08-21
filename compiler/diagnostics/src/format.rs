//! Diagnostic formatting.
//!
//! # Tasks and Issues
//!
//! * diagnostics with primary and secondary spans are not *that* readable because of
//!   all those lengthy paths. That didn't use to be the case, maybe we should some
//!   rules when the paths can be omitted
//! * cannot handle overly long lines of highlighted code (does not look tidy anymore)
//! * unindenting long lines of highlighted source code (i.e. mapping initial whitespace to
//!   a single one) rustc replaces large amount of spaces with colored ellipses `...`

use super::{Highlight, Role, Severity, Subdiagnostic, Subseverity, UnboxedUntaggedDiagnostic};
use colored::{Color, ColoredString, Colorize};
use span::{
    source_map::{LineWithHighlight, LinesWithHighlight},
    SourceMap,
};
use std::{fmt, iter::once, path::Path};
use unicode_width::UnicodeWidthStr;

#[cfg(test)]
mod test;

// @Task add back the alorithm which reduces the amount of paths printed
// @Beacon @Task special case trailing line break in subdiagnostics
// @Beacon @Bug tabs \t mess up the alignment! (in text literals, in metadata files)
// since `"\t".width() == 0`! solution: replace tabs with N spaces in the place where
// we measure the width() and in the rendered string!
pub(super) fn format(diagnostic: &UnboxedUntaggedDiagnostic, map: Option<&SourceMap>) -> String {
    Formatter { diagnostic, map }.to_string()
}

struct Formatter<'a> {
    diagnostic: &'a UnboxedUntaggedDiagnostic,
    map: Option<&'a SourceMap>,
}

impl<'a> Formatter<'a> {
    fn format_header(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.diagnostic.severity)?;

        let code = self
            .diagnostic
            .code
            .map(|code| format!("[{code}]").color(self.diagnostic.severity.color()))
            .unwrap_or_default();

        write!(f, "{code}")?;

        if let Some(message) = &self.diagnostic.message {
            write!(f, ": {}", message.bold())?;
        }

        Ok(())
    }

    fn format_path_and_highlights(
        &self,
        padding: &mut String,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let highlights = &self.diagnostic.highlights;

        let rows_of_lines = if highlights.is_empty() {
            *padding = " ".into();
            None
        } else {
            let map = self
                .map
                .expect("missing source map for a diagnostic with highlights");

            let rows_of_lines = highlights
                .iter()
                .map(|highlight| map.lines_with_highlight(highlight.span))
                .collect::<Vec<_>>();

            *padding = " ".repeat(calculate_padding(&rows_of_lines));
            Some(rows_of_lines)
        };

        let bar = Line::Vertical.single().color(palette::FRAME);
        let mut needs_upward_connection = false;

        if let Some(path) = &self.diagnostic.path {
            let path = path.to_string_lossy();
            let is_final = highlights.is_empty() && self.diagnostic.subdiagnostics.is_empty();

            writeln!(f)?;
            write!(
                f,
                "{padding} {}",
                format!(
                    "{}{} {path}",
                    if is_final {
                        Line::Horizontal
                    } else {
                        Line::DownAndRight
                    }
                    .single(),
                    Line::Horizontal.single()
                )
                .color(palette::FRAME)
            )?;

            if !is_final {
                writeln!(f)?;
                write!(f, "{padding} {bar}")?;
            }

            needs_upward_connection = true;
        }

        let Some(rows_of_lines) = rows_of_lines else {
            return Ok(());
        };

        for (highlight, lines) in highlights.iter().zip(rows_of_lines) {
            let path = lines.path.map(Path::to_string_lossy).unwrap_or_default();
            let line = lines.first.number;
            let column = lines.first.highlight.start;

            writeln!(f)?;
            write!(
                f,
                "{padding} {}",
                format!(
                    "{}{} {path}:{line}:{column}",
                    if needs_upward_connection {
                        Line::VerticalAndRight
                    } else {
                        Line::DownAndRight
                    }
                    .single(),
                    Line::Horizontal.single()
                )
                .color(palette::FRAME)
            )?;

            match &lines.last {
                None => self.format_single_line_highlight(highlight, &lines, &bar, padding, f),
                Some(final_line) => self
                    .format_multi_line_highlight(highlight, &lines, final_line, &bar, padding, f),
            }?;

            writeln!(f)?;
            write!(f, "{padding} {bar}")?;

            if !needs_upward_connection {
                needs_upward_connection = true;
            }
        }

        Ok(())
    }

    fn format_single_line_highlight(
        &self,
        highlight: &Highlight,
        lines: &LinesWithHighlight<'_>,
        bar: &ColoredString,
        padding: &str,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let number = format!("{0:>1$}", lines.first.number, padding.len()).color(palette::FRAME);
        let snippet = lines.first.content;
        let highlight_prefix_width = lines.first.highlight.prefix_width;
        let zero_length_highlight = lines.first.highlight.width == 0;
        let color = highlight.role.color(self.diagnostic.severity.color());
        let mut lines_of_label = highlight.label.iter().flat_map(|label| label.split('\n'));

        let snippet_padding = match zero_length_highlight && highlight_prefix_width == 0 {
            true => " ",
            false => "",
        };

        writeln!(f)?;
        writeln!(f, "{padding} {bar}")?;
        writeln!(f, "{number} {bar} {snippet_padding}{snippet}")?;

        let underline_padding = " ".repeat(match zero_length_highlight {
            true => highlight_prefix_width.saturating_sub(1),
            false => highlight_prefix_width,
        });
        let underline = if !zero_length_highlight {
            Line::Horizontal
                .to_str(highlight.role)
                .repeat(lines.first.highlight.width)
        } else {
            format!(
                "{}{}",
                Line::RightAngleBracket.to_str(highlight.role),
                Line::LeftAngleBracket.to_str(highlight.role),
            )
        };
        let underline = underline.color(color);

        // the underline and the label
        {
            write!(f, "{padding} {bar} {underline_padding}{underline}")?;

            if let Some(line_of_label) = lines_of_label.next() {
                write!(f, " {}", line_of_label.color(color))?;
            }

            let spacing = " ".repeat(
                lines.first.highlight.prefix_width
                    + if zero_length_highlight {
                        1
                    } else {
                        lines.first.highlight.width
                    },
            );

            for line_of_label in lines_of_label {
                writeln!(f)?;
                write!(f, "{padding} {bar}")?;

                if !line_of_label.is_empty() {
                    write!(f, " {spacing} {}", line_of_label.color(color))?;
                }
            }
        }

        Ok(())
    }

    fn format_multi_line_highlight(
        &self,
        highlight: &Highlight,
        lines: &LinesWithHighlight<'_>,
        final_line: &LineWithHighlight<'_>,
        bar: &ColoredString,
        padding: &str,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let color = highlight.role.color(self.diagnostic.severity.color());
        let mut lines_of_label = highlight.label.iter().flat_map(|label| label.split('\n'));
        // the hand is currently not dependent on the Unicode width of the first character
        let hand = Line::UpAndLeft.to_str(highlight.role).color(color);

        // the upper arm
        {
            let number =
                format!("{0:>1$}", lines.first.number, padding.len()).color(palette::FRAME);
            let snippet = lines.first.content;
            let joint = Line::DownAndRight.to_str(highlight.role).color(color);
            let horizontal_arm = Line::Horizontal
                .to_str(highlight.role)
                .repeat(lines.first.highlight.prefix_width + 1)
                .color(color);
            let ellipsis_or_bar = if final_line.number - lines.first.number > 1 {
                "·".color(palette::FRAME)
            } else {
                bar.clone()
            };

            writeln!(f)?;
            writeln!(f, "{padding} {bar}")?;
            writeln!(f, "{number} {bar}   {snippet}")?;
            writeln!(
                f,
                "{padding} {ellipsis_or_bar} {joint}{horizontal_arm}{hand}"
            )?;
        }

        // the connector and the lower arm
        {
            let number = format!("{0:>1$}", final_line.number, padding.len()).color(palette::FRAME);
            let snippet = &final_line.content;
            // the arm is currently not dependent on the Unicode width of the last character
            let horizontal_arm = Line::Horizontal
                .to_str(highlight.role)
                .repeat(final_line.highlight.width)
                .color(color);
            let vertical_arm = Line::Vertical.to_str(highlight.role).color(color);
            let joint = Line::UpAndRight.to_str(highlight.role).color(color);
            writeln!(f, "{number} {bar} {vertical_arm} {snippet}")?;

            // the lower arm and the label
            {
                write!(f, "{padding} {bar} {joint}{horizontal_arm}{hand}")?;

                if let Some(line_of_label) = lines_of_label.next() {
                    if !line_of_label.is_empty() {
                        write!(f, " {}", line_of_label.color(color))?;
                    }
                }

                let spacing = " ".repeat(1 + final_line.highlight.width + 1);

                for line_of_label in lines_of_label {
                    writeln!(f)?;
                    write!(f, "{padding} {bar}")?;

                    if !line_of_label.is_empty() {
                        write!(f, " {spacing} {}", line_of_label.color(color))?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Formatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_header(f)?;

        let mut padding = String::new();
        self.format_path_and_highlights(&mut padding, f)?;
        for subdiagnostic in &self.diagnostic.subdiagnostics {
            format_subdiagnostic(subdiagnostic, &padding, f)?;
        }
        Ok(())
    }
}

fn format_subdiagnostic(
    subdiagnostic: &Subdiagnostic,
    padding: &str,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    writeln!(f)?;
    write!(f, "{padding}{}: ", subdiagnostic.severity)?;

    let mut lines_of_message = subdiagnostic.message.split('\n');

    if let Some(line_of_message) = lines_of_message.next() {
        write!(f, "{line_of_message}")?;
    }

    for line_of_message in lines_of_message {
        if !line_of_message.is_empty() {
            let severity_spacing = " ".repeat(subdiagnostic.severity.name().width() + 1);

            writeln!(f)?;
            write!(f, "{padding}{severity_spacing} {line_of_message}")?;
        }
    }

    Ok(())
}

fn calculate_padding(rows_of_lines: &[LinesWithHighlight<'_>]) -> usize {
    let mut padding = 0;

    let mut largest_line_number = rows_of_lines
        .iter()
        .flat_map(|span| once(span.first.number).chain(span.last.as_ref().map(|line| line.number)))
        .max()
        .unwrap();

    while largest_line_number > 0 {
        largest_line_number /= 10;
        padding += 1;
    }

    padding
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

mod palette {
    use colored::Color;

    pub(super) const FRAME: Color = Color::BrightBlue;
    pub(super) const ERROR: Color = Color::BrightRed;
    pub(super) const WARNING: Color = Color::BrightYellow;
    pub(super) const HELP: Color = Color::BrightCyan;
    pub(super) const DEBUG: Color = Color::BrightMagenta;
}
