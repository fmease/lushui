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

use super::{Diagnostic, Highlight, Role, Severity, Subdiagnostic, Subseverity, UnboxedDiagnostic};
use crate::span::{
    source_map::{Line, Lines},
    SourceMap,
};
use colored::{Color, ColoredString, Colorize};
use std::{fmt, iter::once, path::Path};
use unicode_width::UnicodeWidthStr;

impl Diagnostic {
    /// Format the diagnostic for the use in a terminal.
    pub(super) fn format_for_terminal(&self, map: Option<&SourceMap>) -> String {
        // @Task add back the alorithm which reduces the amount of paths printed
        // @Beacon @Task special case trailing line break in subdiagnostics
        // @Beacon @Bug tabs \t mess up the alignment! (in text literals, in metadata files)
        // since `"\t".width() == 0`! solution: replace tabs with N spaces in the place where
        // we measure the width() and in the rendered string!

        #[rustfmt::skip]
        TerminalFormat { diagnostic: &self.0, map }.to_string()
    }
}

struct TerminalFormat<'a> {
    diagnostic: &'a UnboxedDiagnostic,
    map: Option<&'a SourceMap>,
}

impl TerminalFormat<'_> {
    fn format_header(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.diagnostic.severity)?;

        let code = self
            .diagnostic
            .code
            .map(|code| format!("[{code}]").color(self.diagnostic.severity.color()))
            .unwrap_or_default();

        write!(f, "{code}")
    }

    fn format_highlights(&self, padding: &mut String, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let highlights = &self.diagnostic.highlights;

        if highlights.is_empty() {
            *padding = " ".into();
            return Ok(());
        }

        let map = self
            .map
            .expect("missing source map for a diagnostic with highlights");

        let rows_of_lines_of_highlights = highlights
            .iter()
            .map(|highlight| map.lines(highlight.span))
            .collect::<Vec<_>>();

        *padding = " ".repeat(calculate_padding(&rows_of_lines_of_highlights));

        for (highlight, lines) in highlights.iter().zip(rows_of_lines_of_highlights) {
            format_path(&lines, padding, f)?;

            let bar = "|".color(color_palette::FRAME).bold();

            match &lines.last_line {
                None => self.format_single_line_highlight(&highlight, &lines, &bar, padding, f),
                Some(final_line) => self.format_multi_line_highlight(
                    &highlight,
                    &lines,
                    &final_line,
                    &bar,
                    padding,
                    f,
                ),
            }?;

            writeln!(f)?;
            write!(f, "{padding} {bar}")?;
        }

        Ok(())
    }

    fn format_single_line_highlight(
        &self,
        highlight: &Highlight,
        lines: &Lines<'_>,
        bar: &ColoredString,
        padding: &str,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let line_number = lines.first_line.number;
        let snippet = lines.first_line.content;
        let highlight_padding_width = lines.first_line.highlight_padding_width;
        let zero_length_highlight = lines.first_line.highlight_width == 0;
        let role_color = highlight.role.color(self.diagnostic.severity.color());
        let mut lines_of_label = highlight.label.iter().flat_map(|label| label.split('\n'));

        let snippet_padding = match zero_length_highlight && highlight_padding_width == 0 {
            true => " ",
            false => "",
        };

        writeln!(f)?;
        writeln!(f, "{padding} {bar}")?;
        writeln!(
            f,
            "{line_number:>padding$} {bar} {snippet_padding}{snippet}",
            padding = padding.len(),
        )?;

        let underline_padding = " ".repeat(match zero_length_highlight {
            true => highlight_padding_width.saturating_sub(1),
            false => highlight_padding_width,
        });
        let underline = if !zero_length_highlight {
            highlight
                .role
                .symbol()
                .repeat(lines.first_line.highlight_width)
        } else {
            "><".to_owned()
        };
        let underline = underline.color(role_color).bold();

        // the underline and the label
        {
            write!(f, "{padding} {bar} {underline_padding}{underline}")?;

            if let Some(line_of_label) = lines_of_label.next() {
                write!(f, " {}", line_of_label.color(role_color))?;
            }

            let spacing = " ".repeat(
                lines.first_line.highlight_padding_width
                    + if zero_length_highlight {
                        1
                    } else {
                        lines.first_line.highlight_width
                    },
            );

            for line_of_label in lines_of_label {
                writeln!(f)?;
                write!(f, "{padding} {bar}")?;

                if !line_of_label.is_empty() {
                    write!(f, " {spacing} {}", line_of_label.color(role_color))?;
                }
            }
        }

        Ok(())
    }

    fn format_multi_line_highlight(
        &self,
        highlight: &Highlight,
        lines: &Lines<'_>,
        final_line: &Line<'_>,
        bar: &ColoredString,
        padding: &str,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let role_color = highlight.role.color(self.diagnostic.severity.color());
        let mut lines_of_label = highlight.label.iter().flat_map(|label| label.split('\n'));

        // the upper arm
        {
            let line_number = lines.first_line.number;
            let snippet = lines.first_line.content;
            let horizontal_arm = "_"
                .repeat(lines.first_line.highlight_padding_width + 1)
                .color(role_color)
                .bold();
            // the hand is currently not dependent on the Unicode width of the first character
            let hand = highlight.role.symbol().color(role_color).bold();
            let ellipsis_or_bar = if final_line.number - lines.first_line.number > 1 {
                "...".into()
            } else {
                format!(" {bar} ")
            };

            writeln!(f)?;
            writeln!(f, "{padding} {bar}")?;
            writeln!(
                f,
                "{line_number:>padding$} {bar}   {snippet}",
                padding = padding.len(),
            )?;
            writeln!(f, "{padding}{ellipsis_or_bar} {horizontal_arm}{hand}")?;
        }

        // the connector and the lower arm
        {
            let line_number = final_line.number;
            let snippet = &final_line.content;
            // the arm is currently not dependent on the Unicode width of the last character
            let horizontal_arm = "_"
                .repeat(final_line.highlight_width)
                .color(role_color)
                .bold();
            let vertical_arm = "|".color(role_color).bold();
            // the hand is currently not dependent on the Unicode width of the 1st character
            let hand = highlight.role.symbol().color(role_color).bold();

            writeln!(
                f,
                "{line_number:>padding$} {bar} {vertical_arm} {snippet}",
                padding = padding.len(),
            )?;

            // the lower arm and the label
            {
                write!(f, "{padding} {bar} {vertical_arm}{horizontal_arm}{hand}")?;

                if let Some(line_of_label) = lines_of_label.next() {
                    if !line_of_label.is_empty() {
                        write!(f, " {}", line_of_label.color(role_color))?;
                    }
                }

                let spacing = " ".repeat(1 + final_line.highlight_width + 1);

                for line_of_label in lines_of_label {
                    writeln!(f)?;
                    write!(f, "{padding} {bar}")?;

                    if !line_of_label.is_empty() {
                        write!(f, " {spacing} {}", line_of_label.color(role_color))?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for TerminalFormat<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_header(f)?;

        // text message
        if let Some(message) = &self.diagnostic.message {
            write!(f, ": {}", message.bold())?;
        }

        let mut padding = String::new();
        self.format_highlights(&mut padding, f)?;
        for subdiagnostic in &self.diagnostic.subdiagnostics {
            format_subdiagnostic(subdiagnostic, &padding, f)?;
        }
        Ok(())
    }
}

fn format_path(lines: &Lines<'_>, padding: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f)?;
    write!(f, "{padding}{} ", "-->".color(color_palette::FRAME).bold())?;

    let path = lines.path.map(Path::to_string_lossy).unwrap_or_default();
    let line = lines.first_line.number;
    let column = lines.first_line.highlight_start_column;
    // unbelieveably wasteful memory-wise but inevitable due to the API of `colored`
    write!(
        f,
        "{}",
        format!("{path}:{line}:{column}").color(color_palette::FRAME)
    )
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

fn calculate_padding(rows_of_lines: &[Lines<'_>]) -> usize {
    let mut padding = 0;

    let mut largest_line_number = rows_of_lines
        .iter()
        .flat_map(|span| {
            once(span.first_line.number).chain(span.last_line.as_ref().map(|line| line.number))
        })
        .max()
        .unwrap();

    while largest_line_number > 0 {
        largest_line_number /= 10;
        padding += 1;
    }

    padding
}

impl Severity {
    const fn color(self) -> Color {
        match self {
            Self::Bug | Self::Error => color_palette::ERROR,
            Self::Warning => color_palette::WARNING,
            Self::Debug => color_palette::DEBUG,
        }
    }

    const fn name(self) -> &'static str {
        match self {
            Self::Bug => "internal compiler error",
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Debug => "internal debugging message",
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name().color(self.color()).bold())
    }
}

impl Subseverity {
    const fn color(self) -> Color {
        match self {
            Self::Note | Self::Help => color_palette::HELP,
            Self::Debug => color_palette::DEBUG,
        }
    }
}

impl fmt::Display for Subseverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name().color(self.color()).bold())
    }
}

impl Role {
    const fn color(self, primary: Color) -> Color {
        match self {
            Self::Primary => primary,
            Self::Secondary => color_palette::HELP,
        }
    }

    const fn symbol(self) -> &'static str {
        match self {
            Self::Primary => "^",
            Self::Secondary => "-",
        }
    }
}

mod color_palette {
    use colored::Color;

    pub(super) const FRAME: Color = Color::BrightBlue;
    pub(super) const ERROR: Color = Color::BrightRed;
    pub(super) const WARNING: Color = Color::BrightYellow;
    pub(super) const HELP: Color = Color::BrightCyan;
    pub(super) const DEBUG: Color = Color::BrightMagenta;
}
