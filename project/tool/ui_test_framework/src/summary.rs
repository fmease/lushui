//! The summary of the entire test suite.
#![allow(clippy::cast_precision_loss)]

use crate::Gilding;
use std::{
    io::{self, Write},
    time::Duration,
};
use utility::paint::{AnsiColor, Painter};

pub(crate) struct TestSuiteSummary {
    pub(crate) statistics: TestSuiteStatistics,
    pub(crate) gilding: Gilding,
    pub(crate) duration: Duration,
}

impl TestSuiteSummary {
    pub(crate) fn render(&self, painter: &mut Painter) -> io::Result<()> {
        write!(painter, "  ")?;

        if self.statistics.passed() {
            let (message, color) = if self.gilding == Gilding::No {
                if self.statistics.total_amount() != 0 {
                    ("ALL TESTS PASSED!", AnsiColor::Green)
                } else {
                    ("ALL TESTS WERE FILTERED OUT!", AnsiColor::Yellow)
                }
            } else if self.statistics.gilded == 0 {
                ("ALL TESTS PASSED WITHOUT GILDING!", AnsiColor::Green)
            } else {
                ("SOME TESTS WERE GILDED!", AnsiColor::Blue)
            };

            painter.set(color)?;
            write!(painter, "{message}")?;
            painter.unset()?;
        } else {
            painter.set(AnsiColor::Red)?;

            if self.statistics.failed > 0 {
                write!(painter, "SOME TESTS FAILED!")?;
            }

            if self.statistics.invalid > 0 {
                if self.statistics.failed > 0 {
                    write!(painter, " ")?;
                }

                write!(painter, "SOME INVALID TESTS FOUND!")?;
            }

            painter.unset()?;
        };
        writeln!(painter)?;

        write!(painter, "    {} ", self.statistics.passed)?;
        painter.set(AnsiColor::Green)?;
        if self.gilding == Gilding::No {
            write!(painter, "passed")?;
        } else {
            write!(painter, "passed without gilding")?;
        }
        painter.unset()?;

        write!(painter, " ({:.2}%)", self.statistics.ratio_passed_vs_included(),)?;

        fn column(
            count: usize,
            tag: &str,
            color: AnsiColor,
            painter: &mut Painter,
        ) -> io::Result<()> {
            write!(painter, " | {count} ")?;
            painter.set(color)?;
            write!(painter, "{tag}")?;
            painter.unset()
        }

        if self.gilding == Gilding::Yes {
            column(self.statistics.gilded, "gilded", AnsiColor::Blue, painter)?;
        }

        if self.statistics.invalid > 0 {
            column(self.statistics.invalid, "invalid", AnsiColor::Red, painter)?;
        }

        column(self.statistics.failed, "failed", AnsiColor::Red, painter)?;

        column(self.statistics.ignored, "ignored", AnsiColor::Yellow, painter)?;

        write!(painter, " | {} filtered out", self.statistics.skipped)?;
        write!(painter, " | {} in total", self.statistics.total_amount())?;

        write!(
            painter,
            " ({:.2}% of {})",
            self.statistics.filter_ratio(),
            self.statistics.unfiltered_total_amount(),
        )?;

        writeln!(painter)?;
        painter.set(AnsiColor::BrightBlack)?;
        writeln!(painter, "    {:.2?}", self.duration)?;
        painter.unset()
    }
}

// `failed` does not necessarily equal `gilded` since the former includes
// invalid tests which are not gilded
#[derive(Default, Clone)]
pub(crate) struct TestSuiteStatistics {
    pub(crate) ignored: usize,
    pub(crate) passed: usize,
    pub(crate) failed: usize,
    pub(crate) gilded: usize,
    pub(crate) skipped: usize,
    pub(crate) invalid: usize,
}

impl TestSuiteStatistics {
    pub(crate) fn passed(&self) -> bool {
        self.failed == 0 && self.invalid == 0
    }

    // @Note horrid name
    fn included(&self) -> usize {
        self.passed + self.gilded + self.failed + self.invalid
    }

    fn ratio_passed_vs_included(&self) -> f32 {
        let ratio = match self.included() {
            0 => 1.0,
            included => self.passed as f32 / included as f32,
        };

        ratio * 100.0
    }

    // @Note misleading name
    pub(crate) fn total_amount(&self) -> usize {
        self.included() + self.ignored
    }

    // @Note horrid name
    fn unfiltered_total_amount(&self) -> usize {
        self.total_amount() + self.skipped
    }

    // @Note horrible name
    fn filter_ratio(&self) -> f32 {
        let ratio = self.total_amount() as f32 / self.unfiltered_total_amount() as f32;
        ratio * 100.0
    }
}

impl std::ops::Add for &TestSuiteStatistics {
    type Output = TestSuiteStatistics;

    fn add(self, other: Self) -> Self::Output {
        TestSuiteStatistics {
            ignored: self.ignored + other.ignored,
            passed: self.passed + other.passed,
            failed: self.failed + other.failed,
            gilded: self.gilded + other.gilded,
            skipped: self.skipped + other.skipped,
            invalid: self.invalid + other.invalid,
        }
    }
}

impl std::ops::AddAssign<&Self> for TestSuiteStatistics {
    fn add_assign(&mut self, other: &Self) {
        *self = &*self + other;
    }
}
