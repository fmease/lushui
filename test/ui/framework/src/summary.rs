//! The summary of the entire test suite.
#![allow(clippy::cast_precision_loss)]

use crate::Gilding;
use colored::Colorize;
use std::{fmt, time::Duration};

pub(crate) struct TestSuiteSummary {
    pub(crate) statistics: TestSuiteStatistics,
    pub(crate) gilding: Gilding,
    pub(crate) duration: Duration,
}

impl fmt::Display for TestSuiteSummary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let status = if self.statistics.passed() {
            if self.gilding == Gilding::No {
                if self.statistics.total_amount() != 0 {
                    "ALL TESTS PASSED!".green()
                } else {
                    "ALL TESTS WERE FILTERED OUT!".yellow()
                }
            } else if self.statistics.gilded == 0 {
                "ALL TESTS PASSED WITHOUT GILDING!".green()
            } else {
                "SOME TESTS WERE GILDED!".blue()
            }
        } else {
            let mut message = String::new();

            if self.statistics.failed > 0 {
                message += "SOME TESTS FAILED!";
            }

            if self.statistics.invalid > 0 {
                if self.statistics.failed > 0 {
                    message.push(' ');
                }

                message += "SOME INVALID TESTS FOUND!";
            }

            message.red()
        };

        write!(
            f,
            "  {status}
    {passed} {label_passed} ({ratio:.2}%)",
            passed = self.statistics.passed,
            label_passed = if self.gilding == Gilding::No {
                "passed".green()
            } else {
                "passed without gilding".green()
            },
            ratio = self.statistics.ratio_passed_vs_included(),
        )?;

        if self.gilding == Gilding::Yes {
            write!(
                f,
                " | {gilded} {label_gilded}",
                gilded = self.statistics.gilded,
                label_gilded = "gilded".blue(),
            )?;
        }

        if self.statistics.invalid > 0 {
            write!(
                f,
                " | {invalid} {label_invalid}",
                invalid = self.statistics.invalid,
                label_invalid = "invalid".red(),
            )?;
        }

        writeln!(f, " | {failed} {label_failed} | {ignored} {label_ignored} | {filtered_out} filtered out | {total} in total ({filter_ratio:.2}% of {unfiltered_total})
    {duration}\
",
            failed = self.statistics.failed,
            label_failed = "failed".red(),
            ignored = self.statistics.ignored,
            label_ignored = "ignored".yellow(),
            total = self.statistics.total_amount(),
            filtered_out = self.statistics.skipped,
            unfiltered_total = self.statistics.unfiltered_total_amount(),
            filter_ratio = self.statistics.filter_ratio(),
            duration = format!("{:.2?}", self.duration).bright_black(),
        )?;

        Ok(())
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
