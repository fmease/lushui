use std::fmt;

const MAGIC_TEXT: &str = " TEST ";

pub(crate) struct Configuration<'a> {
    pub(crate) tag: TestTag,
    pub(crate) arguments: Vec<&'a str>,
}

impl<'a> Configuration<'a> {
    pub(crate) fn parse(source: &'a str, language: Language) -> Result<Self, ParseError> {
        use ParseError::*;

        // @Bug the prefix `;;; TEST` (no final space) leads to ParseError::MissingTag but
        // ideally, it should lead to ParseError::MissingTag. we need to apply trimming beforehand
        // (and restructure the code a bit)
        let line = source
            .lines()
            .next()
            .and_then(|line| line.strip_prefix(language.comment()))
            .and_then(|line| line.strip_prefix(MAGIC_TEXT))
            .ok_or(MissingPrefix)?;

        let mut arguments = line.split_ascii_whitespace();

        let tag = arguments.next().ok_or(MissingTag)?;

        Ok(Configuration {
            tag: match tag {
                "ignore" => TestTag::Ignore,
                "pass" => TestTag::Pass,
                "fail" => TestTag::Fail,
                "auxiliary" => TestTag::Auxiliary,
                tag => return Err(InvalidTag(tag.to_owned())),
            },
            arguments: arguments.collect(),
        })
    }
}

#[derive(Clone, Copy)]
#[cfg_attr(not(FALSE), derive(Debug))]
pub(crate) enum Language {
    Lushui,
    Metadata,
}

impl Language {
    const fn comment(self) -> &'static str {
        match self {
            Self::Lushui => ";;;",
            Self::Metadata => "#",
        }
    }
}

pub(crate) enum TestTag {
    Auxiliary,
    Fail,
    Ignore,
    Pass,
}

impl TestTag {
    // @Task derive this
    const VALUES: &'static str = "`auxiliary`, `fail`, `ignore` and `pass`";
}

#[derive(Debug)]
pub(crate) enum ParseError {
    MissingPrefix,
    MissingTag,
    InvalidTag(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // @Beacon @Beacon @Beacon @Task update to new system
            Self::MissingPrefix => write!(
                f,
                "the test file is missing a test configuration \
                 which is prefixed with `{MAGIC_TEXT}`"
            ),
            Self::MissingTag => write!(
                f,
                "the test file is missing a tag; valid tags are {}",
                TestTag::VALUES
            ),
            Self::InvalidTag(argument) => {
                write!(
                    f,
                    "the test file contains the invalid tag `{argument}`; valid tags are {}",
                    TestTag::VALUES
                )
            }
        }
    }
}
