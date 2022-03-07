use derivation::{Elements, FromStr, Str};
use joinery::JoinableIterator;
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
        // @Bug if someone mixes up "#" and ";;;", the error message is misleading
        let line = source
            .lines()
            .next()
            .and_then(|line| line.strip_prefix(language.comment()))
            .and_then(|line| line.strip_prefix(MAGIC_TEXT))
            .ok_or(MissingConfiguration)?;

        let mut arguments = line.split_ascii_whitespace();

        let tag = arguments.next().ok_or(MissingTag)?;

        Ok(Configuration {
            tag: tag.parse().map_err(|_| InvalidTag(tag.to_owned()))?,
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

#[derive(Clone, Copy, Elements, FromStr, Str)]
#[format(dash_case)]
pub(crate) enum TestTag {
    Auxiliary,
    Fail,
    Ignore,
    Pass,
}

impl fmt::Display for TestTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Debug)]
pub(crate) enum ParseError {
    MissingConfiguration,
    MissingTag,
    InvalidTag(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingConfiguration => write!(
                f,
                "The test does not have a configuration i.e. a comment at the start of the file \
                 starting with the code `{MAGIC_TEXT}`"
            ),
            Self::MissingTag | Self::InvalidTag(_) => {
                let tags = TestTag::elements()
                    .map(|tag| format!("`{tag}`"))
                    .join_with(", ");

                #[allow(clippy::match_wildcard_for_single_variants)]
                match self {
                    Self::MissingTag => {
                        write!(
                            f,
                            "The test does not have a tag. Consider using one of the following tags \
                             in the configuration: {tags}"
                        )
                    }
                    Self::InvalidTag(tag) => {
                        write!(
                            f,
                            "The test contains the invalid tag `{tag}`. Valid tags are {tags}",
                        )
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}
