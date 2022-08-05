//! Test configuration.

use crate::TestType;
use derivation::{FromStr, Str};
use std::{collections::HashMap, default::default, str::FromStr, time::Duration};
use utilities::{pluralize, Str};

#[cfg(test)]
mod tests;

// @Task parse quotes in configurations to enable support for arguments containing whitespace

const PREFIX: &str = " TEST ";

/// The test configuration.
#[derive(Default)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub(crate) struct Configuration<'src> {
    pub(crate) tag: TestTag<'src>,
    pub(crate) compiler_args: Vec<&'src str>,
    pub(crate) compiler_env_vars: HashMap<&'src str, &'src str>,
    pub(crate) program_args: Vec<&'src str>,
    pub(crate) program_env_vars: HashMap<&'src str, &'src str>,
    pub(crate) timeout: Timeout,
    pub(crate) substitutions: HashMap<&'src str, &'src str>,
    pub(crate) revisions: Vec<&'src str>,
}

impl<'src> Configuration<'src> {
    // @Task error spans
    pub(crate) fn parse(source: &'src str, type_: TestType) -> Result<Self, Error> {
        let mut configuration = Configuration::default();

        let mut tag = None;
        let mut timeout = None;

        let comment = type_.language().comment();

        for line in source.lines() {
            if let Some(line) = line.strip_prefix(comment)
            && let Some(line) = line.strip_prefix(PREFIX)
            {
                use ParameterKind::*;

                let parameter = parse_parameter(line, type_)?;

                if !parameter.revisions.is_empty() {
                    return Err(Error::new("revisions are not supported yet"));
                }

                match parameter.kind {
                    Pass { .. } | Fail { .. } | Auxiliary { .. } | Ignore => {
                        if tag.is_some() {
                            // @Task better message
                            return Err(Error::new("a test tag is already set"));
                        }

                        tag = Some(match parameter.kind {
                            Pass { mode } => TestTag::Pass { mode },
                            Fail { mode } => TestTag::Fail { mode },
                            Auxiliary { users } => TestTag::Auxiliary { users },
                            Ignore => TestTag::Ignore,
                            _ => unreachable!(),
                        });
                    }
                    CompilerArgs(arguments) => configuration.compiler_args.extend(arguments),
                    CompilerEnvVar { name, value } => {
                        if configuration.compiler_env_vars.contains_key(name) {
                            // @Task better message
                            return Err(Error::new(
                                format!("the compiler environment variable ‘{name}’ is already set"),
                            ));
                        }

                        configuration.compiler_env_vars.insert(name, value);
                    }
                    ProgramArgs(arguments) => {
                        configuration.program_args.extend(arguments);

                        return Err(Error::new(
                            "the test parameter ‘program-args’ is not supported yet",
                        ));
                    }
                    ProgramEnvVar { name, value } => {
                        if configuration.program_env_vars.contains_key(name) {
                            // @Task better message
                            return Err(Error::new(
                                format!("the program environment variable ‘{name}’ is already set"),
                            ));
                        }

                        configuration.program_env_vars.insert(name, value);

                        return Err(Error::new(
                            "the test parameter ‘program-env-var’ is not supported yet",
                        ));
                    }
                    Timeout(timeout_) => {
                        if timeout.is_some() {
                            // @Task better message
                            return Err(Error::new("a timeout is already set"));
                        }

                        timeout = Some(timeout_);
                    }
                    Substitution { name, value } => {
                        if configuration.substitutions.contains_key(name) {
                            // @Task better message
                            return Err(Error::new(format!("the substitution ‘{name}’ is already set")));
                        }

                        configuration.substitutions.insert(name, value);

                        return Err(Error::new(
                            "the test parameter ‘substitution’ is not supported yet",
                        ));
                    }
                    Revisions(revisions) => {
                        if !configuration.revisions.is_empty() {
                            // @Task better message
                            return Err(Error::new("the revisions are already set"));
                        }

                        if !parameter.revisions.is_empty() {
                            // @Task better msg
                            return Err(Error::new(
                                "the test parameter ‘revisions’ cannot itself depend on revisions",
                            ));
                        }

                        configuration.revisions = revisions;

                        return Err(Error::new(
                            "the test parameter ‘revisions’ is not supported yet",
                        ));
                    }
                }
            }
        }

        if let Some(tag) = tag {
            configuration.tag = tag;
        }
        if let Some(timeout) = timeout {
            configuration.timeout = timeout;
        }

        Ok(configuration)
    }
}

// @Task only allow alphanumeric revs and parse `@foo@bar` as two revs
// @Task error spans
fn parse_parameter(mut source: &str, type_: TestType) -> Result<Parameter<'_>, Error> {
    let mut revisions = Vec::new();

    while let Some(stripped) = source.strip_prefix('@') {
        let mut split = stripped.splitn(2, ' ');

        let Some(revision) = split.next().filter(|revision| !revision.is_empty()) else {
            // @Task better message
            return Err(Error::new("invalid empty revision"));
        };

        revisions.push(revision);
        source = split.as_str();
    }

    let mut arguments = source.split(' ');

    let Some(parameter) = arguments.next().filter(|argument| !argument.is_empty()) else {
        // @Task better message
        return Err(Error::new("expected a test parameter"));
    };

    let missing_argument = |argument| Error::missing_argument(parameter, argument);

    let exhaust_arguments = |arguments: std::str::Split<'_, _>| {
        let count = arguments.count();
        if count != 0 {
            // @Task improve message
            Err(Error::new(format!(
                "{count} extraneous {} {} passed to ‘{parameter}’",
                pluralize!(count, "argument"),
                pluralize!(count, "is", "are")
            )))
        } else {
            Ok(())
        }
    };

    let kind = match parameter {
        "pass" | "fail" => {
            let mode = parse_mode(
                arguments.next().ok_or_else(|| missing_argument("mode"))?,
                type_,
            )?;

            exhaust_arguments(arguments)?;

            match parameter {
                "pass" => ParameterKind::Pass { mode },
                "fail" => ParameterKind::Fail { mode },
                _ => unreachable!(),
            }
        }
        "auxiliary" => ParameterKind::Auxiliary {
            users: arguments.collect(),
        },
        "ignore" => {
            exhaust_arguments(arguments)?;

            ParameterKind::Ignore
        }
        // @Task enforce that at least one arg is given
        "compiler-args" => ParameterKind::CompilerArgs(arguments.collect()),
        "compiler-env-var" => {
            let name = arguments.next().ok_or_else(|| missing_argument("name"))?;
            // @Task support spaces in the value if it's quoted
            let value = arguments.next().ok_or_else(|| missing_argument("value"))?;

            let count = arguments.count();
            if count != 0 {
                // @Task improve message
                return Err(Error::new(format!(
                    "{count} extraneous {} {} passed to ‘{parameter}’",
                    pluralize!(count, "argument"),
                    pluralize!(count, "is", "are")
                )));
            }

            ParameterKind::CompilerEnvVar { name, value }
        }
        // @Task enforce that at least one arg is given
        "program-args" => ParameterKind::ProgramArgs(arguments.collect()),
        "program-env-var" => {
            let name = arguments.next().ok_or_else(|| missing_argument("name"))?;
            // @Task support spaces in the value if it's quoted
            let value = arguments.next().ok_or_else(|| missing_argument("value"))?;

            exhaust_arguments(arguments)?;

            ParameterKind::ProgramEnvVar { name, value }
        }
        "timeout" => {
            let timeout = arguments
                .next()
                .ok_or_else(|| missing_argument("duration"))?;

            exhaust_arguments(arguments)?;

            ParameterKind::Timeout(
                timeout
                    .parse()
                    .map_err(|_| Error::new(format!("‘{timeout}’ is not a valid duration")))?,
            )
        }
        "substitution" => {
            let name = arguments.next().ok_or_else(|| missing_argument("name"))?;
            // @Task support spaces in the value if it's quoted
            let value = arguments.next().ok_or_else(|| missing_argument("value"))?;

            exhaust_arguments(arguments)?;

            ParameterKind::Substitution { name, value }
        }
        // @Question disallow empty list of revisions?
        "revisions" => ParameterKind::Revisions(arguments.collect()),
        parameter => {
            // @Task smh. (levenshtein distance maybe?) call out potential typos
            //       esp. ‘{compiler,program}-arg’,  (lacking an s),
            //            ‘{compiler,program}-env-vars’ (extraneous s),
            //            ‘substitution’ (lacking an s),
            //            ‘revision’ (lacking an s),

            return Err(Error::new(format!(
                "‘{parameter}’ is not a valid test parameter"
            )));
        }
    };

    Ok(Parameter { revisions, kind })
}

fn parse_mode(mode: &str, type_: TestType) -> Result<Mode, Error> {
    use TestType::*;

    let mode = mode
        .parse()
        .map_err(|_| Error::new(format!("‘{mode}’ is not a valid test mode")))?;

    #[allow(clippy::match_same_arms)]
    match (type_, mode) {
        (SourceFile | Package, Mode::Check | Mode::Build | Mode::Run) => {}
        (MetadataSourceFile, Mode::Check) => {}
        (MetadataSourceFile, Mode::Build | Mode::Run) => {
            return Err(Error::new(format!(
                "the test mode ‘{}’ is not available for {type_} tests",
                mode.name()
            )))
        }
    }

    Ok(mode)
}

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
struct Parameter<'src> {
    revisions: Vec<&'src str>,
    kind: ParameterKind<'src>,
}

#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
enum ParameterKind<'src> {
    Pass { mode: Mode },
    Fail { mode: Mode },
    Auxiliary { users: Vec<&'src str> },
    Ignore,
    CompilerArgs(Vec<&'src str>),
    CompilerEnvVar { name: &'src str, value: &'src str },
    ProgramArgs(Vec<&'src str>),
    ProgramEnvVar { name: &'src str, value: &'src str },
    Timeout(Timeout),
    Substitution { name: &'src str, value: &'src str },
    Revisions(Vec<&'src str>),
}

#[derive(Clone, Copy)]
enum Language {
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

impl TestType {
    fn language(self) -> Language {
        match self {
            Self::Package | Self::MetadataSourceFile => Language::Metadata,
            Self::SourceFile => Language::Lushui,
        }
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub(crate) enum TestTag<'src> {
    Pass { mode: Mode },
    Fail { mode: Mode },
    Auxiliary { users: Vec<&'src str> },
    Ignore,
}

impl Default for TestTag<'_> {
    fn default() -> Self {
        Self::Fail { mode: default() }
    }
}

#[derive(Clone, Copy, Default, FromStr, Str, PartialEq, Eq)]
#[cfg_attr(test, derive(Debug))]
#[format(dash_case)]
pub(crate) enum Mode {
    #[default]
    Check,
    Build,
    Run,
}

#[derive(Default, Clone, Copy)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub(crate) enum Timeout {
    #[default]
    Inherited,
    Overwritten(Option<Duration>),
}

impl FromStr for Timeout {
    type Err = ();

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        Ok(Self::Overwritten(match source {
            "none" => None,
            _ => Some(Duration::from_secs(source.parse().map_err(|_| ())?)),
        }))
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub(crate) struct Error {
    pub(crate) message: Str,
    // span: Span, // @Task
}

impl Error {
    fn new(message: impl Into<Str>) -> Self {
        Self {
            message: message.into(),
        }
    }

    fn missing_argument(parameter: &str, argument: &str) -> Self {
        Self::new(format!(
            "the test parameter ‘{parameter}’ is missing the argument ‘{argument}’"
        ))
    }
}
