use super::{parse_parameter, Error, Mode, Parameter, ParameterKind, Timeout};
use crate::{
    configuration::{Configuration, TestTag},
    TestType::*,
};
use std::default::default;

#[test]
fn parse_parameter_program_args() {
    assert_eq!(
        Ok(Parameter {
            revisions: Vec::new(),
            kind: ParameterKind::ProgramArgs(vec!["outsourced"])
        }),
        parse_parameter("program-args outsourced", SourceFile)
    );
}

#[test]
fn parse_parameter_pass_check() {
    assert_eq!(
        Ok(Parameter {
            revisions: Vec::new(),
            kind: ParameterKind::Pass { mode: Mode::Check }
        }),
        parse_parameter("pass check", SourceFile)
    );
}

#[test]
fn parse_parameter_fail_build() {
    assert_eq!(
        Ok(Parameter {
            revisions: Vec::new(),
            kind: ParameterKind::Fail { mode: Mode::Build }
        }),
        parse_parameter("fail build", SourceFile)
    );
}

#[test]
fn parse_parameter_build_mode_unavailable_to_metadata_tests() {
    assert_eq!(
        Err(Error::new(
            "the test mode ‘build’ is not available for metadata source file tests"
        )),
        parse_parameter("pass build", MetadataSourceFile)
    );
}

#[test]
fn parse_parameter_invalid_empty_revision_0() {
    assert_eq!(
        Err(Error::new("invalid empty revision")),
        parse_parameter("@", SourceFile)
    );
}

#[test]
fn parse_parameter_invalid_empty_revision_1() {
    assert_eq!(
        Err(Error::new("invalid empty revision")),
        parse_parameter("@ ", SourceFile)
    );
}

#[test]
fn parse_parameter_revision_missing_parameter_0() {
    assert_eq!(
        Err(Error::new("expected a test parameter")),
        parse_parameter("@alpha", SourceFile)
    );
}

#[test]
fn parse_parameter_revision_missing_parameter_1() {
    assert_eq!(
        Err(Error::new("expected a test parameter")),
        parse_parameter("@thing ", SourceFile)
    );
}

#[test]
fn parse_parameter_single_revision() {
    assert_eq!(
        Ok(Parameter {
            revisions: vec!["great"],
            kind: ParameterKind::CompilerArgs(vec!["--something", "--else"])
        }),
        parse_parameter("@great compiler-args --something --else", SourceFile)
    );
}

#[test]
fn parse_parameter_non_alphanumeric_revisions() {
    assert_eq!(
        Ok(Parameter {
            revisions: vec!["!@??//x"],
            kind: ParameterKind::CompilerArgs(vec!["-Zextra"])
        }),
        parse_parameter("@!@??//x compiler-args -Zextra", SourceFile)
    );
}

#[test]
fn parse_parameter_multiple_revisions() {
    assert_eq!(
        Ok(Parameter {
            revisions: vec!["alpha", "@beta", "gamma@addendum"],
            kind: ParameterKind::Timeout(Timeout::Overwritten(None))
        }),
        parse_parameter("@alpha @@beta @gamma@addendum timeout none", SourceFile)
    );
}

#[test]
fn parse_parameter_missing_parameter() {
    assert_eq!(
        Err(Error::new("expected a test parameter")),
        parse_parameter("", SourceFile)
    );
}

#[test]
fn parse_parameter_undefined_parameter() {
    assert_eq!(
        Err(Error::new("‘weird’ is not a valid test parameter")),
        parse_parameter("weird", SourceFile)
    );
}

#[test]
fn parse_parameter_missing_argument_pass() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘pass’ is missing the argument ‘mode’"
        )),
        parse_parameter("pass", SourceFile)
    );
}

#[test]
fn parse_parameter_missing_argument_compiler_env_var_0() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘compiler-env-var’ is missing the argument ‘name’"
        )),
        parse_parameter("compiler-env-var", SourceFile)
    )
}

#[test]
fn parse_parameter_missing_argument_compiler_env_var_1() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘compiler-env-var’ is missing the argument ‘value’"
        )),
        parse_parameter("compiler-env-var BACKTRACE", SourceFile)
    )
}

#[test]
fn parse_parameter_invalid_argument() {
    assert_eq!(
        Err(Error::new("‘no’ is not a valid duration")),
        parse_parameter("timeout no", SourceFile)
    )
}

#[test]
fn parse_parameter_too_many_arguments_timeout() {
    assert_eq!(
        Err(Error::new("1 extraneous argument is passed to ‘timeout’")),
        parse_parameter("timeout 9999 none", SourceFile)
    );
}

#[test]
fn parse_parameter_too_many_arguments_pass() {
    assert_eq!(
        Err(Error::new("3 extraneous arguments are passed to ‘pass’")),
        parse_parameter("pass check -Zinternals --no-core --tlib", SourceFile)
    )
}

#[test]
fn parse_parameter_too_many_arguments_program_env_var() {
    assert_eq!(
        Err(Error::new(
            "2 extraneous arguments are passed to ‘program-env-var’"
        )),
        parse_parameter("program-env-var hey there fellow traveler", SourceFile)
    );
}

#[test]
fn parse_configuraiton_no_parameters() {
    assert_eq!(
        Ok(Configuration::default()),
        Configuration::parse("", SourceFile)
    );
}

#[test]
fn parse_configuration_single_parameter() {
    assert_eq!(
        Ok(Configuration {
            tag: TestTag::Pass { mode: Mode::Build },
            ..default()
        }),
        Configuration::parse(";;; TEST pass build", SourceFile)
    );
}

#[test]
fn parse_configuration_conflicting_parameters_0() {
    assert_eq!(
        Err(Error::new("a test tag is already set")),
        Configuration::parse(
            "
;;; TEST pass build
;;; TEST fail run
    ",
            SourceFile
        )
    );
}

#[test]
fn parse_configuration_conflicting_parameters_1() {
    assert_eq!(
        Err(Error::new("a timeout is already set")),
        Configuration::parse(
            "
;;; TEST timeout none
;;; TEST timeout 0
    ",
            SourceFile
        )
    );
}

#[test]
fn parse_configuration_accumulating_parameters() {
    assert_eq!(
        Ok(Configuration {
            compiler_args: vec!["one", "two", "three"],
            ..default()
        }),
        Configuration::parse(
            "
;;; TEST compiler-args one two
;;;
;;; TEST compiler-args three
",
            SourceFile
        )
    );
}

#[test]
fn parse_configuration_unsupported_program_args() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘program-args’ is not supported yet"
        )),
        Configuration::parse("# TEST program-args input", MetadataSourceFile),
    );
}

#[test]
fn parse_configuration_unsupported_program_env_var() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘program-env-var’ is not supported yet"
        )),
        Configuration::parse("# TEST program-env-var EXTRA STUFF!", MetadataSourceFile),
    );
}

#[test]
fn parse_configuration_unsupported_revisions_parameter() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘revisions’ is not supported yet"
        )),
        Configuration::parse(";;; TEST revisions base modif", SourceFile),
    );
}

#[test]
fn parse_configuration_unsupported_revisions_syntax() {
    assert_eq!(
        Err(Error::new("revisions are not supported yet")),
        Configuration::parse(";;; TEST @modif compiler-args -Zparse-only", SourceFile),
    );
}

#[test]
fn parse_configuration_unsupported_substitution() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘substitution’ is not supported yet"
        )),
        Configuration::parse(r#";;; TEST substitution DIR path/to/(\w+)"#, SourceFile),
    );
}

#[test]
#[ignore = "currently overshadowed by ‘revisions are not supported yet’"]
fn parse_configuration_revisions_depending_on_revisions() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘revisions’ cannot itself depend on revisions"
        )),
        Configuration::parse("# TEST @self revisions self", MetadataSourceFile)
    );
}
