use super::{Error, Mode, Param, ParameterKind, Timeout};
use crate::{
    configuration::{Configuration, TestTag},
    TestType::{self, *},
};
use span::{span, FileName, SourceMap, Spanned};
use utility::default;

fn parse_configuration<'m>(
    source: &'static str,
    ty: TestType,
    map: &'m mut SourceMap,
) -> Result<Configuration<'m>, Error> {
    let file = map.add_str(FileName::Anon, source);
    Configuration::parse(&map[file], ty, map)
}

// @Temporary
fn parse_parameter(source: &str, ty: TestType) -> Result<Param<'_>, Error> {
    super::parse_parameter(Spanned::bare(source), ty)
}

#[test]
fn parse_parameter_program_args() {
    assert_eq!(
        Ok(Param {
            revisions: Vec::new(),
            kind: ParameterKind::ProgramArgs(vec!["outsourced"])
        }),
        parse_parameter("program-args outsourced", SourceFile)
    );
}

#[test]
fn parse_parameter_pass_check() {
    assert_eq!(
        Ok(Param {
            revisions: Vec::new(),
            kind: ParameterKind::Pass { mode: Mode::Check }
        }),
        parse_parameter("pass check", SourceFile)
    );
}

#[test]
fn parse_parameter_fail_build() {
    assert_eq!(
        Ok(Param {
            revisions: Vec::new(),
            kind: ParameterKind::Fail { mode: Mode::Build }
        }),
        parse_parameter("fail build", SourceFile)
    );
}

#[test]
fn parse_parameter_build_mode_unavailable_to_recnot_tests() {
    assert_eq!(
        Err(Error::new(
            "the test mode ‘build’ is not available for Recnot source file tests",
            span(0, 0)
        )),
        parse_parameter("pass build", RecnotSourceFile)
    );
}

#[test]
fn parse_parameter_invalid_empty_revision_0() {
    assert_eq!(
        Err(Error::new("invalid empty revision", span(0, 0))),
        parse_parameter("@", SourceFile)
    );
}

#[test]
fn parse_parameter_invalid_empty_revision_1() {
    assert_eq!(
        Err(Error::new("invalid empty revision", span(0, 0))),
        parse_parameter("@ ", SourceFile)
    );
}

#[test]
fn parse_parameter_revision_missing_parameter_0() {
    assert_eq!(
        Err(Error::new("expected a test parameter", span(0, 0))),
        parse_parameter("@alpha", SourceFile)
    );
}

#[test]
fn parse_parameter_revision_missing_parameter_1() {
    assert_eq!(
        Err(Error::new("expected a test parameter", span(0, 0))),
        parse_parameter("@thing ", SourceFile)
    );
}

#[test]
fn parse_parameter_single_revision() {
    assert_eq!(
        Ok(Param {
            revisions: vec!["great"],
            kind: ParameterKind::CompilerArgs(vec!["--something", "--else"])
        }),
        parse_parameter("@great compiler-args --something --else", SourceFile)
    );
}

#[test]
fn parse_parameter_non_alphanumeric_revisions() {
    assert_eq!(
        Ok(Param {
            revisions: vec!["!@??//x"],
            kind: ParameterKind::CompilerArgs(vec!["-Zextra"])
        }),
        parse_parameter("@!@??//x compiler-args -Zextra", SourceFile)
    );
}

#[test]
fn parse_parameter_multiple_revisions() {
    assert_eq!(
        Ok(Param {
            revisions: vec!["alpha", "@beta", "gamma@addendum"],
            kind: ParameterKind::Timeout(Timeout::Overwritten(None))
        }),
        parse_parameter("@alpha @@beta @gamma@addendum timeout none", SourceFile)
    );
}

#[test]
fn parse_parameter_missing_parameter() {
    assert_eq!(
        Err(Error::new("expected a test parameter", span(0, 0))),
        parse_parameter("", SourceFile)
    );
}

#[test]
fn parse_parameter_undefined_parameter() {
    assert_eq!(
        Err(Error::new(
            "‘weird’ is not a valid test parameter",
            span(0, 0)
        )),
        parse_parameter("weird", SourceFile)
    );
}

#[test]
fn parse_parameter_missing_argument_pass() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘pass’ is missing the argument ‘mode’",
            span(0, 0)
        )),
        parse_parameter("pass", SourceFile)
    );
}

#[test]
fn parse_parameter_missing_argument_compiler_env_var_0() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘compiler-env-var’ is missing the argument ‘name’",
            span(0, 0)
        )),
        parse_parameter("compiler-env-var", SourceFile)
    );
}

#[test]
fn parse_parameter_missing_argument_compiler_env_var_1() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘compiler-env-var’ is missing the argument ‘value’",
            span(0, 0)
        )),
        parse_parameter("compiler-env-var BACKTRACE", SourceFile)
    );
}

#[test]
fn parse_parameter_invalid_argument() {
    assert_eq!(
        Err(Error::new("‘no’ is not a valid duration", span(0, 0))),
        parse_parameter("timeout no", SourceFile)
    );
}

#[test]
fn parse_parameter_too_many_arguments_timeout() {
    assert_eq!(
        Err(Error::new(
            "1 extraneous argument is passed to ‘timeout’",
            span(0, 0)
        )),
        parse_parameter("timeout 9999 none", SourceFile)
    );
}

#[test]
fn parse_parameter_too_many_arguments_pass() {
    assert_eq!(
        Err(Error::new(
            "3 extraneous arguments are passed to ‘pass’",
            span(0, 0)
        )),
        parse_parameter("pass check -Zinternals --no-core --tlib", SourceFile)
    );
}

#[test]
fn parse_parameter_too_many_arguments_program_env_var() {
    assert_eq!(
        Err(Error::new(
            "2 extraneous arguments are passed to ‘program-env-var’",
            span(0, 0)
        )),
        parse_parameter("program-env-var hey there fellow traveler", SourceFile)
    );
}

#[test]
fn parse_configuraiton_no_parameters() {
    assert_eq!(
        Ok(Configuration::default()),
        parse_configuration("", SourceFile, &mut SourceMap::default())
    );
}

#[test]
fn parse_configuration_single_parameter() {
    assert_eq!(
        Ok(Configuration {
            tag: TestTag::Pass { mode: Mode::Build },
            ..default()
        }),
        parse_configuration(";;; TEST pass build", SourceFile, &mut SourceMap::default())
    );
}

#[test]
fn parse_configuration_conflicting_parameters_0() {
    assert_eq!(
        Err(Error::new("a test tag is already set", span(22, 39))),
        parse_configuration(
            "
;;; TEST pass build
;;; TEST fail run
    ",
            SourceFile,
            &mut SourceMap::default()
        )
    );
}

#[test]
fn parse_configuration_conflicting_parameters_1() {
    assert_eq!(
        Err(Error::new("a timeout is already set", span(24, 42))),
        parse_configuration(
            "
;;; TEST timeout none
;;; TEST timeout 0
    ",
            SourceFile,
            &mut SourceMap::default()
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
        parse_configuration(
            "
;;; TEST compiler-args one two
;;;
;;; TEST compiler-args three
",
            SourceFile,
            &mut SourceMap::default()
        )
    );
}

#[test]
fn parse_configuration_unsupported_program_args() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘program-args’ is not supported yet",
            span(1, 26)
        )),
        parse_configuration(
            "# TEST program-args input",
            RecnotSourceFile,
            &mut SourceMap::default()
        ),
    );
}

#[test]
fn parse_configuration_unsupported_program_env_var() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘program-env-var’ is not supported yet",
            span(1, 36)
        )),
        parse_configuration(
            "# TEST program-env-var EXTRA STUFF!",
            RecnotSourceFile,
            &mut SourceMap::default()
        ),
    );
}

#[test]
fn parse_configuration_unsupported_revisions_parameter() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘revisions’ is not supported yet",
            span(1, 30)
        )),
        parse_configuration(
            ";;; TEST revisions base modif",
            SourceFile,
            &mut SourceMap::default()
        ),
    );
}

#[test]
fn parse_configuration_unsupported_revisions_syntax() {
    assert_eq!(
        Err(Error::new("revisions are not supported yet", span(1, 43))),
        parse_configuration(
            ";;; TEST @modif compiler-args -Zparse-only",
            SourceFile,
            &mut SourceMap::default()
        ),
    );
}

#[test]
fn parse_configuration_unsupported_substitution() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘substitution’ is not supported yet",
            span(1, 40)
        )),
        parse_configuration(
            r";;; TEST substitution DIR path/to/(\w+)",
            SourceFile,
            &mut SourceMap::default()
        ),
    );
}

#[test]
#[ignore = "currently overshadowed by ‘revisions are not supported yet’"]
fn parse_configuration_revisions_depending_on_revisions() {
    assert_eq!(
        Err(Error::new(
            "the test parameter ‘revisions’ cannot itself depend on revisions",
            span(0, 0)
        )),
        parse_configuration(
            "# TEST @self revisions self",
            RecnotSourceFile,
            &mut SourceMap::default()
        )
    );
}

#[test]
fn parse_configuration_end_of_line_source_file() {
    assert_eq!(
        Ok(Configuration {
            tag: TestTag::Pass { mode: Mode::Check },
            ..default()
        }),
        parse_configuration(
            "some filler content ;;; TEST pass check",
            SourceFile,
            &mut SourceMap::default()
        )
    );
}

#[test]
fn parse_configuration_end_of_line_source_file_invalid() {
    assert_eq!(
        Err(Error::new(
            "‘undefined’ is not a valid test parameter",
            span(14, 32)
        )),
        parse_configuration(
            "Int -> Int32 ;;; TEST undefined",
            SourceFile,
            &mut SourceMap::default()
        )
    );
}

#[test]
fn do_not_parse_configuration_inside_text_literal() {
    assert_eq!(
        Ok(Configuration::default()),
        parse_configuration(
            r#"constant: Text = "
;;; TEST trigger?""#,
            SourceFile,
            &mut SourceMap::default()
        ),
    );
}

#[test]
fn parse_configuration_end_of_line_recnot_source_file() {
    assert_eq!(
        Ok(Configuration {
            tag: TestTag::Pass { mode: Mode::Check },
            ..default()
        }),
        parse_configuration(
            "some filler content # TEST pass check",
            RecnotSourceFile,
            &mut SourceMap::default()
        )
    );
}

#[test]
fn parse_configuration_end_of_line_recnot_source_file_invalid() {
    assert_eq!(
        Err(Error::new(
            "‘unknown’ is not a valid test parameter",
            span(19, 33)
        )),
        parse_configuration(
            r#"{ left: "right" } # TEST unknown"#,
            RecnotSourceFile,
            &mut SourceMap::default()
        )
    );
}

#[test]
fn do_not_parse_configuration_inside_recnot_text() {
    assert_eq!(
        Ok(Configuration::default()),
        parse_configuration(
            r#"snippet: "
# TEST trigger?","#,
            RecnotSourceFile,
            &mut SourceMap::default()
        ),
    );
}
