//! Package creation.

use diagnostics::{Diagnostic, ErrorCode, Reporter};
use error::Result;
use lexer::WordExt;
use package::MANIFEST_FILE_NAME;
use std::{fs, io, path::PathBuf};
use token::Word;
use utilities::{FormatWithPathExt, FILE_EXTENSION};

const SOURCE_FOLDER_NAME: &str = "source";
const LIBRARY_FILE_STEM: &str = "library";
const EXECUTABLE_FILE_STEM: &str = "main";

pub(crate) fn create_package(
    name: &str,
    options: &PackageCreationOptions,
    reporter: &Reporter,
) -> Result {
    let name = Word::parse(name.to_owned()).map_err(|_| {
        // @Task DRY @Question is the common code justified?
        Diagnostic::error()
            .code(ErrorCode::E036)
            .message(format!("the package name ‘{name}’ is not a valid word"))
            .report(reporter)
    })?;

    || -> Result<(), Error> {
        let current_path = std::env::current_dir()?;

        let package_path = current_path.join(name.as_str());
        fs::create_dir(&package_path).with_path(package_path.clone())?;

        let source_folder_path = package_path.join(SOURCE_FOLDER_NAME);
        fs::create_dir(&source_folder_path).with_path(source_folder_path.clone())?;

        {
            let path = package_path.join(MANIFEST_FILE_NAME);
            let package_manifest = io::BufWriter::new(fs::File::create(&path).with_path(path)?);
            create_package_manifest(&name, options, package_manifest)?;
        }

        {
            let path = package_path.join(".gitignore");
            fs::write(&path, "build/\n").with_path(path)?;
        }

        if options.library {
            let path = source_folder_path
                .join(LIBRARY_FILE_STEM)
                .with_extension(FILE_EXTENSION);
            fs::File::create(&path).with_path(path)?;
        }

        if options.executable {
            let path = source_folder_path
                .join(EXECUTABLE_FILE_STEM)
                .with_extension(FILE_EXTENSION);
            let content = "main: extern.core.text.Text =\n    \"Hello there!\"";
            fs::write(&path, content).with_path(path)?;
        }

        Ok(())
    }()
    .map_err(|error| {
        Diagnostic::error()
            .message(format!("could not create package ‘{name}’"))
            .note(error.inner.format(error.path.as_deref()))
            .report(reporter)
    })
}

pub(crate) struct PackageCreationOptions {
    pub(crate) no_core: bool,
    pub(crate) library: bool,
    pub(crate) executable: bool,
}

fn create_package_manifest(
    name: &Word,
    options: &PackageCreationOptions,
    mut sink: impl io::Write,
) -> io::Result<()> {
    {
        write!(sink, "name: ")?;

        if name.as_str() == "false" || name.as_str() == "true" {
            write!(sink, r#""{name}""#)?;
        } else {
            write!(sink, "{name}")?;
        }

        writeln!(sink, ",")?;
    }

    writeln!(sink, r#"version: "0.0.0","#)?;
    writeln!(sink)?;

    writeln!(sink, "components: {{")?;

    if options.library {
        writeln!(sink, "    {name}: {{")?;
        // @Beacon @Note we might need to update this to ‘default-library’
        writeln!(sink, "        type: library,")?;
        writeln!(
            sink,
            r#"        path: "{SOURCE_FOLDER_NAME}/{LIBRARY_FILE_STEM}.lushui","#
        )?;
        writeln!(sink)?;
        writeln!(sink, "        dependencies: {{")?;
        if !options.no_core {
            writeln!(sink, "            core: {{ provider: distribution }},")?;
        }
        writeln!(sink, "        }},")?;
        writeln!(sink, "    }},")?;
    }

    if options.executable {
        let executable_name = if name.as_str() != "main" {
            "main"
        } else {
            "main_"
        };

        writeln!(sink, "    {executable_name}: {{")?;
        // @Beacon @Note we might need to update this to ‘default-executable’
        writeln!(sink, "        type: executable,")?;
        writeln!(
            sink,
            r#"        path: "{SOURCE_FOLDER_NAME}/{EXECUTABLE_FILE_STEM}.lushui","#
        )?;
        writeln!(sink)?;
        writeln!(sink, "        dependencies: {{")?;
        if options.library {
            writeln!(sink, "            {name}: {{}},")?;
        }
        if !options.no_core {
            writeln!(sink, "            core: {{ provider: distribution }},")?;
        }
        writeln!(sink, "        }},")?;
        writeln!(sink, "    }},")?;
    }

    writeln!(sink, "}},")
}

struct Error {
    inner: io::Error,
    path: Option<PathBuf>,
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self {
            inner: error,
            path: None,
        }
    }
}

trait WithPathExt<T> {
    fn with_path(self, path: PathBuf) -> Result<T, Error>;
}

impl<T> WithPathExt<T> for Result<T, std::io::Error> {
    fn with_path(self, path: PathBuf) -> Result<T, Error> {
        self.map_err(|error| Error {
            inner: error,
            path: Some(path),
        })
    }
}
