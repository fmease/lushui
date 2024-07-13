//! Package creation.

use diagnostics::{error::Result, Diag, Reporter};
use lexer::word::Word;
use session::package::ManifestPath;
use std::{fs, io, path::PathBuf};
use utility::{Atom, FormatError, FILE_EXTENSION};

const SOURCE_FOLDER_NAME: &str = "source";
const LIBRARY_FILE_STEM: &str = "library";
const EXECUTABLE_FILE_STEM: &str = "main";

pub(crate) fn create_pkg(name: Word, opts: &PackageCreationOptions, rep: &Reporter) -> Result {
    if let Err(error) = create(name, opts) {
        return Err(Diag::error()
            .message(format!("could not create package ‘{name}’"))
            .with(|it| match &error.path {
                Some(path) => it.path(path.clone()),
                None => it,
            })
            .note(error.inner.format())
            .report(rep));
    }

    Ok(())
}

fn create(name: Word, opts: &PackageCreationOptions) -> Result<(), Error> {
    let current_path = std::env::current_dir()?;

    let pkg_path = current_path.join(name.to_str());
    fs::create_dir(&pkg_path).with_path(pkg_path.clone())?;

    let src_folder_path = pkg_path.join(SOURCE_FOLDER_NAME);
    fs::create_dir(&src_folder_path).with_path(src_folder_path.clone())?;

    {
        let path = pkg_path.join(ManifestPath::FILE_NAME);
        let pkg_manifest = io::BufWriter::new(fs::File::create(&path).with_path(path)?);
        create_pkg_manifest(name, opts, pkg_manifest)?;
    }

    {
        let path = pkg_path.join(".gitignore");
        fs::write(&path, "build/\n").with_path(path)?;
    }

    if opts.library {
        let path = src_folder_path
            .join(LIBRARY_FILE_STEM)
            .with_extension(FILE_EXTENSION);
        fs::File::create(&path).with_path(path)?;
    }

    if opts.executable {
        let path = src_folder_path
            .join(EXECUTABLE_FILE_STEM)
            .with_extension(FILE_EXTENSION);
        let content = "main: extern.core.text.Text =\n    \"Hello there!\"";
        fs::write(&path, content).with_path(path)?;
    }

    Ok(())
}

pub(crate) struct PackageCreationOptions {
    pub(crate) no_core: bool,
    pub(crate) library: bool,
    pub(crate) executable: bool,
}

fn create_pkg_manifest(
    name: Word,
    opt: &PackageCreationOptions,
    mut sink: impl io::Write,
) -> io::Result<()> {
    {
        write!(sink, "name: ")?;

        if let Atom::FALSE | Atom::TRUE = name.into_inner() {
            write!(sink, r#""{name}""#)?;
        } else {
            write!(sink, "{name}")?;
        }

        writeln!(sink, ",")?;
    }

    writeln!(sink, r#"version: "0.0.0","#)?;
    writeln!(sink)?;

    writeln!(sink, "components: {{")?;

    if opt.library {
        writeln!(sink, "    {name}: {{")?;
        // @Beacon @Note we might need to update this to ‘default-library’
        writeln!(sink, "        type: library,")?;
        writeln!(
            sink,
            r#"        path: "{SOURCE_FOLDER_NAME}/{LIBRARY_FILE_STEM}.lushui","#
        )?;
        writeln!(sink)?;
        writeln!(sink, "        dependencies: {{")?;
        if !opt.no_core {
            writeln!(sink, "            core: {{ provider: distribution }},")?;
        }
        writeln!(sink, "        }},")?;
        writeln!(sink, "    }},")?;
    }

    if opt.executable {
        let executable_name = if name.into_inner() != Atom::MAIN {
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
        if opt.library {
            writeln!(sink, "            {name}: {{}},")?;
        }
        if !opt.no_core {
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
