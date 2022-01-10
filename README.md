# The Lushui Programming Language

**露水 (lushui)**: _dew; ephemeral, transient, of short duration_.

The reference compiler of the [Lushui programming language](https://lushui.ml/).  
Its code is hosted on [GitHub](https://github.com/fmease/lushui.git) and mirrored on [GitLab](https://gitlab.com/fmease/lushui.git).

## Stability

This project is under heavy development and not recommended for use outside of experiments.
The language, the compiler API and its CLI have no stability guarantees whatsoever. Anything may change in a new version without notice. The majority of the language has not been implemented yet in the compiler and the latter contains quite a lot of bugs and the test suites are not big enough yet.

## Installation

The compiler is mainly written in [Rust](https://www.rust-lang.org/) and it uses Rust's official package manager [Cargo](https://doc.rust-lang.org/cargo/).
We require the (latest) _nightly_ version of the Rust compiler `rustc` (with the `rustup`, just do `rustup install nightly`).

To build the Lushui compiler, run:

```sh
cargo +nightly build
```

To show the help text on Linux, execute:

```sh
./lushui -h
```

On Windows, use `cargo +nightly run -- -h`.

Currently, the shell script `./lushui` also rebuilds the compiler if necessary (no status info shown though during building) and runs it afterwards.

## Tests

Run the following command (Linux only) to run all test suites:

```sh
./test/run
```

If you want to restrict yourself to UI tests, execute `./test/ui/run`.

## Tooling

No IDE is available yet. However basic syntax highlighting is available for Visual Studio Code as an extension. To install it, copy or symlink the folder `./misc/vscode/fmease.lushui-syntax-0.0.1/` to `~/.vscode/extensions/` (Linux). For package metadata files, we provide a VS Code extension, too, separately at `./misc/vscode/fmease.lushui-metadata-syntax-0.0.1`.

## Generated API Documentation

As hinted in the help text (`./lushui -h`), you use `lushui doc` (and variations) to generate (HTML) documentation.
To view it, you currently need to manually do `<browser> build/doc/<capsule>/index.html`. In the future, you will be able
to just pass `--open` to `./lushui doc`.

By default, documentation comments are treated as plain text. However, the goal is to make AsciiDoc the standard markup language. Today, this is only opt-in via the _unstable_ option `-Z asciidoc` which requires [Asciidoctor](https://asciidoctor.org/) to be installed and available as `asciidoctor` (a custom installation path is not supported at the moment).

## License

None yet.
