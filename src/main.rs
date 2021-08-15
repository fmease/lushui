#![feature(backtrace, format_args_capture, derive_default_enum)]
#![forbid(rust_2018_idioms, unused_must_use)]

use cli::{Command, CrateType, PhaseRestriction};
use lushui::{
    crates::{
        distributed_libraries_path, Binary, BinaryManifest, CrateIndex, CrateStore,
        DependencyManifest, Library, Package, PackageManifest, CORE_LIBRARY_NAME,
        DEFAULT_BINARY_ROOT_FILE_STEM, DEFAULT_LIBRARY_ROOT_FILE_STEM, DEFAULT_SOURCE_FOLDER_NAME,
    },
    diagnostics::{
        reporter::{BufferedStderrReporter, StderrReporter},
        Diagnostic, Reporter,
    },
    error::{Outcome, Result},
    format::DisplayWith,
    lexer::Lexer,
    lowerer::Lowerer,
    parser::{ast::Identifier, Parser},
    resolver,
    span::{SourceMap, Span},
    typer::Typer,
    FILE_EXTENSION,
};
use resolver::Resolver;
use rustc_hash::FxHashMap as HashMap;
use std::{cell::RefCell, convert::TryInto, path::Path, rc::Rc};

mod cli;

type Str = std::borrow::Cow<'static, str>;

fn main() {
    if main_().is_err() {
        // all destructors have been run
        std::process::exit(1);
    }
}

fn main_() -> Result<(), ()> {
    set_panic_hook();

    let (command, options) = cli::arguments();

    // @Task get rid of this!
    lushui::set_global_options(lushui::GlobalOptions {
        show_binding_indices: options.show_binding_indices,
    });

    let map = Rc::new(RefCell::new(SourceMap::default()));
    let reporter = BufferedStderrReporter::new(map.clone()).into();

    let result = execute_command(command, options, &map, &reporter);

    let reporter = match reporter {
        Reporter::BufferedStderr(reporter) => reporter,
        _ => unreachable!(),
    };

    let number_of_errors_reported = reporter.release_buffer();

    if number_of_errors_reported > 0 || result.is_err() {
        assert!(
            number_of_errors_reported > 0,
            "some errors occurred but none were reported",
        );

        return Err(());
    }

    Ok(())
}

fn execute_command(
    command: Command,
    options: cli::Options,
    map: &Rc<RefCell<SourceMap>>,
    reporter: &Reporter,
) -> Result {
    use cli::GenerationMode;
    use Command::*;

    match command {
        Check | Run | Build => check_run_or_build_package(command, options, map, reporter),
        Explain => todo!(),
        Generate { mode, crate_type } => match mode {
            GenerationMode::Initialize => todo!(),
            GenerationMode::New { package_name } => {
                create_new_package(package_name, crate_type, options, reporter)
            }
        },
    }
}

// @Temporary location, concept
// @Note inept name: package <-> crate
#[derive(Default)]
pub struct CrateQueue(Vec<Package>);

impl CrateQueue {
    // @Note not scalable
    pub fn find_by_path(&self, path: &Path) -> Option<CrateIndex> {
        self.0
            .iter()
            .find(|package| package.path == path)
            .map(|package| package.index)
    }

    pub fn enqueue(&mut self, package: impl FnOnce(CrateIndex) -> Package) {
        let index = CrateIndex(self.0.len().try_into().unwrap());

        self.0.push(package(index));
    }
}

fn check_run_or_build_package(
    command: Command,
    options: cli::Options,
    map: &Rc<RefCell<SourceMap>>,
    reporter: &Reporter,
) -> Result {
    // @Temporary architecture
    let mut built_crates = CrateStore::default();
    let mut unbuilt_crates = CrateQueue::default();

    let mut single_file_crate_resolved_dependencies = HashMap::default();

    if options.source_file_path.is_some() && !options.unlink_core {
        let core_library_path = distributed_libraries_path().join(CORE_LIBRARY_NAME);

        // @Question custom message for not finding the core library?
        let manifest = PackageManifest::open(&core_library_path, &reporter)?;

        unbuilt_crates.enqueue(|index| {
            single_file_crate_resolved_dependencies.insert(CORE_LIBRARY_NAME.into(), index);

            Package::from_manifest(
                false,
                index,
                core_library_path,
                // @Bug do not hardcode core's dependencies: use the resolve_dependencies function here
                // no special-casing!!
                HashMap::default(),
                manifest,
            )
        });
    }

    if options.source_file_path.is_none() && options.unlink_core {
        // @Temporary message
        // @Beacon @Question does curl support this in a built-in way?:
        // @Task add note explaining how one needs to remove the
        // explicit `core` dep in the manifest to achieve the wanted behavior
        Diagnostic::error()
            .message("option --unlink-core only works with explicit source file paths")
            .report(&reporter);
    }

    match options.source_file_path {
        // @Note does not scale to `--link`s
        Some(source_file_path) => {
            let crate_name = lushui::lexer::parse_crate_name(source_file_path.clone(), &reporter)
                .map_err(|error| error.report(&reporter))?;

            // @Bug probably crashes on `lushui check $CARGO_MANIFEST_DIR/libraries/core/source/library.lushui`
            unbuilt_crates.enqueue(|index| {
                Package::from_manifest(
                    true,
                    index,
                    // @Task dont unwrap, handle error case
                    // joining with "." since it might return "" which fails to canonicalize
                    source_file_path
                        .parent()
                        .unwrap()
                        .join(".")
                        .canonicalize()
                        .unwrap(),
                    single_file_crate_resolved_dependencies,
                    // @Task create a PackageManifest::single_file_package constructor for this
                    PackageManifest {
                        // @Task we can do better here
                        name: crate_name.as_str().to_owned(),
                        version: "0.0.0".to_owned(),
                        description: String::new(),
                        private: true,
                        // @Bug we don't want to signal "check for yourself if there exists source/library.lushui" with None
                        // but to explicitly set the library to None
                        // meaning Manifest is not the correct type we want here!!
                        library: None,
                        binary: Some(BinaryManifest {
                            path: Some(source_file_path),
                        }),
                        // @Beacon @Bug missing `core` (unless --unlink-core)
                        dependencies: HashMap::default(),
                    },
                )
            });
        }
        None => {
            // @Beacon @Note even though we use recursion, we have duplicate code here, we need to
            // have a proper (simpler!) base case! only call enqueue_dependency once here in this block
            // not twice!

            // @Task dont unwrap, handle the error cases
            let path = std::env::current_dir().unwrap();
            let manifest = PackageManifest::open(&path, &reporter)?;

            let resolved_dependencies =
                enqueue_dependencies(&mut unbuilt_crates, &path, &manifest.dependencies, reporter)?;

            unbuilt_crates.enqueue(|index| {
                Package::from_manifest(true, index, path, resolved_dependencies, manifest)
            });

            // @Temporary
            type ResolvedDependencies = HashMap<String, CrateIndex>;

            // @Temporary location
            // @Question are some errors non-fatal??
            fn enqueue_dependencies(
                unbuilt_crates: &mut CrateQueue,
                project_path: &Path,
                dependencies: &HashMap<String, DependencyManifest>,
                reporter: &Reporter,
            ) -> Result<ResolvedDependencies> {
                let mut resolved_dependencies = ResolvedDependencies::default();

                // @Note bad names
                for (name, dependency_manifest) in dependencies {
                    // @Beacon @Task also incorporate dep_mani.name

                    // @Note not sure whether we can move DepManif validation to mod crates

                    let path = match &dependency_manifest.path {
                        // @Task handle error
                        Some(path) => project_path.join(path).canonicalize().unwrap(),
                        // @Beacon @Task we need to emit a custom diagnostic if stuff cannot be
                        // found in the distributies libraries path
                        None => distributed_libraries_path().join(name),
                    };

                    if let Some(index) = unbuilt_crates.find_by_path(&path) {
                        resolved_dependencies.insert(name.clone(), index);
                        continue;
                    }

                    // @Task verify name and version matches (unless overwritten!)
                    // @Task don't return early here!
                    // @Task don't bubble up with `?` but once `open` returns a proper error type,
                    // report a custom diagnostic saying ~ "could not find a package manifest for the package XY
                    // specified as a path dependency" (sth sim to this)
                    let manifest = PackageManifest::open(&path, &reporter)?;

                    // @Task handle circular deps! (currently loops indefinitly I guess)
                    let resolved_transitive_dependencies = enqueue_dependencies(
                        unbuilt_crates,
                        &path,
                        &manifest.dependencies,
                        reporter,
                    )?;

                    // @Note we probably need to disallow referencing the same package through different
                    // names from the same package to be able to generate a correct lock-file
                    unbuilt_crates.enqueue(|index| {
                        resolved_dependencies.insert(name.clone(), index);

                        Package::from_manifest(
                            false,
                            index,
                            path,
                            resolved_transitive_dependencies,
                            manifest,
                        )
                    });
                }

                Ok(resolved_dependencies)
            }
        }
    };

    // @Beacon @Temporary
    // eprintln!(
    //     "crates to build: {}",
    //     unbuilt_crates
    //         .0
    //         .iter()
    //         .map(|crate_| format!("{} [{:?}]\n", crate_.name, crate_.path))
    //         .collect::<String>()
    // );

    // @Task forall crates in dependencies "compile"/… them
    // and in the end crates.add() them
    for mut crate_ in unbuilt_crates.0 {
        // @Beacon @Beacon @Task create CLI-flag -q/--quiet
        // and don't print this if it is set
        // eprintln!(
        //     "building crate `{}` ({:?})",
        //     crate_.name, crate_.scope.owner
        // ); // @Temporary

        // @Temporary
        let source_file_path = match (&crate_.library, &crate_.binary) {
            (None, None) => panic!(),
            (None, Some(Binary { path })) | (Some(Library { path }), None) => path,
            (Some(_), Some(_)) => todo!(),
        };

        let source_file = map
            .borrow_mut()
            .load(source_file_path.clone())
            .map_err(|error| Diagnostic::from(error).report(&reporter))?;

        let Outcome {
            value: tokens,
            health,
        } = Lexer::new(map.borrow().get(source_file), &reporter).lex()?;

        {
            if options.dump.tokens {
                for token in &tokens {
                    eprintln!("{:?}", token);
                }
            }
            if options.phase_restriction == Some(PhaseRestriction::Lexer) {
                if health.is_tainted() {
                    return Err(());
                }
                return Ok(());
            }
        }

        let declaration = Parser::new(source_file, &tokens, map.clone(), &reporter)
            .parse(Identifier::new(crate_.name.clone().into(), Span::SHAM))?;
        if health.is_tainted() {
            return Err(());
        }
        {
            if options.dump.ast {
                eprintln!("{declaration:#?}");
            }
            if options.phase_restriction == Some(PhaseRestriction::Parser) {
                return Ok(());
            }
        }

        let Outcome {
            value: mut declarations,
            health,
        } = Lowerer::new(map.clone(), &reporter).lower_declaration(declaration);

        if health.is_tainted() {
            return Err(());
        }

        let declaration = declarations.pop().unwrap();

        {
            if options.dump.lowered_ast {
                eprintln!("{}", declaration);
            }
            if options.phase_restriction == Some(PhaseRestriction::Lowerer) {
                return Ok(());
            }
        }

        let mut resolver = Resolver::new(
            &mut crate_.scope,
            &crate_.metadata,
            &built_crates,
            &reporter,
        );
        let declaration = resolver.resolve_declaration(declaration)?;

        {
            if options.dump.hir {
                eprintln!("{}", declaration.with((&crate_.scope, &built_crates)));
            }
            if options.dump.untyped_scope {
                eprintln!("{}", crate_.scope.with(&built_crates));
            }
            if options.phase_restriction == Some(PhaseRestriction::Resolver) {
                return Ok(());
            }
        }

        // @Beacon @Temporary only in `core`
        crate_.scope.register_foreign_bindings();

        let mut typer = Typer::new(&mut crate_.scope, &built_crates, &reporter);
        typer.infer_types_in_declaration(&declaration)?;

        {
            if options.dump.scope {
                eprintln!("{}", typer.scope.with(&built_crates));
            }
        }

        // @Beacon @Task dont check for program_entry in scope.run() or compile_and_interp
        // but here (a static error) (if the CLI dictates to run it)

        if let Command::Run = command {
            if crate_.is_main {
                let result = typer.interpreter().run()?;

                println!("{}", result.with((&crate_.scope, &built_crates)));
            }
        }
        // @Temporary
        else if let Command::Build = command {
            // @Temporary not just builds, also runs ^^

            lushui::compiler::compile_and_interpret_declaration(&declaration, &crate_.scope)
                .unwrap_or_else(|_| panic!());
        }

        // @Note awkward: crate_.scope.owner
        built_crates.add(crate_.scope.owner, crate_);
    }

    Ok(())
}

// @Task initialize git repository (unless `--vsc=none` or similar)
fn create_new_package(
    name: String,
    crate_type: CrateType,
    _options: cli::Options,
    _reporter: &Reporter,
) -> Result {
    use std::fs;

    // @Task verify name is a valid crate name

    // @Task handle errors properly
    let current_path = std::env::current_dir().unwrap();
    let package_path = current_path.join(&name);
    fs::create_dir(&package_path).unwrap();
    let source_folder_path = package_path.join(DEFAULT_SOURCE_FOLDER_NAME);
    fs::create_dir(&source_folder_path).unwrap();
    fs::write(
        package_path.join(PackageManifest::FILE_NAME),
        format!(
            "\
{{
    name: '{name}',
    version: '0.0.0',
    dependencies: {{
        core: {{}},
    }},
}}"
        ),
    )
    .unwrap();

    let root_file_path = source_folder_path
        .join(match crate_type {
            CrateType::Binary => DEFAULT_BINARY_ROOT_FILE_STEM,
            CrateType::Library => DEFAULT_LIBRARY_ROOT_FILE_STEM,
        })
        .with_extension(FILE_EXTENSION);

    let root_file_content = match crate_type {
        CrateType::Binary => "main: crates.core.text.Text =\n    \"hello there!\"",
        CrateType::Library => "",
    };

    fs::write(root_file_path, root_file_content).unwrap();

    Ok(())
}

fn set_panic_hook() {
    std::panic::set_hook(Box::new(|information| {
        let payload = information.payload();

        let message = payload
            .downcast_ref::<&str>()
            .copied()
            .or_else(|| payload.downcast_ref::<String>().map(|payload| &payload[..]))
            .unwrap_or_else(|| "unknown cause")
            .to_owned();

        let backtrace = std::backtrace::Backtrace::force_capture();

        Diagnostic::bug()
            .message(message)
            .when_present(information.location(), |this, location| {
                this.note(format!("at `{location}`"))
            })
            .note(
                std::thread::current()
                    .name()
                    .map(|name| format!("in thread `{name}`").into())
                    .unwrap_or(Str::from("in an unnamed thread")),
            )
            .note(format!("with the following backtrace:\n{backtrace}"))
            .report(&StderrReporter::new(None).into());
    }));
}
