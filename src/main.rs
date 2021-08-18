#![feature(backtrace, format_args_capture, derive_default_enum, decl_macro)]
#![forbid(rust_2018_idioms, unused_must_use)]

use cli::{Command, PhaseRestriction};
use lushui::{
    crates::{
        distributed_libraries_path, find_package_path, Binary, BinaryManifest, CrateIndex,
        CrateRole, CrateStore, CrateType, DependencyManifest, Library, Package, PackageManifest,
        CORE_LIBRARY_NAME, DEFAULT_SOURCE_FOLDER_NAME,
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
    span::{SharedSourceMap, SourceMap, Span},
    typer::Typer,
    FILE_EXTENSION,
};
use resolver::Resolver;
use rustc_hash::FxHashMap as HashMap;
use std::{
    convert::TryInto,
    path::{Path, PathBuf},
};
use util::{obtain, Str};

mod cli;
mod util;

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

    let map = SourceMap::shared();
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
    map: &SharedSourceMap,
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

#[derive(Default)]
// @Note it's an IndexVec
pub struct CrateQueue2(Vec<QueueItem>);

impl CrateQueue2 {
    fn available_index(&self) -> CrateIndex {
        CrateIndex(self.0.len().try_into().unwrap())
    }

    pub fn find_by_path(&self, path: &Path) -> Option<CrateIndex> {
        self.0
            .iter()
            .enumerate()
            .find(|(_, item)| item.path() == path)
            .map(|(index, _)| CrateIndex(index.try_into().unwrap()))
    }

    pub fn enqueue_unresolved(&mut self, path: PathBuf) -> CrateIndex {
        let index = self.available_index();
        self.0.push(QueueItem::Unresolved(path));
        index
    }

    pub fn enqueue_resolved(&mut self, package: impl FnOnce(CrateIndex) -> Package) {
        self.0
            .push(QueueItem::Resolved(package(self.available_index())));
    }
}

pub enum QueueItem {
    Unresolved(PathBuf),
    Resolved(Package),
}

impl QueueItem {
    pub fn path(&self) -> &Path {
        match self {
            Self::Unresolved(path) => path,
            Self::Resolved(package) => &package.path,
        }
    }

    pub fn resolve(&mut self, package: Package) {
        match self {
            Self::Unresolved(_) => *self = QueueItem::Resolved(package),
            Self::Resolved(_) => unreachable!(),
        }
    }

    pub fn into_resolved(self) -> Option<Package> {
        obtain!(self, Self::Resolved(package) => package)
    }
}

impl std::ops::Index<CrateIndex> for CrateQueue2 {
    type Output = QueueItem;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl std::ops::IndexMut<CrateIndex> for CrateQueue2 {
    fn index_mut(&mut self, index: CrateIndex) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

// @Temporary
type ResolvedDependencies = HashMap<String, CrateIndex>;

fn check_run_or_build_package(
    command: Command,
    options: cli::Options,
    map: &SharedSourceMap,
    reporter: &Reporter,
) -> Result {
    let mut built_crates = CrateStore::default();
    let mut unbuilt_crates = CrateQueue2::default();

    match options.source_file_path {
        // @Note does not scale to `--link`s
        Some(source_file_path) => {
            let crate_name = lushui::lexer::parse_crate_name(source_file_path.clone(), &reporter)
                .map_err(|error| error.report(&reporter))?;

            // @Task dont unwrap, handle error case
            // joining with "." since it might return "" which fails to canonicalize
            let path = source_file_path
                .parent()
                .unwrap()
                .join(".")
                .canonicalize()
                .unwrap();

            let index = unbuilt_crates.enqueue_unresolved(path.clone());

            let mut resolved_dependencies = ResolvedDependencies::default();

            if !options.unlink_core {
                let core_library_path = distributed_libraries_path().join(CORE_LIBRARY_NAME);

                // @Question custom message for not finding the core library?
                let manifest = PackageManifest::from_package_path(&core_library_path, &reporter)?;

                unbuilt_crates.enqueue_resolved(|index| {
                    resolved_dependencies.insert(CORE_LIBRARY_NAME.to_string(), index);

                    Package::from_manifest(
                        CrateRole::Dependency,
                        index,
                        core_library_path,
                        // @Bug do not hardcode core's dependencies: use the resolve_dependencies function here
                        // no special-casing!!
                        HashMap::default(),
                        manifest,
                    )
                });
            }

            unbuilt_crates[index].resolve(Package::from_manifest(
                CrateRole::Main,
                index,
                path,
                resolved_dependencies,
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
            ));
        }
        None => {
            if options.unlink_core {
                // @Temporary message
                // @Beacon @Question does curl support this in a built-in way?:
                // @Task add note explaining how one needs to remove the
                // explicit `core` dep in the manifest to achieve the wanted behavior
                Diagnostic::error()
                    .message("option --unlink-core only works with explicit source file paths")
                    .report(&reporter);
            }

            // @Task dont unwrap, handle the error cases
            let path = std::env::current_dir().unwrap();
            let path = find_package_path(&path).unwrap();

            // @Task verify name and version matches (unless overwritten!)
            // @Task don't return early here!
            // @Task don't bubble up with `?` but once `open` returns a proper error type,
            // report a custom diagnostic saying ~ "could not find a package manifest for the package XY
            // specified as a path dependency" (sth sim to this)
            let manifest = PackageManifest::from_package_path(&path, &reporter)?;

            let index = unbuilt_crates.enqueue_unresolved(path.into());

            let resolved_dependencies =
                enqueue_dependencies(&mut unbuilt_crates, &path, &manifest.dependencies, reporter)?;

            // @Note we probably need to disallow referencing the same package through different
            // names from the same package to be able to generate a correct lock-file
            unbuilt_crates[index].resolve(Package::from_manifest(
                CrateRole::Main,
                index,
                path.into(),
                resolved_dependencies,
                manifest,
            ));

            // @Temporary location
            // @Question are some errors non-fatal??
            fn enqueue_dependencies(
                unbuilt_crates: &mut CrateQueue2,
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
                        if matches!(unbuilt_crates[index], QueueItem::Unresolved(_)) {
                            panic!("circular crates");
                        }

                        resolved_dependencies.insert(name.clone(), index);
                        continue;
                    }

                    // @Task verify name and version matches (unless overwritten!)
                    // @Task don't return early here!
                    // @Task don't bubble up with `?` but once `open` returns a proper error type,
                    // report a custom diagnostic saying ~ "could not find a package manifest for the package XY
                    // specified as a path dependency" (sth sim to this)
                    let manifest = PackageManifest::from_package_path(&path, &reporter)?;

                    let index = unbuilt_crates.enqueue_unresolved(path.clone());

                    let resolved_transitive_dependencies = enqueue_dependencies(
                        unbuilt_crates,
                        &path,
                        &manifest.dependencies,
                        reporter,
                    )?;

                    // @Note we probably need to disallow referencing the same package through different
                    // names from the same package to be able to generate a correct lock-file
                    unbuilt_crates[index].resolve(Package::from_manifest(
                        CrateRole::Dependency,
                        index,
                        path,
                        resolved_transitive_dependencies,
                        manifest,
                    ));
                    resolved_dependencies.insert(name.clone(), index);
                }

                Ok(resolved_dependencies)
            }
        }
    };

    // @Beacon @Temporary
    let unbuilt_crates = unbuilt_crates
        .0
        .into_iter()
        .map(|crate_| {
            let crate_ = crate_.into_resolved().unwrap();
            println!(
                "crate to build: {:?} {} [{:?}]\n",
                crate_.scope.owner, crate_.name, crate_.path
            );
            crate_
        })
        .collect::<Vec<_>>();

    // for item in unbuilt_crates.0 {
    for item in unbuilt_crates.into_iter().rev() {
        let mut crate_ = item;
        // let mut crate_ = item.into_resolved().unwrap();
        eprintln!("in build loop: {:?} {}", crate_.scope.owner, crate_.name);

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
            if crate_.role == CrateRole::Main {
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
        .join(crate_type.default_root_file_stem())
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
