//! The CLIF generator.
use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::{
        isa::CallConv,
        settings::{self, Flags},
        types, AbiParam, InstBuilder, Signature,
    },
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use diagnostics::{error::Result, Diagnostic};
use session::{Session, OUTPUT_FOLDER_NAME};
use std::{
    path::{Path, PathBuf},
    process::Command,
};

const PROGRAM_ENTRY_NAME: &str = "main";

pub fn compile_and_link(
    options: Options,
    component_root: &hir::Declaration,
    session: &Session<'_>,
) -> Result {
    if options.emit_clif || options.verify_clif {
        todo!(); //@Temporary
    }

    let path = compile(options, component_root, session);
    link(&path, session)
}

#[derive(Clone, Copy, Default)]
pub struct Options {
    pub emit_clif: bool,
    pub verify_clif: bool,
}

fn compile(
    _options: Options,
    _component_root: &hir::Declaration,
    session: &Session<'_>,
) -> PathBuf {
    let isa = cranelift_native::builder()
        .unwrap()
        .finish(Flags::new(settings::builder()))
        .unwrap();

    let mut module = ObjectModule::new(
        ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names()).unwrap(),
    );
    let mut context = module.make_context();

    context.func.signature = Signature {
        params: Vec::new(),
        returns: vec![AbiParam::new(types::I64)],
        call_conv: CallConv::Fast,
    };

    // @Beacon @Temporary
    // @Task actually compile `component_root`!
    let mut builder_context = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let entry_block = builder.create_block();
    builder.seal_block(entry_block);
    builder.switch_to_block(entry_block);
    let status = builder.ins().iconst(types::I64, 0);
    builder.ins().return_(&[status]);
    builder.finalize();

    // @Task do this for every function with `options.verify_clif`
    // context
    //         .verify(FlagsOrIsa {
    //             flags: &Flags::new(settings::builder()),
    //             isa: Some(module.isa()),
    //         })
    //         .unwrap();

    // @Task do this for every function with `options.emit_clif`
    // println!("{}", context.func.display());

    let function_id = module
        .declare_function(PROGRAM_ENTRY_NAME, Linkage::Export, &context.func.signature)
        .unwrap();
    module.define_function(function_id, &mut context).unwrap();

    let product = module.finish();

    let name = session.component().name().as_str();

    let path = match session.root_package() {
        // @Task ensure that the build folder exists
        Some(package) => {
            let mut path = session[package].folder().join(OUTPUT_FOLDER_NAME);
            path.push(name);
            path.set_extension("o");
            path
        }
        None => Path::new(name).with_extension("o"),
    };

    std::fs::write(&path, product.emit().unwrap()).unwrap();

    path
}

// @Task support linkers other than clang
//       (e.g. "`cc`", `gcc` (requires us to manually link to `libc` I think))
fn link(path: &Path, session: &Session<'_>) -> Result {
    let name = session.component().name().as_str();

    // @Task error handling!
    let output = Command::new("clang")
        .arg(path)
        .arg("-o")
        .arg(match session.root_package() {
            Some(package) => {
                let mut path = session[package].folder().join(OUTPUT_FOLDER_NAME);
                path.push(name);
                path
            }
            None => PathBuf::from(name),
        })
        .output()
        .unwrap();

    if !output.status.success() {
        // @Beacon @Task smh print output.{stdout,stderr} here
        return Err(Diagnostic::error()
            .message("failed to link object files")
            .report(session.reporter()));
    }

    Ok(())
}
