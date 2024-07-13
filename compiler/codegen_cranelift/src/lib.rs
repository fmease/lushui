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
use diagnostics::{error::Result, Diag};
use session::{Session, OUTPUT_FOLDER_NAME};
use std::{
    path::{Path, PathBuf},
    process::Command,
};
use utility::PROGRAM_ENTRY;

pub fn compile_and_link(opts: Options, comp_root: &hir::Decl, sess: &Session<'_>) -> Result {
    if opts.emit_clif || opts.verify_clif {
        todo!(); //@Temporary
    }

    let path = compile(opts, comp_root, sess);
    link(&path, sess)
}

#[derive(Clone, Copy, Default)]
pub struct Options {
    pub emit_clif: bool,
    pub verify_clif: bool,
}

fn compile(_opts: Options, _comp_root: &hir::Decl, sess: &Session<'_>) -> PathBuf {
    let program_entry_name = PROGRAM_ENTRY.to_str();

    let isa = cranelift_native::builder()
        .unwrap()
        .finish(Flags::new(settings::builder()))
        .unwrap();

    let mut module = ObjectModule::new(
        ObjectBuilder::new(
            isa,
            program_entry_name,
            cranelift_module::default_libcall_names(),
        )
        .unwrap(),
    );
    let mut cx = module.make_context();

    cx.func.signature = Signature {
        params: Vec::new(),
        returns: vec![AbiParam::new(types::I64)],
        call_conv: CallConv::Fast,
    };

    // @Beacon @Temporary
    // @Task actually compile `component_root`!
    let mut builder_context = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut cx.func, &mut builder_context);
    let entry_block = builder.create_block();
    builder.seal_block(entry_block);
    builder.switch_to_block(entry_block);
    let status = builder.ins().iconst(types::I64, 0);
    builder.ins().return_(&[status]);
    builder.finalize();

    // @Task do this for every function with `options.verify_clif`
    // cx.verify(FlagsOrIsa {
    //             flags: &Flags::new(settings::builder()),
    //             isa: Some(module.isa()),
    //         })
    //         .unwrap();

    // @Task do this for every function with `options.emit_clif`
    // println!("{}", cx.func.display());

    let func_id = module
        .declare_function(program_entry_name, Linkage::Export, &cx.func.signature)
        .unwrap();
    module.define_function(func_id, &mut cx).unwrap();

    let product = module.finish();

    let name = sess.comp().name().to_str();

    let path = match sess.root_pkg() {
        // @Task ensure that the build folder exists
        Some(package) => {
            let mut path = sess[package].folder().join(OUTPUT_FOLDER_NAME);
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
fn link(path: &Path, sess: &Session<'_>) -> Result {
    let name = sess.comp().name().to_str();

    // @Task error handling!
    let output = Command::new("clang")
        .arg(path)
        .arg("-o")
        .arg(match sess.root_pkg() {
            Some(package) => {
                let mut path = sess[package].folder().join(OUTPUT_FOLDER_NAME);
                path.push(name);
                path
            }
            None => PathBuf::from(name),
        })
        .output()
        .unwrap();

    if !output.status.success() {
        // @Beacon @Task smh print output.{stdout,stderr} here
        return Err(Diag::error()
            .message("failed to link object files")
            .report(sess.rep()));
    }

    Ok(())
}
