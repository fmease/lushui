//! Code generation.

pub mod cranelift;
#[cfg(feature = "llvm")]
pub mod llvm;

#[derive(Clone, Copy, Default)]
pub struct Options {
    pub emit_clif: bool,
    pub emit_llvm_ir: bool,
    pub verify_clif: bool,
    pub verify_llvm_ir: bool,
    // @Task
    // pub linker: Option<PathBuf>,
}
