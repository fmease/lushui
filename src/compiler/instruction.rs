//! The bytecode instructions.

use crate::utility::Str;

// #[repr(u8)]
// enum Opcode {
//     CLOSURE,
//     CONSTANT,
//     ARGUMENT,
//     APPLY,
// }

#[derive(Clone, Copy, PartialEq, Eq, index_map::Index)]
pub struct ChunkIndex(pub usize);

use std::fmt;

impl fmt::Debug for ChunkIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:04}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Closure {
        chunk: ChunkIndex,
        captures: Vec<()>,
    },
    // @Temporary
    Constant(usize),
    Argument,
    Apply,
    Return,
    AddNat32,
}

impl Instruction {
    // @Temporary
    pub(super) fn print_with_constant_table(&self, constants: &[super::Value]) -> Str {
        match self {
            Self::Closure { chunk, captures } => {
                format!("closure {:?}, {:?}", chunk, captures).into()
            }
            &Self::Constant(constant) => {
                format!("constant {:04} ;;; {:?}", constant, constants[constant]).into()
            }
            Self::Argument => "argument".into(),
            Self::Apply => "apply".into(),
            Self::Return => "return".into(),
            Self::AddNat32 => "add.nat32".into(),
        }
    }

    // @Task need to resolve offsets
    // fn write(self, buffer: &mut [u8]) {
    //     match self {
    //         Self::Closure { chunk, captures } => {}
    //         Self::Constant(_) => {}
    //         Self::Argument => {}
    //         Self::Apply => {}
    //     }
    // }
}

// #[derive(Debug)]
pub struct Chunk {
    // debug
    pub name: String,
    pub instructions: Vec<Instruction>,
}

impl Chunk {
    pub const fn dummy() -> Self {
        Self {
            name: String::new(),
            instructions: Vec::new(),
        }
    }
}
