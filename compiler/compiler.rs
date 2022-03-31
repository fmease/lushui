//! The compiler emitting bytecode.

// @Task rename to codegen

use crate::{
    component::Component,
    hir::{self, Declaration, DeclarationIndex, Expression, Number},
    syntax::lowered_ast::attributes::AttributeName,
    utility::HashMap,
};
use index_map::{Index as _, IndexMap};
use instruction::{Chunk, ChunkIndex, Instruction};
use std::{default::default, fmt};

mod instruction; // @Note naming swap out with name&concept bytecode
pub(crate) mod interpreter; // @Task move stuff from here to there

#[derive(PartialEq, Eq)]
enum LambdaParent {
    Lambda,
    Declaration,
}

struct Compiler<'a> {
    chunks: IndexMap<ChunkIndex, Chunk>,
    // will be tagless in the bytecode
    constants: Vec<Value>, // IndexVec?
    lambda_amount: usize,
    // @Temporary
    entry: Option<ChunkIndex>,
    declaration_mapping: HashMap<DeclarationIndex, ChunkIndex>,
    component: &'a Component,
}

impl<'a> Compiler<'a> {
    fn new(component: &'a Component) -> Self {
        Self {
            chunks: IndexMap::new(),
            constants: Vec::new(),
            lambda_amount: 0,
            entry: None,
            declaration_mapping: default(),
            component,
        }
    }

    // @Temporary nicer debugging
    fn print_chunks(&self) -> String {
        let mut result = String::new();

        for (index, chunk) in self.chunks.iter() {
            result += &format!("{:04} {}:\n", index.value(), chunk.name);

            for (index, instruction) in chunk.instructions.iter().enumerate() {
                result += &format!(
                    "    {:04} {}\n",
                    index,
                    instruction.print_with_constant_table(&self.constants)
                );
            }

            result.push('\n');
        }

        result
    }

    fn next_chunk_index_for_declaration(&mut self, index: DeclarationIndex) -> ChunkIndex {
        if let Some(&index) = self.declaration_mapping.get(&index) {
            index
        } else {
            self.add_chunk_unseen_declaration(index, Chunk::dummy())
        }
    }

    fn add_next_chunk_from_declaration(
        &mut self,
        index: DeclarationIndex,
        chunk: Chunk,
    ) -> ChunkIndex {
        if let Some(&index) = self.declaration_mapping.get(&index) {
            self.chunks[index] = chunk;
            index
        } else {
            self.add_chunk_unseen_declaration(index, chunk)
        }
    }

    fn add_chunk_unseen_declaration(
        &mut self,
        declaration_index: DeclarationIndex,
        chunk: Chunk,
    ) -> ChunkIndex {
        let index = self.chunks.insert(chunk);
        self.declaration_mapping.insert(declaration_index, index);
        index
    }

    fn compile_declaration(&mut self, declaration: &Declaration) -> Result<(), CompilationError> {
        use hir::DeclarationKind::*;

        match &declaration.value {
            Function(function) => {
                let index = self.add_next_chunk_from_declaration(
                    function.binder.declaration_index().unwrap(),
                    Chunk {
                        name: function.binder.to_string(),
                        instructions: Vec::new(),
                    },
                );

                if declaration.attributes.contains(AttributeName::Intrinsic) {
                    // @Bug can actually have arity > 1
                    // @Task handle currying/partial evaluation
                    // do nothing right now
                    // @Task abstract over this!!!
                    // if value.binder.as_str() == "add-nat32" {
                    //     self.chunks[index].instructions.push(Instruction::AddNat32);
                    // } else {
                    //     panic!();
                    // }
                    panic!();
                } else {
                    self.chunks[index].instructions = self.compile_expression(
                        function.expression.as_ref().unwrap(),
                        LambdaParent::Declaration,
                    )?;
                    self.chunks[index].instructions.push(Instruction::Return);

                    // @Task obsolete once we map any ComponentIndex to a chunk identifier
                    if self
                        .component
                        .entry
                        .as_ref()
                        .map_or(false, |entry| entry == &function.binder)
                    {
                        self.entry = Some(index);
                    }
                }
            }
            Module(module) => {
                for declaration in &module.declarations {
                    self.compile_declaration(declaration)?;
                }
            }
            // @Temporary @Beacon
            _ => {}
        }

        Ok(())
    }

    fn next_lambda_chunk_name(&mut self) -> usize {
        let next = self.lambda_amount;
        self.lambda_amount += 1;
        next
    }

    // @Temporary
    fn compile_expression(
        &mut self,
        expression: &Expression,
        parent: LambdaParent,
    ) -> Result<Vec<Instruction>, CompilationError> {
        use hir::ExpressionKind::*;

        let mut instructions = Vec::new();

        match &expression.value {
            Application(application) => {
                let mut argument =
                    self.compile_expression(&application.argument, LambdaParent::Lambda)?;
                let mut callee =
                    self.compile_expression(&application.callee, LambdaParent::Lambda)?;
                instructions.append(&mut argument);
                instructions.append(&mut callee);
                instructions.push(Instruction::Apply);
            }
            Number(number) => {
                let constant = self.constants.len();
                self.constants.push(Value::Number(number.as_ref().clone()));
                instructions.push(Instruction::Constant(constant));
            }
            Text(text) => {
                let constant = self.constants.len();
                let hir::Text::Text(text) = &**text;
                self.constants.push(Value::Text(text.clone()));
                instructions.push(Instruction::Constant(constant));
            }
            Binding(binding) => {
                if let Some(index) = binding.0.declaration_index() {
                    // declarations will not always compile to chunks
                    // so we gonna need to push constant in some places
                    instructions.push(Instruction::Closure {
                        chunk: self.next_chunk_index_for_declaration(index),
                        captures: Vec::new(),
                    });
                } else if binding.0.is_innermost() {
                    instructions.push(Instruction::Argument);
                } else {
                    todo!()
                }
            }
            Lambda(lambda) => {
                let mut body = self.compile_expression(&lambda.body, LambdaParent::Lambda)?;

                if parent == LambdaParent::Lambda {
                    let name = format!("$lambda{}", self.next_lambda_chunk_name());
                    body.push(Instruction::Return);
                    let index = self.chunks.insert(Chunk {
                        name,
                        instructions: body,
                    });

                    instructions.push(Instruction::Closure {
                        chunk: index,
                        captures: Vec::new(),
                    });
                } else {
                    instructions.append(&mut body);
                }
            }
            _ => todo!(),
        };

        Ok(instructions)
    }
}

#[derive(Clone)]
// very @Temporary data structure (too much wasted memory)
enum Value {
    Number(Number),
    Text(String),
    Closure {
        chunk: ChunkIndex,
        captures: Vec<()>,
    },
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(number) => write!(f, "{number}"),
            Self::Text(text) => write!(f, "{text}"),
            Self::Closure { chunk, captures } => write!(f, "?(closure {chunk:?} {captures:?})"),
        }
    }
}

// @Temporary
pub enum Error {
    Compiletime(CompilationError),
    Runtime(RuntimeError),
}

impl From<RuntimeError> for Error {
    fn from(error: RuntimeError) -> Self {
        Self::Runtime(error)
    }
}

impl From<CompilationError> for Error {
    fn from(error: CompilationError) -> Self {
        Self::Compiletime(error)
    }
}

pub enum CompilationError {}

// @Temporary
pub fn compile_and_interpret_declaration(
    declaration: &Declaration,
    component: &Component,
) -> Result<(), Error> {
    let mut compiler = Compiler::new(component);
    compiler.compile_declaration(declaration)?;
    eprintln!("{}", compiler.print_chunks());

    let mut interpreter = ByteCodeInterpreter::new(&compiler);
    interpreter.execute()?;

    Ok(())
}

// @Question what should the relation be *actually* like?
//const FRAME_SIZE: usize = 64;
//const STACK_SIZE: usize = FRAME_SIZE * u8::MAX as usize;

struct CallFrame {
    chunk: ChunkIndex,
    instruction: usize,
    base: usize,
}

// @Task read bytecode not Compiler
struct ByteCodeInterpreter<'a> {
    c: &'a Compiler<'a>,
    //stack: StaticVec<Value, STACK_SIZE>,
    stack: Vec<Value>,
    //frames: StaticVec<CallFrame, FRAME_SIZE>,
    frames: Vec<CallFrame>,
}

impl<'a> ByteCodeInterpreter<'a> {
    fn new(c: &'a Compiler<'a>) -> Self {
        Self {
            c,
            stack: default(),
            frames: default(),
        }
    }

    fn execute(&mut self) -> Result<(), RuntimeError> {
        let entry = self.c.entry.ok_or(RuntimeError::NoEntry)?;

        self.frames.push(CallFrame {
            chunk: entry,
            instruction: 0,
            base: 0,
        });

        while let Some(frame) = self.frames.last_mut() {
            let chunk = &self.c.chunks[frame.chunk];

            let instruction = chunk.instructions[frame.instruction].clone();
            frame.instruction += 1;

            eprintln!(
                ">> at {:?}/{:04}: instruction={:?}, stack={:?}",
                frame.chunk, frame.instruction, instruction, self.stack
            );

            match instruction {
                Instruction::Closure { chunk, captures } => {
                    self.stack.push(Value::Closure { chunk, captures });
                }
                Instruction::Constant(constant) => {
                    self.stack.push(self.c.constants[constant].clone());
                }
                Instruction::Argument => {
                    self.stack.push(self.stack.get(frame.base).unwrap().clone());
                }
                Instruction::Apply => {
                    let callee = self.stack.pop().unwrap();
                    self.frames.push(CallFrame {
                        chunk: match callee {
                            Value::Closure { chunk, .. } => chunk,
                            _ => unreachable!("calling non-function {:?}", callee),
                        },
                        base: self.stack.len() - 1,
                        instruction: 0,
                    });
                }
                Instruction::Return => {
                    let result = self.stack.pop().unwrap();
                    let frame = self.frames.pop().unwrap();
                    self.stack.truncate(frame.base);
                    self.stack.push(result);
                }
                Instruction::AddNat32 => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    let a = match a {
                        Value::Number(Number::Nat32(a)) => a,
                        _ => unreachable!(),
                    };
                    let b = match b {
                        Value::Number(Number::Nat32(b)) => b,
                        _ => unreachable!(),
                    };
                    self.stack.push(Value::Number(Number::Nat32(a + b)));
                }
            };
        }

        Ok(())
    }
}

pub enum RuntimeError {
    NoEntry,
}
