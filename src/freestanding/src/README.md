# Freestanding

TODO

## Features

TODO. List stuff like inherited visibility, derives, etc. Nullary variants.

Not (yet) supported: Streamlining using `&`, `&mut` etc.

Planned: Helper attribute on fields (also those inside of `#[common]`) called like
`#[get]`/`#[get_mut]` to generate delegate functions on the container enum to the field.

A way to define a constructor macro e.g. `#[constructor(name_of_con)]` super useful when
enum is streamlined e.g. `name_of_con! { Var { f: 20 } }` turns into
`Container::Var(std::rc::Rc::new(Var {f:20 }))`

streamliner trait??? but how if proc-macro??

## Examples

### Basic

```rust
use freestanding::freestanding;

#[freestanding]
#[derive(Clone, Debug)]
pub enum Container {
    Alpha { x: i32, y: String },
    Beta { a: [u16; 12] },
    Gamma(Box<Container>),
    Delta,
}
```

This generates the following code:

```rust
#[derive(Clone, Debug)]
pub enum Container {
    Alpha(pub Alpha),
    Beta(pub Beta),
    Gamma(pub Gamma),
    Delta,
}

#[derive(Clone, Debug)]
pub struct Alpha {
    pub x: i32,
    pub y: String,
}

#[derive(Clone, Debug)]
pub struct Beta {
    pub a: [u16; 12],
}

#[derive(Clone, Debug)]
pub struct Gamma(pub Box<Container>);
```

### Common Fields

```rust
#[freestanding]
#[common { foo: String, bar: u64 }]
#[streamline(Box)]
enum Container {
    Alpha { alpha: i32 },
    Beta,
    Gamma { gamma: String },
}
```

Output:

```rust
enum Container {
    Alpha(Box<Alpha>),
    Beta(Box<Beta>),
    Gamma(Box<Gamma>),
}

struct Alpha { foo: String, bar: u64, alpha: i32 }
struct Beta { foo: String, bar: u64 }
struct Gamma { foo: String, bar: u64, gamma: String }
```

NOTE errors if mixed: Structs vs tuple structs.

TODO common tuple struct: positioned first (if we support at all)

### Streamlining the Variants

```rust
#[freestanding]
#[streamline(std::rc::Rc)]
enum Container {
    Alpha { x: i32, y: String },
    Beta { a: Container, b: Container },
}
```

Generated output:

```rust
enum Container {
    Alpha(std::rc::Rc<Alpha>),
    Beta(std::rc::Rc<Beta>),
}

struct Alpha {
    x: i32,
    y: String,
}

struct Beta {
    a: Container,
    b: Container,
}
```

### With Generics

`PhantomData` marker needs to be supplied manually (in the future this restriction might be lifited).

```rust
use std::marker::PhantomData;

#[freestanding]
enum Container<A, B: std::ops::Add> {
    Alpha { alpha: A, beta: A, gamma: i32, _marker: PhantomData<B> },
    Beta { left: B::Output, right: B, _marker: PhantomData<A> },
}
```

Generated code:

```rust
enum Container<A, B: std::ops::Add> {
    Alpha(Alpha<A, B>),
    Beta(Beta<A, B>),
}

struct Alpha<A, B: std::ops::Add> { alpha: A, beta: A, gamma: i32, _marker: PhantomData<B> }
struct Beta<A, B: std::ops::Add> { left: B::Output, right: B, _marker: PhantomData<A> }
```
