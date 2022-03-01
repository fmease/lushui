#![feature(result_into_ok_or_err, let_else)]

use utilites::{SerializeExt, TokenStream1};

mod discriminant;
mod elements;
mod format;
mod from_str;
mod str;
mod utilites;

// @Beacon @Task create DashStr, DashFromStr which implies #[format(dash_case)]

/// Derive a fieldless counterpart to the given enum together with a mapping.
///
/// One additionally has to specify the name of the fieldless type `TYPE` and
/// the name of the mapping `MAPPING` via the helper attribute `#[discriminant(MAPPING: TYPE)]`.
/// Currently, this macro unconditionally adds `#[derives(Clone, Copy, PartialEq, Eq, Debug)]`
/// to the fieldless type. The mapping is defined as an inherent associated function on
/// the original type.
///
/// # Examples
///
/// ```no_run
/// #[derive(Discriminant)]
/// #[discriminant(name: TokenName)]
/// pub(crate) enum TokenKind {
///     Comment,
///     Identifier { source: String, raw: bool },
///     NumberLiteral(i32),
///     QuestionMark,
///     Colon,
/// }
/// ```
///
/// The following code will be derived from the code above:
///
/// ```no_run
/// impl TokenKind {
///     pub(crate) fn name(&self) -> TokenName {
///         match self {
///             Self::Comment => TokenName::Comment,
///             Self::Identifier { .. } => TokenName::Identifier,
///             Self::NumberLiteral(..) => TokenName::NumberLiteral,
///             Self::QuestionMark => TokenName::QuestionMark,
///             Self::Colon => TokenName::Colon,
///         }
///     }
/// }
///
/// #[derives(Clone, Copy, PartialEq, Eq, Debug)]
/// pub(crate) enum TokenName {
///     Comment,
///     Identifier,
///     NumberLiteral,
///     QuestionMark,
///     Colon,
/// }
/// ```
#[proc_macro_derive(Discriminant, attributes(discriminant))]
pub fn derive_discriminant(input: TokenStream1) -> TokenStream1 {
    discriminant::derive(input).serialize()
}

/// Derive an associated function returning an array of all variants of the given enum.
///
/// Variants with fields are currently allowed as long as they implement [`Default`].
/// The associated function is called `elements` and is not `const` right now.
// @Task update docs: we use a trait rn + add example
// @Task allow the user to opt-out of the trait impl (only doing an inherent impl)
#[proc_macro_derive(Elements)]
pub fn derive_elements(input: TokenStream1) -> TokenStream1 {
    elements::derive(input).serialize()
}

/// Derive a [`FromStr`][trait] implementation from the given fieldless enum.
///
/// One additionally has to specify the letter case `CASE` of the textual representation
/// via the helper attribute `#[format(CASE)]`. Currently, the only option is `dash_case`.
///
/// ## Examples
///
/// ```no_run
/// #[derive(FromStr)]
/// #[format(dash_case)]
/// pub enum Mode {
///     FoldOut,
///     RipApart,
/// }
/// ```
///
/// Roughly the following code will be derived from the code above:
///
/// ```no_run
/// impl core::str::FromStr for Mode {
///     type Err = ();
///
///     fn from_str(source: &str) -> Result<Self, Self::Err> {
///          Ok(match source {
///             "fold-out" => Self::FoldOut,
///             "rip-apart" => Self::RipApart,
///             _ => return Err(()),
///          })
///     }
/// }
/// ```
///
/// [trait]: std::str::FromStr
#[proc_macro_derive(FromStr, attributes(format))]
pub fn derive_from_str(input: TokenStream1) -> TokenStream1 {
    from_str::derive(input).serialize()
}

// @Task docs
/// # Examples
///
/// ```no_run
/// #[derive(Str)]
/// #[format(dash_case)]
/// // #[str(name)] // the default
/// pub enum Mode {
///     FoldOut,
///     RipApart { force: u64 },
/// }
/// ```
///
/// Roughly the following code will be derived from the code above:
///
/// ```no_run
/// impl Mode {
///     pub const fn name(&self) -> &'static str {
///         match self {
///             Self::FoldOut => "fold-out",
///             Self::RipApart { .. } => "rip-apart",
///         }
///     }
/// }
/// ```
// @Task mention that #[str] is opt
// @Task mention that self is taken by value if derive(Copy) (not yet impl)
#[proc_macro_derive(Str, attributes(format, str))]
pub fn derive_static_str(input: TokenStream1) -> TokenStream1 {
    str::derive(input).serialize()
}
