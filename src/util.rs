//! Utility functionality.

use std::path::Path;

pub(crate) fn has_file_extension(path: &Path, required_extension: &str) -> bool {
    path.extension().and_then(|extension| extension.to_str()) == Some(required_extension)
}

pub(crate) macro obtain($expr:expr, $( $pat:pat )|+ $( if $guard:expr )? $(,)? => $mapping:expr) {
    match $expr {
        $( $pat )|+ $( if $guard )? => Some($mapping),
        _ => None
    }
}

pub(crate) trait GetFromEndExt {
    type Item;

    fn get_from_end(&self, index: usize) -> Option<&Self::Item>;
}

impl<T> GetFromEndExt for [T] {
    type Item = T;

    fn get_from_end(&self, index: usize) -> Option<&Self::Item> {
        let index = self.len().checked_sub(index.checked_add(1)?)?;
        Some(unsafe { self.get_unchecked(index) })
    }
}
