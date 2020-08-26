//! Stack-allocated growable array.

// @Note lacks a lot of general purpose methods like turning it into an iterator
// and so on.. (we don't need them right now) and constructing it with several elements
// others than calling `new` and using `push` (and a macro), also equality, Debug and so on...

use std::mem::{replace, MaybeUninit};
use std::{fmt, ptr::drop_in_place};

pub struct GrowArray<T, const N: usize> {
    items: [MaybeUninit<T>; N],
    length: usize,
}

impl<T, const N: usize> GrowArray<T, N> {
    pub const fn new() -> Self {
        Self {
            items: [MaybeUninit::uninit(); N],
            length: 0,
        }
    }

    pub const fn len(&self) -> usize {
        self.length
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn is_non_empty(&self) -> bool {
        !self.is_empty()
    }

    pub fn push(&mut self, value: T) {
        self.items[self.length] = MaybeUninit::new(value);
        self.length += 1;
    }

    pub fn try_push(&mut self, value: T) -> Result<(), OverflowError> {
        *self.items.get_mut(self.length).ok_or(OverflowError)? = MaybeUninit::new(value);
        self.length += 1;
        Ok(())
    }

    #[allow(unused_unsafe)]
    pub unsafe fn push_unchecked(&mut self, value: T) {
        *unsafe { self.items.get_unchecked_mut(self.length) } = MaybeUninit::new(value);
        self.length += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }
        let item = unsafe {
            replace(
                self.items.get_unchecked_mut(self.length - 1),
                MaybeUninit::uninit(),
            )
            .assume_init()
        };
        self.length -= 1;
        Some(item)
    }

    pub fn truncate(&mut self, length: usize) {
        if length >= self.length {
            return;
        }

        let previous_length = self.length;
        self.length = length;

        for index in length..previous_length {
            unsafe { drop_in_place(self.items.get_unchecked_mut(index).as_mut_ptr()) };
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        (index < self.length).then(|| unsafe { self.get_unchecked(index) })
    }

    #[allow(unused_unsafe)]
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe { &*self.items.get_unchecked(index).as_ptr() }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        (index < self.length).then(move || unsafe { self.get_unchecked_mut(index) })
    }

    #[allow(unused_unsafe)]
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe { &mut *self.items.get_unchecked_mut(index).as_mut_ptr() }
    }

    // redundant once we impl Deref/DerefMut
    pub fn first(&self) -> Option<&T> {
        self.get(0)
    }

    pub fn first_mut(&mut self) -> Option<&mut T> {
        self.get_mut(0)
    }

    pub fn last(&self) -> Option<&T> {
        self.is_non_empty()
            .then(|| unsafe { self.get_unchecked(self.length - 1) })
    }

    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.is_non_empty()
            .then(move || unsafe { self.get_unchecked_mut(self.length - 1) })
    }
}

impl<T, const N: usize> Drop for GrowArray<T, N> {
    fn drop(&mut self) {
        self.truncate(0);
    }
}

// use std::ops::{Deref, DerefMut, Index, IndexMut};

// impl<T, const N: usize> Deref for GrowArray<T, N> {
//     type Target = [T];

//     fn deref(&self) -> &Self::Target {
//         todo!()
//     }
// }

impl<T, const N: usize> Default for GrowArray<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

// @Beacon task impl owning, borrowing and uniquely borrowing iterators!!

impl<T: fmt::Debug, const N: usize> fmt::Debug for GrowArray<T, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries((0..self.length).map(|index| unsafe { self.get_unchecked(index) }))
            .finish()
    }
}

#[derive(Debug)]
pub struct OverflowError;
