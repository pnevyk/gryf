//! Module for hiding the difference between owned and borrowed data.

use core::{
    borrow::Borrow,
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::Deref,
};

/// `OwnableRef` is a type that represents either a
/// [borrowed](OwnableRef::Borrowed) or [owned](OwnableRef::Owned) value of
/// which a shared reference can be taken.
///
/// This type is very similar to [`std::borrow::Cow`] type, but is not intended
/// for copy-on-write semantics. The main justification for using a custom type
/// is that [`OwnableRef::into_owned`] requires `T: Clone` instead of `T:
/// ToOwned`.
#[derive(Debug, Clone)]
pub enum OwnableRef<'a, T> {
    Borrowed(&'a T),
    Owned(T),
}

impl<T: Clone> OwnableRef<'_, T> {
    /// Extracts the owned data or clones it in case of a borrow.
    pub fn into_owned(self) -> T {
        match self {
            OwnableRef::Borrowed(attr) => attr.clone(),
            OwnableRef::Owned(attr) => attr,
        }
    }
}

impl<T> PartialEq for OwnableRef<'_, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&**self, &**other)
    }
}

impl<T> Eq for OwnableRef<'_, T> where T: Eq {}

impl<T> Hash for OwnableRef<'_, T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T> PartialOrd for OwnableRef<'_, T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        PartialOrd::partial_cmp(&**self, &**other)
    }
}

impl<T> Ord for OwnableRef<'_, T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&**self, &**other)
    }
}

impl<'a, T> From<&'a T> for OwnableRef<'a, T> {
    fn from(value: &'a T) -> Self {
        OwnableRef::Borrowed(value)
    }
}

impl<T> From<T> for OwnableRef<'_, T> {
    fn from(value: T) -> Self {
        OwnableRef::Owned(value)
    }
}

impl<T> Deref for OwnableRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            OwnableRef::Borrowed(attr) => attr,
            OwnableRef::Owned(attr) => attr,
        }
    }
}

impl<T> AsRef<T> for OwnableRef<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            OwnableRef::Borrowed(attr) => attr,
            OwnableRef::Owned(attr) => attr,
        }
    }
}

impl<T> Borrow<T> for OwnableRef<'_, T> {
    fn borrow(&self) -> &T {
        self
    }
}
