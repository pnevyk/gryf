use core::{
    borrow::Borrow,
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::Deref,
};

#[derive(Debug, Clone)]
pub enum WeakRef<'a, T> {
    Borrowed(&'a T),
    Owned(T),
}

impl<T: Clone> WeakRef<'_, T> {
    pub fn into_owned(self) -> T {
        match self {
            WeakRef::Borrowed(data) => data.clone(),
            WeakRef::Owned(data) => data,
        }
    }
}

impl<T> PartialEq for WeakRef<'_, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&**self, &**other)
    }
}

impl<T> Eq for WeakRef<'_, T> where T: Eq {}

impl<T> Hash for WeakRef<'_, T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T> PartialOrd for WeakRef<'_, T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        PartialOrd::partial_cmp(&**self, &**other)
    }
}

impl<T> Ord for WeakRef<'_, T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&**self, &**other)
    }
}

impl<'a, T> From<&'a T> for WeakRef<'a, T> {
    fn from(value: &'a T) -> Self {
        WeakRef::Borrowed(value)
    }
}

impl<T> From<T> for WeakRef<'_, T> {
    fn from(value: T) -> Self {
        WeakRef::Owned(value)
    }
}

impl<T> Deref for WeakRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            WeakRef::Borrowed(data) => data,
            WeakRef::Owned(ref data) => data,
        }
    }
}

impl<T> AsRef<T> for WeakRef<'_, T> {
    fn as_ref(&self) -> &T {
        match self {
            WeakRef::Borrowed(data) => data,
            WeakRef::Owned(ref data) => data,
        }
    }
}

impl<T> Borrow<T> for WeakRef<'_, T> {
    fn borrow(&self) -> &T {
        self
    }
}
