use std::cmp::Ordering;
use std::ops::{Add, Deref, DerefMut};

pub trait Weight: Ord + Add<Self, Output = Self> + Clone + Sized {
    fn zero() -> Self;
    fn inf() -> Self;
    fn is_unsigned() -> bool;
}

pub trait GetEdgeWeight<E, W>
where
    W: Weight,
{
    fn get(&self, edge: &E) -> W;
}

impl<F, E, W> GetEdgeWeight<E, W> for F
where
    F: Fn(&E) -> W,
    W: Weight,
{
    fn get(&self, edge: &E) -> W {
        (self)(edge)
    }
}

#[derive(Debug)]
pub struct Identity;

impl<E> GetEdgeWeight<E, E> for Identity
where
    E: Clone + Weight,
{
    fn get(&self, edge: &E) -> E {
        edge.clone()
    }
}

#[derive(Debug)]
pub struct Unit;

impl<E> GetEdgeWeight<E, usize> for Unit {
    fn get(&self, _edge: &E) -> usize {
        1
    }
}

#[derive(Debug)]
pub struct FromWeighted;

impl<E, W> GetEdgeWeight<Weighted<E, W>, W> for FromWeighted
where
    W: Weight,
{
    fn get(&self, edge: &Weighted<E, W>) -> W {
        edge.1.clone()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Weighted<T, W>(pub T, pub W);

impl<T, W: Weight> PartialEq for Weighted<T, W> {
    fn eq(&self, other: &Self) -> bool {
        self.1.eq(&other.1)
    }
}

impl<T, W: Weight> Eq for Weighted<T, W> {}

impl<T, W: Weight> PartialOrd for Weighted<T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.1.partial_cmp(&other.1)
    }
}

impl<T, W: Weight> Ord for Weighted<T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct FloatWeight(f64);

impl FloatWeight {
    pub fn new(value: f64) -> Option<Self> {
        if value.is_finite() || value == f64::INFINITY {
            Some(Self(value))
        } else {
            None
        }
    }
}

impl Eq for FloatWeight {}

// FloatWeight is guaranteed to hold only finite or infinity value (not a NaN).
#[allow(clippy::derive_ord_xor_partial_ord)]
impl Ord for FloatWeight {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}

impl Add<Self> for FloatWeight {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Deref for FloatWeight {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FloatWeight {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<FloatWeight> for f64 {
    fn from(val: FloatWeight) -> Self {
        val.0
    }
}

impl Weight for FloatWeight {
    fn zero() -> Self {
        Self(0.0)
    }

    fn inf() -> Self {
        Self(f64::INFINITY)
    }

    fn is_unsigned() -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct UFloatWeight(f64);

impl UFloatWeight {
    pub fn new(value: f64) -> Option<Self> {
        // Includes inf, but not NaN or -inf.
        if value >= 0.0 {
            Some(Self(value))
        } else {
            None
        }
    }
}

impl Eq for UFloatWeight {}

// UFloatWeight is guaranteed to hold only finite or positive infinity value
// (not a NaN).
#[allow(clippy::derive_ord_xor_partial_ord)]
impl Ord for UFloatWeight {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}

impl Add<Self> for UFloatWeight {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Deref for UFloatWeight {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for UFloatWeight {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<UFloatWeight> for f64 {
    fn from(val: UFloatWeight) -> Self {
        val.0
    }
}

impl Weight for UFloatWeight {
    fn zero() -> Self {
        Self(0.0)
    }

    fn inf() -> Self {
        Self(f64::INFINITY)
    }

    fn is_unsigned() -> bool {
        true
    }
}

macro_rules! impl_num_weight {
    ($ty:ty, $is_unsigned:expr) => {
        impl Weight for $ty {
            fn zero() -> Self {
                0
            }

            fn inf() -> Self {
                <$ty>::MAX
            }

            fn is_unsigned() -> bool {
                $is_unsigned
            }
        }
    };
}

impl_num_weight!(i8, false);
impl_num_weight!(i16, false);
impl_num_weight!(i32, false);
impl_num_weight!(i64, false);
impl_num_weight!(u8, true);
impl_num_weight!(u16, true);
impl_num_weight!(u32, true);
impl_num_weight!(u64, true);
impl_num_weight!(isize, false);
impl_num_weight!(usize, true);
