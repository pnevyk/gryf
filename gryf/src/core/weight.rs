use std::{cmp::Ordering, ops::Add};

mod ordered_float;
mod unsigned_float;

use ordered_float::OrderedFloat;
pub use unsigned_float::*;

pub trait Weight: PartialOrd + Add<Self, Output = Self> + Clone + Sized {
    type Ord: Ord + From<Self> + Into<Self>;

    fn zero() -> Self;
    fn inf() -> Self;
    fn is_unsigned() -> bool;
}

pub trait GetWeight<E, W>
where
    W: Weight,
{
    fn get(&self, edge: &E) -> W;

    fn get_const(&self) -> Option<W> {
        None
    }

    fn is_const(&self) -> bool {
        self.get_const().is_some()
    }
}

pub trait IsConstWeight {}

impl<F, E, W> GetWeight<E, W> for F
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

impl<E> GetWeight<E, E> for Identity
where
    E: Clone + Weight,
{
    fn get(&self, edge: &E) -> E {
        edge.clone()
    }
}

#[derive(Debug)]
pub struct Unit;

impl<E> GetWeight<E, u8> for Unit {
    fn get(&self, _edge: &E) -> u8 {
        1
    }

    fn get_const(&self) -> Option<u8> {
        Some(1)
    }
}

impl IsConstWeight for Unit {}

#[derive(Debug)]
pub struct FromWeighted;

impl<E, W> GetWeight<Weighted<E, W>, W> for FromWeighted
where
    W: Weight,
{
    fn get(&self, edge: &Weighted<E, W>) -> W {
        edge.1.clone()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Weighted<T, W>(pub T, pub W);

impl<T, W: PartialEq> PartialEq for Weighted<T, W> {
    fn eq(&self, other: &Self) -> bool {
        self.1.eq(&other.1)
    }
}

impl<T, W: Eq> Eq for Weighted<T, W> {}

impl<T, W: PartialOrd> PartialOrd for Weighted<T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.1.partial_cmp(&other.1)
    }
}

impl<T, W: Ord> Ord for Weighted<T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1)
    }
}

macro_rules! impl_int_weight {
    ($ty:ty, $is_unsigned:expr) => {
        impl Weight for $ty {
            type Ord = Self;

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

impl_int_weight!(i8, false);
impl_int_weight!(i16, false);
impl_int_weight!(i32, false);
impl_int_weight!(i64, false);
impl_int_weight!(u8, true);
impl_int_weight!(u16, true);
impl_int_weight!(u32, true);
impl_int_weight!(u64, true);
impl_int_weight!(isize, false);
impl_int_weight!(usize, true);

macro_rules! impl_float_weight {
    ($ty:ty, $is_unsigned:expr) => {
        impl Weight for $ty {
            type Ord = OrderedFloat<Self>;

            fn zero() -> Self {
                <$ty>::default()
            }

            fn inf() -> Self {
                <$ty>::INFINITY
            }

            fn is_unsigned() -> bool {
                $is_unsigned
            }
        }
    };
}

impl_float_weight!(f32, false);
impl_float_weight!(f64, false);
impl_float_weight!(uf32, true);
impl_float_weight!(uf64, true);
