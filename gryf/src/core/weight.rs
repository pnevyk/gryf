//! Traits and types for edge weights.
//!
//! gryf distinguishes between _attributes_ (any data) and _weights_ (measurable
//! and comparable values). This distinction makes the usage flexible where the
//! attributes contain more data than just weights or the weight function is
//! different depending on context even if the graph attributes are the same.

use std::{cmp::Ordering, ops::Add};

mod ordered_float;
mod unsigned_float;

use ordered_float::OrderedFloat;
pub use unsigned_float::*;

/// Type that can be treated as a weight.
pub trait Weight: PartialOrd + Add<Self, Output = Self> + Clone + Sized {
    /// Associated type that is equivalent to `Self` but implements the [`Ord`]
    /// trait as the weight itself is required to implement only [`PartialOrd`].
    ///
    /// The main and only motivation for this extra level of abstraction is to
    /// have convenient support for floats from the standard library that don't
    /// implement [`Ord`]. It's a compromise between not requiring users to use
    /// special types or wrappers in their graphs and giving the possibility to
    /// use `Ord`-based functions and data structures for algorithm developers.
    type Ord: Ord + From<Self> + Into<Self>;

    /// Returns value that represents the weight equal to zero.
    fn zero() -> Self;

    /// Returns value that represents the weight equal to infinity.
    fn inf() -> Self;

    /// Returns `true` if the weight is guaranteed to be unsigned at compile
    /// time.
    ///
    /// This information can be utilized for performance-oriented choices that
    /// can exploit it. For example, there can be a more efficient algorithm
    /// that works only for unsigned weights.
    fn is_unsigned() -> bool;
}

/// Mapping from an edge attribute to corresponding edge weight.
///
/// The list of mappings supported out of the box:
///
/// * [identity](Identity): `E → E`
/// * [unit](Unit): `E → 1`
/// * [from weighted](FromWeighted): `(E, W) → W`
/// * anything that implements `Fn(&E) -> W`
pub trait GetWeight<E, W>
where
    W: Weight,
{
    /// Maps the edge attribute to the corresponding weight.
    fn get(&self, edge: &E) -> W;

    /// Returns the weight regardless of the edge attribute, if it's the same
    /// for all edges.
    ///
    /// Algorithms should try this first before calling [`get`](GetWeight::get)
    /// because it avoids the need for retrieving the edge attribute, which
    /// means better efficiency, in cases where the weight is known (e.g.,
    /// [unit](`Unit`) weight).
    fn get_const(&self) -> Option<W> {
        None
    }

    /// Returns `true` when the mapping represents a constant weight, that is,
    /// [`get_const`](GetWeight::get_const) always returns `Some`.
    fn is_const(&self) -> bool {
        self.get_const().is_some()
    }
}

/// Type-level information that the [`GetWeight::get_const`] always returns
/// `Some`.
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

/// Identity mapping `E → E` used when the edge attribute is the weight itself.
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

// Support for unit type which on its own doesn't implement the `Weight` trait.
// This is useful because many algorithms might use `Identity` as the default
// weight mapping and this allows to seamlessly support graphs with unit
// attributes.
impl GetWeight<(), u8> for Identity {
    fn get(&self, _: &()) -> u8 {
        1
    }

    fn get_const(&self) -> Option<u8> {
        Some(1)
    }
}

/// Unit weight `E → 1` used when the edge weight doesn't matter.
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

/// Mapping from tuple `(E, W) → W` used when the weight is stored together with
/// the edge attribute in the graph.
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

/// Helper type for holding the weight together with the edge attribute.
#[derive(Debug, Clone, Copy)]
pub struct Weighted<E, W>(pub E, pub W);

impl<E, W: PartialEq> PartialEq for Weighted<E, W> {
    fn eq(&self, other: &Self) -> bool {
        self.1.eq(&other.1)
    }
}

impl<E, W: Eq> Eq for Weighted<E, W> {}

impl<E, W: PartialOrd> PartialOrd for Weighted<E, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.1.partial_cmp(&other.1)
    }
}

impl<E, W: Ord> Ord for Weighted<E, W> {
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
