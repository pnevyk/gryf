use std::cmp::Ordering;

use super::unsigned_float::{uf32, uf64};

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct OrderedFloat<T>(T);

macro_rules! impl_ord_eq {
    ($ty:ty) => {
        impl Ord for OrderedFloat<$ty> {
            fn cmp(&self, other: &Self) -> Ordering {
                self.0.total_cmp(&other.0)
            }
        }

        impl Eq for OrderedFloat<$ty> {}
    };
}

impl_ord_eq!(f32);
impl_ord_eq!(f64);
impl_ord_eq!(uf32);
impl_ord_eq!(uf64);

macro_rules! impl_conv {
    ($ty:ty) => {
        impl From<$ty> for OrderedFloat<$ty> {
            fn from(value: $ty) -> Self {
                Self(value)
            }
        }

        impl From<OrderedFloat<$ty>> for $ty {
            fn from(value: OrderedFloat<$ty>) -> Self {
                value.0
            }
        }
    };
}

impl_conv!(f32);
impl_conv!(f64);
impl_conv!(uf32);
impl_conv!(uf64);
