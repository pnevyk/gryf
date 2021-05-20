use std::cmp::Ordering;
use std::ops::{Add, Deref, DerefMut};

use crate::traits::Weight;

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

impl Into<f64> for FloatWeight {
    fn into(self) -> f64 {
        self.0
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

impl Into<f64> for UFloatWeight {
    fn into(self) -> f64 {
        self.0
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
