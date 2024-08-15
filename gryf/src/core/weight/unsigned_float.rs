use std::{
    cmp::Ordering,
    fmt,
    iter::{Product, Sum},
    num::{FpCategory, ParseFloatError},
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign},
};

macro_rules! delegate_fn {
    ($ty:ty, $name:ident $(,$param:ident, $param_ty:ty)*) => {
        pub fn $name(self $(,$param: $param_ty)*) -> $ty {
            <$ty>::new_unchecked(self.0.$name($($param),*))
        }
    };
    ($ty:ty, $name:ident $(,$param:ident, $param_ty:ty)*; $ret_ty:ty) => {
        pub fn $name(self $(,$param: $param_ty)*) -> $ret_ty {
            self.0.$name($($param),*)
        }
    };
    ($ty:ty, $name:ident, other) => {
        pub fn $name(self, other: $ty) -> $ty {
            <$ty>::new_unchecked(self.0.$name(other.0))
        }
    };
}

macro_rules! delegate_const {
    ($name:ident, $const_ty:ty, $ty:ty) => {
        pub const $name: $const_ty = <$ty>::$name;
    };
    ($name:ident, $const_ty:ty, wrap, $ty:ty) => {
        pub const $name: $const_ty = <$const_ty>::new_unchecked(<$ty>::$name);
    };
}

macro_rules! impl_binop {
    ($ty:ident, $op:ident, $op_fn:ident, $op_assign:ident, $op_assign_fn:ident, $operator:tt) => {
        impl $op<&$ty> for &$ty {
            type Output = $ty;

            fn $op_fn(self, rhs: &$ty) -> Self::Output {
                $ty(self.0 $operator rhs.0)
            }
        }

        impl $op<&$ty> for $ty {
            type Output = $ty;

            fn $op_fn(self, rhs: &$ty) -> Self::Output {
                $ty(self.0 $operator rhs.0)
            }
        }

        impl $op<$ty> for &$ty {
            type Output = $ty;

            fn $op_fn(self, rhs: $ty) -> Self::Output {
                $ty(self.0 $operator rhs.0)
            }
        }

        impl $op<$ty> for $ty {
            type Output = $ty;

            fn $op_fn(self, rhs: $ty) -> Self::Output {
                $ty(self.0 $operator rhs.0)
            }
        }

        #[allow(clippy::assign_op_pattern)]
        impl $op_assign<&$ty> for $ty {
            fn $op_assign_fn(&mut self, rhs: &$ty) {
                self.0 = self.0 $operator rhs.0;
            }
        }

        #[allow(clippy::assign_op_pattern)]
        impl $op_assign<$ty> for $ty {
            fn $op_assign_fn(&mut self, rhs: $ty) {
                self.0 = self.0 $operator rhs.0;
            }
        }
    };
}

macro_rules! declare_unsigned_float {
    ($name:ident, $ty:ty, $int_ty:ty) => {
        #[doc = r"Unsigned variant of ["]
        #[doc = stringify!($ty)]
        #[doc = r"] type, useful for [is_unsigned](super::Weight::is_unsigned) property."]
        #[allow(non_camel_case_types)]
        #[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
        #[repr(transparent)]
        pub struct $name($ty);

        impl $name {
            pub const fn new_unchecked(x: $ty) -> $name {
                $name(x)
            }

            pub fn new(x: $ty) -> Option<$name> {
                x.is_sign_positive().then_some($name(x))
            }

            pub const fn get(self) -> $ty {
                self.0
            }

            delegate_fn!($name, floor);
            delegate_fn!($name, ceil);
            delegate_fn!($name, round);
            delegate_fn!($name, trunc);
            delegate_fn!($name, fract);
            delegate_fn!($name, signum);
            delegate_fn!($name, powi, n, i32);
            delegate_fn!($name, powf, n, $ty);
            delegate_fn!($name, sqrt);
            delegate_fn!($name, exp);
            delegate_fn!($name, exp2);
            delegate_fn!($name, cbrt);
        }

        impl $name {
            delegate_const!(RADIX, u32, $ty);
            delegate_const!(MANTISSA_DIGITS, u32, $ty);
            delegate_const!(DIGITS, u32, $ty);
            delegate_const!(EPSILON, $name, wrap, $ty);
            pub const MIN: $name = $name(0.0);
            delegate_const!(MIN_POSITIVE, $name, wrap, $ty);
            delegate_const!(MAX, $name, wrap, $ty);
            delegate_const!(MIN_EXP, i32, $ty);
            delegate_const!(MAX_EXP, i32, $ty);
            delegate_const!(MIN_10_EXP, i32, $ty);
            delegate_const!(MAX_10_EXP, i32, $ty);
            delegate_const!(NAN, $name, wrap, $ty);
            delegate_const!(INFINITY, $name, wrap, $ty);

            delegate_fn!($name, is_nan; bool);
            delegate_fn!($name, is_infinite; bool);
            delegate_fn!($name, is_finite; bool);
            delegate_fn!($name, is_subnormal; bool);
            delegate_fn!($name, is_normal; bool);
            delegate_fn!($name, classify; FpCategory);
            delegate_fn!($name, recip);
            delegate_fn!($name, to_degrees);
            delegate_fn!($name, to_radians);
            delegate_fn!($name, max, other);
            delegate_fn!($name, min, other);
            delegate_fn!($name, to_bits; $int_ty);
            delegate_fn!($name, to_be_bytes; [u8; core::mem::size_of::<$ty>()]);
            delegate_fn!($name, to_le_bytes; [u8; core::mem::size_of::<$ty>()]);
            delegate_fn!($name, to_ne_bytes; [u8; core::mem::size_of::<$ty>()]);

            pub fn total_cmp(&self, other: &$name) -> Ordering {
                self.0.total_cmp(&other.0)
            }

            pub fn clamp(self, min: $name, max: $name) -> $name {
                $name(self.0.clamp(min.0, max.0))
            }
        }

        impl_binop!($name, Add, add, AddAssign, add_assign, +);
        impl_binop!($name, Sub, sub, SubAssign, sub_assign, -);
        impl_binop!($name, Mul, mul, MulAssign, mul_assign, *);
        impl_binop!($name, Div, div, DivAssign, div_assign, /);
        impl_binop!($name, Rem, rem, RemAssign, rem_assign, %);

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl fmt::LowerExp for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:e}", self.0)
            }
        }

        impl fmt::UpperExp for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:E}", self.0)
            }
        }

        impl std::str::FromStr for $name {
            type Err = ParseFloatError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                $name::new(s.parse()?).ok_or_else(|| <$ty>::from_str("-").err().unwrap())
            }
        }

        impl<'a> Sum<&'a $name> for $name {
            fn sum<I>(iter: I) -> Self
            where
                I: Iterator<Item = &'a $name>,
            {
                $name(iter.fold(0.0, |a, b| a + b.0))
            }
        }

        impl Sum<$name> for $name {
            fn sum<I>(iter: I) -> Self
            where
                I: Iterator<Item = $name>,
            {
                $name(iter.fold(0.0, |a, b| a + b.0))
            }
        }

        impl<'a> Product<&'a $name> for $name {
            fn product<I>(iter: I) -> Self
            where
                I: Iterator<Item = &'a $name>,
            {
                $name(iter.fold(1.0, |a, b| a * b.0))
            }
        }

        impl Product<$name> for $name {
            fn product<I>(iter: I) -> Self
            where
                I: Iterator<Item = $name>,
            {
                $name(iter.fold(1.0, |a, b| a * b.0))
            }
        }
    };
}

declare_unsigned_float!(uf32, f32, u32);
declare_unsigned_float!(uf64, f64, u64);
