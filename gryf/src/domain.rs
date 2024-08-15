//! Collection of various graph types implementations.
//!
//! These types usually wrap a [storage](crate::storage) and provide additional,
//! higher-level semantics and type-level guarantees.

mod generic;
mod path;

pub use generic::Graph;
pub use path::{Path, PathError};
