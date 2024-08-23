//! Collection of graph storage encapsulations with high-level API.
//!
//! These types usually wrap a [storage](crate::storage) and provide additional,
//! higher-level semantics and type-level guarantees.

mod generic;
mod path;

pub use generic::Graph;
pub use path::{Path, PathError};
