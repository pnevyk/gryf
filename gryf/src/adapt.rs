//! Various graph adapters.

pub mod cast;
pub mod complement;
pub mod subgraph;
pub mod transpose;
pub mod undirect;

#[doc(inline)]
pub use complement::Complement;
#[doc(inline)]
pub use subgraph::Subgraph;
#[doc(inline)]
pub use transpose::Transpose;
#[doc(inline)]
pub use undirect::Undirect;
