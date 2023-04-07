use super::{edges::EdgesMut, marker::EdgeType, vertices::VerticesMut};

pub trait ConnectVertices<V, E, Ty: EdgeType>: VerticesMut<V> + EdgesMut<E, Ty> {
    fn connect_vertices<F>(&mut self, connect: F)
    where
        // NOTE: Ideally the API here would be `F: FnMut(Self::VertexRef<'_>, Self::VertexRef<'_>) -> Option<E>`,
        // but the issue is that on the caller's side that would require `V: 'static`
        // due to a known limitation of the current borrow checker
        // (https://blog.rust-lang.org/2022/10/28/gats-stabilization.html#implied-static-requirement-from-higher-ranked-trait-bounds).
        F: FnMut(&V, &V) -> Option<E>;
}
