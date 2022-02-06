use std::marker::PhantomData;
use std::ops::Deref;

use crate::index::{EdgeIndex, IndexType, VertexIndex};
use crate::infra::{CompactIndexMap, TypedBitSet, VisitSet};
use crate::marker::{Direction, EdgeType, Undirected};
use crate::storage::AdjList;
use crate::traits::*;
use crate::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesWeak, Neighbors, Vertices, VerticesBase,
    VerticesBaseWeak, VerticesWeak,
};

#[derive(
    Debug,
    VerticesBase,
    Vertices,
    EdgesBase,
    Edges,
    Neighbors,
    VerticesBaseWeak,
    VerticesWeak,
    EdgesBaseWeak,
    EdgesWeak,
)]
pub struct Path<V, E, S = AdjList<V, E, Undirected>> {
    #[graph]
    graph: S,
    ends: Option<[VertexIndex; 2]>,
    ty: PhantomData<(V, E)>,
}

#[derive(Debug)]
pub enum PathError {
    HigherDegree,
    Cycle,
    Disconnected,
}

impl<V, E, S> Path<V, E, S> {
    fn new_unchecked(graph: S, ends: Option<[VertexIndex; 2]>) -> Self {
        Self {
            graph,
            ends,
            ty: PhantomData,
        }
    }

    pub fn ends(&self) -> Option<[VertexIndex; 2]> {
        self.ends
    }
}

impl<V, E> Path<V, E> {
    pub fn new() -> Self {
        Self::new_unchecked(AdjList::new(), None)
    }
}

impl<V, E, S> Default for Path<V, E, S>
where
    S: Default,
{
    fn default() -> Self {
        Self {
            graph: S::default(),
            ends: None,
            ty: PhantomData,
        }
    }
}

impl<V, E, S> Path<V, E, S>
where
    S: Vertices<V> + Neighbors,
{
    fn check_runtime(graph: &S) -> Result<Option<[VertexIndex; 2]>, PathError> {
        let v = match graph.vertex_indices().next() {
            Some(v) => v,
            // Empty graph.
            None => return Ok(None),
        };

        let vertex_map = graph.vertex_index_map();
        let mut visited = TypedBitSet::with_capacity(vertex_map.len());

        visited.visit(v);

        let mut check_segment =
            |mut v: VertexIndex, mut prev: VertexIndex| -> Result<VertexIndex, PathError> {
                visited.visit(v);

                loop {
                    match graph.degree(v) {
                        1 => return Ok(v),
                        2 => {
                            let u = graph
                                .neighbors(v)
                                .find(|n| n.index() != prev)
                                .ok_or(PathError::Cycle)?
                                .index();

                            if visited.visit(u) {
                                prev = v;
                                v = u;
                            } else {
                                return Err(PathError::Cycle);
                            }
                        }
                        _ => return Err(PathError::HigherDegree),
                    }
                }
            };

        // Based on what vertex we picked, check the rest of the path from an
        // end or both segments from the middle.
        let ends = match graph.degree(v) {
            0 => {
                // Isolated vertex.
                [v, v]
            }
            1 => {
                let u = check_segment(graph.neighbors(v).next().unwrap().index(), v)?;
                [v, u]
            }
            2 => {
                let mut ends = [VertexIndex::null(); 2];
                for (u, end) in graph.neighbors(v).zip(ends.iter_mut()) {
                    *end = check_segment(u.index(), v)?;
                }
                ends
            }
            _ => return Err(PathError::HigherDegree),
        };

        if visited.visited_count() == graph.vertex_count() {
            Ok(Some(ends))
        } else {
            Err(PathError::Disconnected)
        }
    }
}

impl<V, E, S> Path<V, E, S>
where
    S: VerticesMut<V> + EdgesMut<E, Undirected> + Neighbors,
{
    pub fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V> {
        self.graph.vertex_mut(index)
    }

    pub fn try_add_vertex(
        &mut self,
        vertex: V,
        edge: Option<E>,
        end: VertexIndex,
    ) -> Result<VertexIndex, PathError> {
        match self.ends.as_mut() {
            Some(ends) => {
                let end = match ends {
                    // Match the second end first so that it gets replaced by
                    // the new vertex. This is just for satisfying irrational
                    // feeling that growing the path from the isolated vertex
                    // should change the "second" end, not the "first".
                    [_, u] | [u, _] if *u == end => u,
                    _ => {
                        let error = if self.graph.vertex_indices().any(|v| v == end) {
                            PathError::HigherDegree
                        } else {
                            PathError::Disconnected
                        };
                        return Err(error);
                    }
                };

                let edge = edge.ok_or(PathError::Disconnected)?;

                let v = self.graph.add_vertex(vertex);
                self.graph.add_edge(*end, v, edge);
                *end = v;

                Ok(v)
            }
            None => {
                let v = self.graph.add_vertex(vertex);
                self.ends = Some([v, v]);
                Ok(v)
            }
        }
    }

    pub fn remove_vertex(&mut self, index: VertexIndex, edge: Option<E>) -> Option<V> {
        match self.ends.as_mut() {
            Some(ends) if ends[0] == ends[1] => {
                if ends[0] == index {
                    self.ends = None;
                    self.graph.remove_vertex(index)
                } else {
                    None
                }
            }
            Some(ends) => match ends {
                [end, _] | [_, end] if *end == index => {
                    // The removed vertex is an end.
                    *end = self.graph.neighbors(index).next().unwrap().index();
                    self.graph.remove_vertex(index)
                }
                _ => {
                    // The removed vertex is an inner vertex.
                    let (u, v) = {
                        let mut neighbors = self.graph.neighbors(index);
                        let u = neighbors.next().unwrap().index();
                        let v = neighbors.next().unwrap().index();
                        (u, v)
                    };

                    let edge = edge.unwrap_or_else(|| {
                        // An edge to connect vertices was not provided, we will
                        // reuse an edge that was between the removed vertex and
                        // one of its neighbors.
                        let e = self.graph.neighbors(index).next().unwrap().edge();
                        self.graph.remove_edge(e).unwrap()
                    });

                    // Connect the neighbors of the removed vertex.
                    self.graph.add_edge(u, v, edge);

                    self.graph.remove_vertex(index)
                }
            },
            None => None,
        }
    }

    pub fn replace_vertex(&mut self, index: VertexIndex, vertex: V) -> V {
        self.graph.replace_vertex(index, vertex)
    }

    pub fn clear(&mut self) {
        self.graph.clear();
        self.ends = None;
    }

    pub fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E> {
        self.graph.edge_mut(index)
    }

    pub fn replace_edge(&mut self, index: EdgeIndex, edge: E) -> E {
        self.graph.replace_edge(index, edge)
    }
}

impl<V, E, S> Path<V, E, S>
where
    E: Default,
    S: VerticesMut<V> + EdgesMut<E, Undirected> + Neighbors,
{
    pub fn add_vertex(&mut self, vertex: V, end: VertexIndex) -> VertexIndex {
        // Check if end is valid. If not, ignore the passed value and pick an
        // arbitrary real end.
        let end = match self.ends {
            // Provided end is valid.
            Some([u, _]) | Some([_, u]) if u == end => end,
            // Provided end is invalid, pick an arbitrary end.
            Some([u, _]) => u,
            // The graph is empty.
            None => VertexIndex::null(),
        };

        // We made sure that we provide all necessary, correct inputs so that
        // `try_add_vertex` cannot fail.
        self.try_add_vertex(vertex, Some(E::default()), end)
            .unwrap()
    }
}

impl<V, E, S> Guarantee for Path<V, E, S> {
    fn is_loop_free() -> bool {
        true
    }

    fn has_paths_only() -> bool {
        true
    }

    fn is_connected() -> bool {
        true
    }
}

impl<V, E, S> Constrained<S> for Path<V, E, S>
where
    S: Vertices<V> + Neighbors + Guarantee,
{
    type Error = PathError;

    fn check(graph: &S) -> Result<(), Self::Error> {
        // Statically guaranteed.
        if S::has_paths_only() && S::is_connected() {
            return Ok(());
        }

        Self::check_runtime(graph).map(|_| ())
    }

    fn constrain(graph: S) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Self::check_runtime(&graph).map(|ends| Self::new_unchecked(graph, ends))
    }
}

impl<V, E, S> Deref for Path<V, E, S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::storage::AdjList;

    use super::*;

    #[test]
    fn check_empty() {
        let graph: AdjList<(), (), Undirected> = AdjList::new();
        assert!(Path::<(), ()>::check(&graph).is_ok());
    }

    #[test]
    fn check_isolated() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        graph.add_vertex(());

        assert!(Path::<(), ()>::check(&graph).is_ok());
    }

    #[test]
    fn check_from_end() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());

        assert!(Path::<(), ()>::check(&graph).is_ok());
    }

    #[test]
    fn check_from_middle() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v0, v2, ());

        assert!(Path::<(), ()>::check(&graph).is_ok());
    }

    #[test]
    fn check_from_end_higher_degree() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::HigherDegree));
    }

    #[test]
    fn check_from_middle_higher_degree() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v0, v2, ());
        graph.add_edge(v0, v3, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::HigherDegree));
    }

    #[test]
    fn check_from_any_higher_degree() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v0, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::HigherDegree));
    }

    #[test]
    fn check_cycle() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v2, v3, ());
        graph.add_edge(v3, v0, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::Cycle));
    }

    #[test]
    fn check_multigraph() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v2, v1, ());
        graph.add_edge(v2, v3, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::HigherDegree));
    }

    #[test]
    fn check_disconnected() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v2, v3, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::Disconnected));
    }

    #[test]
    fn check_self_loop() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v1, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::HigherDegree));
    }

    #[test]
    fn check_self_loop_isolated() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());

        graph.add_edge(v0, v0, ());

        assert_matches!(Path::<(), ()>::check(&graph), Err(PathError::Cycle));
    }

    #[test]
    fn try_add_vertex_middle() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());

        let mut path = Path::<(), ()>::constrain(graph).unwrap();

        assert_matches!(
            path.try_add_vertex((), None, v1),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn try_add_vertex_non_existent() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());

        let mut path = Path::<(), ()>::constrain(graph).unwrap();

        let u = VertexIndex::from(42);

        assert_matches!(
            path.try_add_vertex((), None, u),
            Err(PathError::Disconnected)
        );
    }

    #[test]
    fn try_add_vertex_no_edge() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());

        let mut path = Path::<(), ()>::constrain(graph).unwrap();

        assert_matches!(
            path.try_add_vertex((), None, v0),
            Err(PathError::Disconnected)
        );
    }

    #[test]
    fn try_add_vertex_empty() {
        let mut path = Path::<(), ()>::new();

        let result = path.try_add_vertex((), None, VertexIndex::null());
        assert!(result.is_ok());

        let v = result.unwrap();
        assert!(path.ends().is_some());
        assert_eq!(path.ends().unwrap()[0], v);
        assert_eq!(path.ends().unwrap()[1], v);
    }

    #[test]
    fn try_add_vertex_isolated() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());

        let mut path = Path::<(), ()>::constrain(graph).unwrap();

        let result = path.try_add_vertex((), Some(()), v0);
        assert!(result.is_ok());

        let v = result.unwrap();
        assert!(path.ends().is_some());
        assert_eq!(path.ends().unwrap()[0], v0);
        assert_eq!(path.ends().unwrap()[1], v);

        assert!(path.edge_index(v0, v).is_some());
    }

    #[test]
    fn try_add_vertex() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());

        let mut path = Path::<(), ()>::constrain(graph).unwrap();

        let [v0, v1] = path.ends().unwrap();

        let result = path.try_add_vertex((), Some(()), v1);
        assert!(result.is_ok());

        let v = result.unwrap();
        assert!(path.ends().is_some());
        assert_eq!(path.ends().unwrap()[0], v0);
        assert_eq!(path.ends().unwrap()[1], v);

        assert!(path.edge_index(v1, v).is_some());
    }
}
