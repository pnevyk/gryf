use std::marker::PhantomData;
use std::ops::Deref;

use crate::index::{EdgeIndex, IndexType, VertexIndex};
use crate::infra::{CompactIndexMap, TypedBitSet, VisitSet};
use crate::marker::{Directed, Direction, EdgeType, Undirected};
use crate::storage::{AdjList, Frozen, Stable};
use crate::traits::*;
use crate::{
    Edges, EdgesBase, EdgesBaseWeak, EdgesWeak, Neighbors, Vertices, VerticesBase,
    VerticesBaseWeak, VerticesWeak,
};

use super::Graph;

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
pub struct Path<V, E, Ty: EdgeType, G> {
    #[graph]
    graph: G,
    ends: Option<[VertexIndex; 2]>,
    ty: PhantomData<(V, E, Ty)>,
}

#[derive(Debug)]
pub enum PathError {
    HigherDegree,
    Cycle,
    Disconnected,
    Direction,
}

impl<V, E, Ty: EdgeType> Path<V, E, Ty, AdjList<V, E, Ty>> {
    pub fn new() -> Self {
        Self::new_unchecked(AdjList::new(), None)
    }

    pub fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
        Self::new_unchecked(AdjList::with_capacity(vertex_count, edge_count), None)
    }
}

impl<V, E> Path<V, E, Undirected, AdjList<V, E, Undirected>> {
    pub fn new_undirected() -> Self {
        Self::new_unchecked(AdjList::new(), None)
    }
}

impl<V, E> Path<V, E, Directed, AdjList<V, E, Directed>> {
    pub fn new_directed() -> Self {
        Self::new_unchecked(AdjList::new(), None)
    }
}

impl<V, E, Ty: EdgeType, G> Default for Path<V, E, Ty, G>
where
    G: Default,
{
    fn default() -> Self {
        Self::new_unchecked(G::default(), None)
    }
}

impl<V, E, Ty: EdgeType, G> Path<V, E, Ty, G> {
    fn new_unchecked(graph: G, ends: Option<[VertexIndex; 2]>) -> Self {
        Self {
            graph,
            ends,
            ty: PhantomData,
        }
    }

    fn check_runtime(graph: &G) -> Result<Option<[VertexIndex; 2]>, PathError>
    where
        G: Vertices<V> + Neighbors,
    {
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
        let mut ends = match graph.degree(v) {
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

        if visited.visited_count() != graph.vertex_count() {
            return Err(PathError::Disconnected);
        }

        if Ty::is_directed() {
            let start = if graph.degree_directed(ends[0], Direction::Outgoing) == 1 {
                ends[0]
            } else if graph.degree_directed(ends[1], Direction::Outgoing) == 1 {
                // In directed paths, the first end is always with in-degree 0
                // and the second end is with out-degree 0.
                ends.swap(0, 1);
                ends[0]
            } else {
                return Err(PathError::Direction);
            };

            visited.clear();

            let mut vertex = Some(start);
            while let Some(v) = vertex {
                visited.visit(v);
                vertex = graph
                    .neighbors_directed(v, Direction::Outgoing)
                    .next()
                    .map(|n| n.index());
            }

            if visited.visited_count() != graph.vertex_count() {
                return Err(PathError::Direction);
            }
        }

        Ok(Some(ends))
    }

    pub fn vertex_count(&self) -> usize
    where
        G: VerticesBase,
    {
        self.graph.vertex_count()
    }

    pub fn vertex_bound(&self) -> usize
    where
        G: VerticesBase,
    {
        self.graph.vertex_bound()
    }

    pub fn vertex_indices(&self) -> G::VertexIndicesIter<'_>
    where
        G: VerticesBase,
    {
        self.graph.vertex_indices()
    }

    pub fn vertex(&self, index: VertexIndex) -> Option<&V>
    where
        G: Vertices<V>,
    {
        self.graph.vertex(index)
    }

    pub fn vertices(&self) -> G::VerticesIter<'_, V>
    where
        G: Vertices<V>,
    {
        self.graph.vertices()
    }

    pub fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V>
    where
        G: VerticesMut<V>,
    {
        self.graph.vertex_mut(index)
    }

    pub fn try_add_vertex(
        &mut self,
        vertex: V,
        edge: Option<E>,
        end: VertexIndex,
    ) -> Result<VertexIndex, PathError>
    where
        G: VerticesMut<V> + EdgesMut<E, Ty> + Neighbors,
    {
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

                let u = *end;
                let v = self.graph.add_vertex(vertex);
                *end = v;

                let (u, v) = if Ty::is_directed() {
                    // For directed graph, the edge must have the correct
                    // direction.
                    if u == ends[0] {
                        (v, u)
                    } else {
                        (u, v)
                    }
                } else {
                    (u, v)
                };

                self.graph.add_edge(u, v, edge);

                Ok(v)
            }
            None => {
                let v = self.graph.add_vertex(vertex);
                self.ends = Some([v, v]);
                Ok(v)
            }
        }
    }

    pub fn add_vertex(&mut self, vertex: V, end: VertexIndex) -> VertexIndex
    where
        E: Default,
        G: VerticesMut<V> + EdgesMut<E, Ty> + Neighbors,
    {
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

    pub fn remove_vertex(&mut self, index: VertexIndex, edge: Option<E>) -> Option<V>
    where
        G: VerticesMut<V> + EdgesMut<E, Ty> + Neighbors,
    {
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
                    let (u, v) = if Ty::is_directed() {
                        let u = self
                            .graph
                            .neighbors_directed(index, Direction::Incoming)
                            .next()
                            .unwrap()
                            .index();
                        let v = self
                            .graph
                            .neighbors_directed(index, Direction::Outgoing)
                            .next()
                            .unwrap()
                            .index();
                        (u, v)
                    } else {
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

    pub fn replace_vertex(&mut self, index: VertexIndex, vertex: V) -> V
    where
        G: VerticesMut<V>,
    {
        self.graph.replace_vertex(index, vertex)
    }

    pub fn clear(&mut self)
    where
        G: VerticesMut<V>,
    {
        self.graph.clear();
        self.ends = None;
    }

    pub fn edge_count(&self) -> usize
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_count()
    }

    pub fn edge_bound(&self) -> usize
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_bound()
    }

    pub fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)>
    where
        G: EdgesBase<Ty>,
    {
        self.graph.endpoints(index)
    }

    pub fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex>
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_index(src, dst)
    }

    pub fn edge_indices(&self) -> G::EdgeIndicesIter<'_>
    where
        G: EdgesBase<Ty>,
    {
        self.graph.edge_indices()
    }

    pub fn contains_edge(&self, index: EdgeIndex) -> bool
    where
        G: EdgesBase<Ty>,
    {
        self.graph.contains_edge(index)
    }

    pub fn is_directed(&self) -> bool
    where
        G: EdgesBase<Ty>,
    {
        self.graph.is_directed()
    }

    pub fn edge(&self, index: EdgeIndex) -> Option<&E>
    where
        G: Edges<E, Ty>,
    {
        self.graph.edge(index)
    }

    pub fn edges(&self) -> G::EdgesIter<'_, E>
    where
        G: Edges<E, Ty>,
    {
        self.graph.edges()
    }

    pub fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E>
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.edge_mut(index)
    }

    pub fn replace_edge(&mut self, index: EdgeIndex, edge: E) -> E
    where
        G: EdgesMut<E, Ty>,
    {
        self.graph.replace_edge(index, edge)
    }

    pub fn neighbors(&self, src: VertexIndex) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
    {
        self.graph.neighbors(src)
    }

    pub fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
    {
        self.graph.neighbors_directed(src, dir)
    }

    pub fn degree(&self, src: VertexIndex) -> usize
    where
        G: Neighbors,
    {
        self.graph.degree(src)
    }

    pub fn degree_directed(&self, src: VertexIndex, dir: Direction) -> usize
    where
        G: Neighbors,
    {
        self.graph.degree_directed(src, dir)
    }

    pub fn ends(&self) -> Option<[VertexIndex; 2]> {
        self.ends
    }

    pub fn stabilize(self) -> Path<V, E, Ty, Stable<G>> {
        Path::new_unchecked(Stable::new(self.graph), self.ends)
    }

    pub fn freeze(self) -> Path<V, E, Ty, Frozen<G>> {
        Path::new_unchecked(Frozen::new(self.graph), self.ends)
    }
}

impl<V, E, Ty: EdgeType, G> From<Path<V, E, Ty, G>> for Graph<V, E, Ty, G> {
    fn from(path: Path<V, E, Ty, G>) -> Self {
        Graph::with_storage(path.graph)
    }
}

impl<V, E, Ty: EdgeType, G> Constrained<G> for Path<V, E, Ty, G>
where
    G: Vertices<V> + Neighbors + Guarantee,
{
    type Error = PathError;

    fn check(graph: &G) -> Result<(), Self::Error> {
        // Statically guaranteed.
        if G::has_paths_only() && G::is_connected() {
            return Ok(());
        }

        Self::check_runtime(graph).map(|_| ())
    }

    fn constrain(graph: G) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Self::check_runtime(&graph).map(|ends| Self::new_unchecked(graph, ends))
    }
}

impl<V, E, Ty: EdgeType, G> Deref for Path<V, E, Ty, G> {
    type Target = G;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl<V, E, Ty: EdgeType, G> Guarantee for Path<V, E, Ty, G> {
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

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::storage::AdjList;

    use super::*;

    #[test]
    fn check_empty() {
        let graph: AdjList<(), (), Undirected> = AdjList::new();
        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_isolated() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        graph.add_vertex(());

        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_from_end() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());

        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_from_middle() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v0, v2, ());

        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_directed() {
        let mut graph: AdjList<(), (), Directed> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v2, v1, ());
        graph.add_edge(v1, v0, ());

        let path = Path::<(), (), Directed, _>::constrain(graph);
        assert!(path.is_ok());
        assert_eq!(path.unwrap().ends(), Some([v2, v0]));
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

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
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

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
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

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
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

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::Cycle)
        );
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

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
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

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::Disconnected)
        );
    }

    #[test]
    fn check_self_loop() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v1, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn check_self_loop_isolated() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());

        graph.add_edge(v0, v0, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::Cycle)
        );
    }

    #[test]
    fn check_direction() {
        let mut graph: AdjList<(), (), Directed> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v2, v1, ());
        graph.add_edge(v2, v3, ());

        assert_matches!(
            Path::<(), (), Directed, _>::check(&graph),
            Err(PathError::Direction)
        );
    }

    #[test]
    fn check_direction_ends() {
        let mut graph: AdjList<(), (), Directed> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v1, v0, ());
        graph.add_edge(v1, v2, ());

        assert_matches!(
            Path::<(), (), Directed, _>::check(&graph),
            Err(PathError::Direction)
        );
    }

    #[test]
    fn try_add_vertex_middle() {
        let mut graph: AdjList<(), (), Undirected> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

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

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

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

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

        assert_matches!(
            path.try_add_vertex((), None, v0),
            Err(PathError::Disconnected)
        );
    }

    #[test]
    fn try_add_vertex_empty() {
        let mut path = Path::<(), (), Undirected, _>::new();

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

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

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

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

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
