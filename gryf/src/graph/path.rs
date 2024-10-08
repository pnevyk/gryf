use std::{
    borrow::Borrow,
    hash::BuildHasherDefault,
    marker::PhantomData,
    ops::{Deref, Index, IndexMut},
};

use rustc_hash::FxHashSet;
use thiserror::Error;

use crate::{
    core::{
        base::NeighborReference,
        create::Create,
        error::{AddEdgeError, AddVertexError, ReplaceEdgeError, ReplaceVertexError},
        id::{AsIdRef, DefaultId, IntegerIdType},
        marker::{Directed, Direction, EdgeType, Undirected},
        props::{Constrained, Guarantee},
        EdgeSet, GraphAdd, GraphBase, GraphFull, GraphMut, GraphRef, Neighbors, VertexSet,
    },
    storage::{AdjList, Frozen, Stable},
    visit::VisitSet,
};

use gryf_derive::{EdgeSet, GraphBase, GraphMut, GraphRef, Neighbors, VertexSet};

use super::generic::Graph;

#[derive(Debug, Clone, GraphBase, Neighbors, VertexSet, EdgeSet, GraphRef, GraphMut)]
#[gryf_crate]
pub struct Path<V, E, Ty: EdgeType, G = AdjList<V, E, Ty, DefaultId>>
where
    G: GraphBase,
{
    #[graph]
    storage: G,
    ends: Option<[G::VertexId; 2]>,
    ty: PhantomData<(V, E, Ty)>,
}

#[derive(Debug, Error)]
pub enum PathError<V, E> {
    #[error("the graph has higher degree than valid")]
    HigherDegree,
    #[error("the graph contains cycle")]
    Cycle,
    #[error("the graph is not connected")]
    Disconnected,
    #[error("the path has mixed direction")]
    Direction,
    #[error("{0}")]
    AddVertex(AddVertexError<V>),
    #[error("{0}")]
    AddEdge(AddEdgeError<E>),
}

impl<V, E> From<AddVertexError<V>> for PathError<V, E> {
    fn from(error: AddVertexError<V>) -> Self {
        PathError::AddVertex(error)
    }
}

impl<V, E> From<AddEdgeError<E>> for PathError<V, E> {
    fn from(error: AddEdgeError<E>) -> Self {
        PathError::AddEdge(error)
    }
}

impl<V, E, Ty: EdgeType> Path<V, E, Ty> {
    pub fn new() -> Self {
        Self::new_unchecked(AdjList::new(), None)
    }

    pub fn with_capacity(vertex_capacity: usize, edge_capacity: usize) -> Self {
        Self::new_unchecked(AdjList::with_capacity(vertex_capacity, edge_capacity), None)
    }
}

impl<V, E> Path<V, E, Undirected> {
    pub fn new_undirected() -> Self {
        Self::new_unchecked(AdjList::new(), None)
    }
}

impl<V, E, G> Path<V, E, Undirected, G>
where
    G: GraphBase<EdgeType = Undirected> + Neighbors + VertexSet + Guarantee,
{
    pub fn new_undirected_in(storage: G) -> Result<Self, PathError<V, E>> {
        Self::new_in(storage)
    }
}

impl<V, E> Path<V, E, Directed, AdjList<V, E, Directed, DefaultId>> {
    pub fn new_directed() -> Self {
        Self::new_unchecked(AdjList::new(), None)
    }
}

impl<V, E, G> Path<V, E, Directed, G>
where
    G: GraphBase<EdgeType = Directed> + Neighbors + VertexSet + Guarantee,
{
    pub fn new_directed_in(storage: G) -> Result<Self, PathError<V, E>> {
        Self::new_in(storage)
    }
}

impl<V, E, G> Default for Path<V, E, G::EdgeType, G>
where
    G: GraphBase + Default,
{
    fn default() -> Self {
        Self::new_unchecked(G::default(), None)
    }
}

impl<V, E, G> Path<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
    fn new_unchecked(storage: G, ends: Option<[G::VertexId; 2]>) -> Self {
        Self {
            storage,
            ends,
            ty: PhantomData,
        }
    }

    fn check_runtime(storage: &G) -> Result<Option<[G::VertexId; 2]>, PathError<V, E>>
    where
        G: Neighbors + VertexSet,
    {
        let v = match storage.vertices_by_id().next() {
            Some(v) => v,
            // Empty graph.
            None => return Ok(None),
        };

        let mut visited = FxHashSet::with_capacity_and_hasher(
            storage.vertex_count(),
            BuildHasherDefault::default(),
        );

        visited.visit(v.clone());

        let mut check_segment =
            |mut v: G::VertexId, mut prev: G::VertexId| -> Result<G::VertexId, PathError<V, E>> {
                visited.visit(v.clone());

                loop {
                    match storage.degree_undirected(&v) {
                        1 => return Ok(v),
                        2 => {
                            let u = storage
                                .neighbors_undirected(&v)
                                .find(|n| n.id().as_ref() != &prev)
                                .ok_or(PathError::Cycle)?
                                .id()
                                .into_owned();

                            if visited.visit(u.clone()) {
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
        let mut ends = match storage.degree_undirected(&v) {
            0 => {
                // Isolated vertex.
                [v.clone(), v]
            }
            1 => {
                let u = check_segment(
                    storage
                        .neighbors_undirected(&v)
                        .next()
                        .unwrap()
                        .id()
                        .into_owned(),
                    v.clone(),
                )?;
                [v, u]
            }
            2 => {
                let mut iter = storage.neighbors_undirected(&v);
                let u = iter.next().unwrap();
                let u1 = check_segment(u.id().into_owned(), v.clone())?;
                let u = iter.next().unwrap();
                let u2 = check_segment(u.id().into_owned(), v)?;
                [u1, u2]
            }
            _ => return Err(PathError::HigherDegree),
        };

        if visited.visited_count() != storage.vertex_count() {
            return Err(PathError::Disconnected);
        }

        if storage.is_directed() {
            let start = if storage.degree_directed(&ends[0], Direction::Outgoing) == 1 {
                ends[0].clone()
            } else if storage.degree_directed(&ends[1], Direction::Outgoing) == 1 {
                // In directed paths, the first end is always with in-degree 0
                // and the second end is with out-degree 0.
                ends.swap(0, 1);
                ends[0].clone()
            } else {
                return Err(PathError::Direction);
            };

            visited.clear();

            let mut vertex = Some(start);
            while let Some(v) = vertex {
                vertex = storage
                    .neighbors_directed(&v, Direction::Outgoing)
                    .next()
                    .map(|n| n.id().into_owned());
                visited.visit(v);
            }

            if visited.visited_count() != storage.vertex_count() {
                return Err(PathError::Direction);
            }
        }

        Ok(Some(ends))
    }

    pub fn new_in(storage: G) -> Result<Self, PathError<V, E>>
    where
        G: Neighbors + VertexSet + Guarantee,
    {
        Self::constrain(storage)
    }

    pub fn vertex_count(&self) -> usize
    where
        G: VertexSet,
    {
        self.storage.vertex_count()
    }

    pub fn vertex_count_hint(&self) -> Option<usize>
    where
        G: GraphBase,
    {
        self.storage.vertex_count_hint()
    }

    pub fn vertex_bound(&self) -> usize
    where
        G: VertexSet,
        G::VertexId: IntegerIdType,
    {
        self.storage.vertex_bound()
    }

    pub fn vertices_by_id(&self) -> G::VerticesByIdIter<'_>
    where
        G: VertexSet,
    {
        self.storage.vertices_by_id()
    }

    pub fn contains_vertex<VI>(&self, id: VI) -> bool
    where
        G: GraphRef<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.contains_vertex(id.as_id().as_ref())
    }

    pub fn vertex<VI>(&self, id: VI) -> Option<&V>
    where
        G: GraphRef<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.vertex(id.as_id().as_ref())
    }

    pub fn vertices(&self) -> G::VerticesIter<'_>
    where
        G: GraphRef<V, E>,
    {
        self.storage.vertices()
    }

    pub fn find_vertex<Q>(&self, vertex: Q) -> Option<G::VertexId>
    where
        G: GraphRef<V, E>,
        V: Eq,
        Q: Borrow<V>,
    {
        self.storage.find_vertex(vertex.borrow())
    }

    pub fn vertex_mut<VI>(&mut self, id: VI) -> Option<&mut V>
    where
        G: GraphMut<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.vertex_mut(id.as_id().as_ref())
    }

    pub fn try_add_vertex<VI>(
        &mut self,
        vertex: V,
        edge: Option<E>,
        end: VI,
    ) -> Result<G::VertexId, PathError<V, E>>
    where
        G: Neighbors + GraphAdd<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        let end = end.as_id();
        let end = end.as_ref();

        match self.ends.as_mut() {
            Some(ends) => {
                let end = match ends {
                    // Match the second end first so that it gets replaced by
                    // the new vertex. This is just for satisfying irrational
                    // feeling that growing the path from the isolated vertex
                    // should change the "second" end, not the "first".
                    [_, u] | [u, _] if u == end => u,
                    _ => {
                        let error = if self.storage.vertices_by_id().any(|v| &v == end) {
                            PathError::HigherDegree
                        } else {
                            PathError::Disconnected
                        };
                        return Err(error);
                    }
                };

                let edge = edge.ok_or(PathError::Disconnected)?;

                let u = end.clone();
                let v = self.storage.try_add_vertex(vertex)?;
                *end = v.clone();

                // For directed graph, the edge must have the correct
                // direction.
                let reverse = self.storage.is_directed() && u == ends[0];

                if reverse {
                    self.storage.try_add_edge(&v, &u, edge)?;
                } else {
                    self.storage.try_add_edge(&u, &v, edge)?;
                }

                Ok(v)
            }
            None => {
                let v = self.storage.try_add_vertex(vertex)?;
                self.ends = Some([v.clone(), v.clone()]);
                Ok(v)
            }
        }
    }

    pub fn add_vertex<VI>(&mut self, vertex: V, end: VI) -> G::VertexId
    where
        E: Default,
        G: Neighbors + GraphAdd<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        let end = end.as_id();
        let end = end.as_ref();

        // Check if end is valid. If not, ignore the passed value and pick an
        // arbitrary real end.
        let end = match self.ends.as_ref() {
            // Provided end is valid.
            Some([u, _]) | Some([_, u]) if u == end => end.clone(),
            // Provided end is invalid, pick an arbitrary end.
            Some([u, _]) => u.clone(),
            // The graph is empty. We can use whatever end because it will *not*
            // be used in `try_add_vertex` anyway.
            None => end.clone(),
        };

        // We made sure that we provide all necessary, correct inputs so that
        // `try_add_vertex` cannot fail (unless underlying storage fails).
        match self.try_add_vertex(vertex, Some(E::default()), &end) {
            Ok(id) => id,
            Err(error) => panic!("{error}"),
        }
    }

    pub fn remove_vertex<VI>(&mut self, id: VI, edge: Option<E>) -> Option<V>
    where
        G: Neighbors + GraphFull<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        let id = id.as_id();
        let id = id.as_ref();

        match self.ends.as_mut() {
            Some(ends) if ends[0] == ends[1] => {
                if &ends[0] == id {
                    self.ends = None;
                    self.storage.remove_vertex(id)
                } else {
                    None
                }
            }
            Some(ends) => match ends {
                [end, _] | [_, end] if end == id => {
                    // The removed vertex is an end.
                    *end = self
                        .storage
                        .neighbors_undirected(id)
                        .next()
                        .unwrap()
                        .id()
                        .into_owned();
                    self.storage.remove_vertex(id)
                }
                _ => {
                    // The removed vertex is an inner vertex.
                    let (u, v) = if self.is_directed() {
                        let u = self
                            .storage
                            .neighbors_directed(id, Direction::Incoming)
                            .next()
                            .unwrap()
                            .id()
                            .into_owned();
                        let v = self
                            .storage
                            .neighbors_directed(id, Direction::Outgoing)
                            .next()
                            .unwrap()
                            .id()
                            .into_owned();
                        (u, v)
                    } else {
                        let mut neighbors = self.storage.neighbors_undirected(id);
                        let u = neighbors.next().unwrap().id().into_owned();
                        let v = neighbors.next().unwrap().id().into_owned();
                        (u, v)
                    };

                    let edge = edge.unwrap_or_else(|| {
                        // An edge to connect vertices was not provided, we will
                        // reuse an edge that was between the removed vertex and
                        // one of its neighbors.
                        let e = self
                            .storage
                            .neighbors_undirected(id)
                            .next()
                            .unwrap()
                            .edge()
                            .into_owned();
                        self.storage.remove_edge(&e).unwrap()
                    });

                    // Connect the neighbors of the removed vertex.
                    self.storage.add_edge(&u, &v, edge);

                    self.storage.remove_vertex(id)
                }
            },
            None => None,
        }
    }

    pub fn replace_vertex<VI>(&mut self, id: VI, vertex: V) -> V
    where
        G: GraphMut<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.replace_vertex(id.as_id().as_ref(), vertex)
    }

    pub fn try_replace_vertex<VI>(&mut self, id: VI, vertex: V) -> Result<V, ReplaceVertexError<V>>
    where
        G: GraphMut<V, E>,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.try_replace_vertex(id.as_id().as_ref(), vertex)
    }

    pub fn clear(&mut self)
    where
        G: GraphFull<V, E>,
    {
        self.storage.clear();
        self.ends = None;
    }

    pub fn edge_count(&self) -> usize
    where
        G: EdgeSet,
    {
        self.storage.edge_count()
    }

    pub fn edge_count_hint(&self) -> Option<usize>
    where
        G: GraphBase,
    {
        self.storage.edge_count_hint()
    }

    pub fn edge_bound(&self) -> usize
    where
        G: EdgeSet,
        G::EdgeId: IntegerIdType,
    {
        self.storage.edge_bound()
    }

    pub fn endpoints<EI>(&self, id: EI) -> Option<(G::VertexId, G::VertexId)>
    where
        G: EdgeSet,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.endpoints(id.as_id().as_ref())
    }

    pub fn edge_id<VI>(&self, from: VI, to: VI) -> G::EdgeIdIter<'_>
    where
        G: EdgeSet,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .edge_id(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn edge_id_any<VI>(&self, from: VI, to: VI) -> Option<G::EdgeId>
    where
        G: EdgeSet,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .edge_id_any(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn edges_by_id(&self) -> G::EdgesByIdIter<'_>
    where
        G: EdgeSet,
    {
        self.storage.edges_by_id()
    }

    pub fn contains_edge<EI>(&self, id: EI) -> bool
    where
        G: EdgeSet,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.contains_edge(id.as_id().as_ref())
    }

    pub fn contains_edge_between<VI>(&self, from: VI, to: VI) -> bool
    where
        G: EdgeSet,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage
            .contains_edge_between(from.as_id().as_ref(), to.as_id().as_ref())
    }

    pub fn is_directed(&self) -> bool
    where
        G: GraphBase,
    {
        self.storage.is_directed()
    }

    pub fn edge<EI>(&self, id: EI) -> Option<&E>
    where
        G: GraphRef<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.edge(id.as_id().as_ref())
    }

    pub fn edges(&self) -> G::EdgesIter<'_>
    where
        G: GraphRef<V, E>,
    {
        self.storage.edges()
    }

    pub fn edge_mut<EI>(&mut self, id: EI) -> Option<&mut E>
    where
        G: GraphMut<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.edge_mut(id.as_id().as_ref())
    }

    pub fn replace_edge<EI>(&mut self, id: EI, edge: E) -> E
    where
        G: GraphMut<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.replace_edge(id.as_id().as_ref(), edge)
    }

    pub fn try_replace_edge<EI>(&mut self, id: EI, edge: E) -> Result<E, ReplaceEdgeError<E>>
    where
        G: GraphMut<V, E>,
        EI: AsIdRef<G::EdgeId>,
    {
        self.storage.try_replace_edge(id.as_id().as_ref(), edge)
    }

    pub fn neighbors_undirected<VI>(&self, from: VI) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.neighbors_undirected(from.as_id().as_ref())
    }

    pub fn neighbors_directed<VI>(&self, from: VI, dir: Direction) -> G::NeighborsIter<'_>
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.neighbors_directed(from.as_id().as_ref(), dir)
    }

    pub fn degree_undirected<VI>(&self, from: VI) -> usize
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.degree_undirected(from.as_id().as_ref())
    }

    pub fn degree_directed<VI>(&self, from: VI, dir: Direction) -> usize
    where
        G: Neighbors,
        VI: AsIdRef<G::VertexId>,
    {
        self.storage.degree_directed(from.as_id().as_ref(), dir)
    }

    pub fn ends(&self) -> Option<&[G::VertexId; 2]> {
        self.ends.as_ref()
    }

    pub fn stabilize(self) -> Path<V, E, G::EdgeType, Stable<G>> {
        Path::new_unchecked(Stable::new(self.storage), self.ends)
    }

    #[doc(hidden)]
    pub fn freeze(self) -> Path<V, E, G::EdgeType, Frozen<G>> {
        Path::new_unchecked(Frozen::new(self.storage), self.ends)
    }
}

impl<V, E, G> From<Path<V, E, G::EdgeType, G>> for Graph<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
    fn from(path: Path<V, E, G::EdgeType, G>) -> Self {
        Graph::new_in(path.storage)
    }
}

impl<V, E, G> Constrained<G> for Path<V, E, G::EdgeType, G>
where
    G: Neighbors + VertexSet + Guarantee,
{
    type Error = PathError<V, E>;

    fn check(storage: &G) -> Result<(), Self::Error> {
        // Statically guaranteed.
        if G::has_paths_only() && G::is_connected() {
            return Ok(());
        }

        Self::check_runtime(storage).map(|_| ())
    }

    fn constrain(storage: G) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        Self::check_runtime(&storage).map(|ends| Self::new_unchecked(storage, ends))
    }
}

impl<V, E, G> Deref for Path<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
    type Target = G;

    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl<V, E, G, VI> Index<VI> for Path<V, E, G::EdgeType, G>
where
    G: GraphRef<V, E>,
    VI: AsIdRef<G::VertexId>,
{
    type Output = V;

    fn index(&self, id: VI) -> &Self::Output {
        self.vertex(id).expect("vertex does not exist")
    }
}

impl<V, E, G, VI> IndexMut<VI> for Path<V, E, G::EdgeType, G>
where
    G: GraphMut<V, E>,
    VI: AsIdRef<G::VertexId>,
{
    fn index_mut(&mut self, id: VI) -> &mut Self::Output {
        self.vertex_mut(id).expect("vertex does not exist")
    }
}

impl<V, E, G> Guarantee for Path<V, E, G::EdgeType, G>
where
    G: GraphBase,
{
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
    use crate::core::id::{IdType, VertexId};

    use super::*;

    use assert_matches::assert_matches;

    fn v(id: usize) -> VertexId {
        id.into()
    }

    #[test]
    fn check_empty() {
        let graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();
        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_isolated() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        graph.add_vertex(());

        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_from_end() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_from_middle() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v0, &v2, ());

        assert!(Path::<(), (), Undirected, _>::check(&graph).is_ok());
    }

    #[test]
    fn check_directed() {
        let mut graph: AdjList<(), (), Directed, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v2, &v1, ());
        graph.add_edge(&v1, &v0, ());

        let path = Path::<(), (), Directed, _>::constrain(graph);
        assert!(path.is_ok());
        assert_eq!(path.unwrap().ends(), Some(&[v2, v0]));
    }

    #[test]
    fn check_from_end_higher_degree() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn check_from_middle_higher_degree() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v0, &v2, ());
        graph.add_edge(&v0, &v3, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn check_from_any_higher_degree() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v0, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn check_cycle() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v3, ());
        graph.add_edge(&v3, &v0, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::Cycle)
        );
    }

    #[test]
    fn check_multigraph() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v2, &v1, ());
        graph.add_edge(&v2, &v3, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn check_disconnected() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v2, &v3, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::Disconnected)
        );
    }

    #[test]
    fn check_self_loop() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v1, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn check_self_loop_isolated() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());

        graph.add_edge(&v0, &v0, ());

        assert_matches!(
            Path::<(), (), Undirected, _>::check(&graph),
            Err(PathError::Cycle)
        );
    }

    #[test]
    fn check_direction() {
        let mut graph: AdjList<(), (), Directed, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v2, &v1, ());
        graph.add_edge(&v2, &v3, ());

        assert_matches!(
            Path::<(), (), Directed, _>::check(&graph),
            Err(PathError::Direction)
        );
    }

    #[test]
    fn check_direction_ends() {
        let mut graph: AdjList<(), (), Directed, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v1, &v0, ());
        graph.add_edge(&v1, &v2, ());

        assert_matches!(
            Path::<(), (), Directed, _>::check(&graph),
            Err(PathError::Direction)
        );
    }

    #[test]
    fn try_add_vertex_middle() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

        assert_matches!(
            path.try_add_vertex((), None, v1),
            Err(PathError::HigherDegree)
        );
    }

    #[test]
    fn try_add_vertex_non_existent() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

        assert_matches!(
            path.try_add_vertex((), None, v(42)),
            Err(PathError::Disconnected)
        );
    }

    #[test]
    fn try_add_vertex_no_edge() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

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

        let result = path.try_add_vertex((), None, VertexId::sentinel());
        assert!(result.is_ok());

        let v = result.unwrap();
        assert_eq!(path.ends(), Some(&[v, v]));
    }

    #[test]
    fn try_add_vertex_isolated() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

        let result = path.try_add_vertex((), Some(()), v0);
        assert!(result.is_ok());

        let v = result.unwrap();
        assert_eq!(path.ends(), Some(&[v0, v]));

        assert!(path.edge_id_any(&v0, &v).is_some());
    }

    #[test]
    fn try_add_vertex() {
        let mut graph: AdjList<(), (), Undirected, DefaultId> = AdjList::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());

        let mut path = Path::<(), (), Undirected, _>::constrain(graph).unwrap();

        let [v0, v1] = *path.ends().unwrap();

        let result = path.try_add_vertex((), Some(()), v1);
        assert!(result.is_ok());

        let v = result.unwrap();
        assert_eq!(path.ends(), Some(&[v0, v]));

        assert!(path.edge_id_any(&v1, &v).is_some());
    }
}
