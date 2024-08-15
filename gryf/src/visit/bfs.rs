//! Implementations of [breadth-first search] (BFS) algorithm.
//!
//! [breadth-first search]: https://en.wikipedia.org/wiki/Breadth-first_search

use super::*;

/// Standard BFS traversal over the vertices of a graph.
pub struct Bfs<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexId, RawBfs>,
}

/// [`Bfs`] rooted in a single vertex.
pub struct BfsRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawBfs>,
}

/// [`Bfs`] with possibly multiple roots.
pub struct BfsMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitRoots<G::VertexId>,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawBfs>,
    multi: RawVisitMulti<G, UseVertexId, RawBfs, S>,
}

impl<G> Bfs<G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../docs/include/visit.new.md")]
    pub fn new(graph: &G) -> Self
    where
        G: GraphBase,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    #[doc = include_str!("../../docs/include/visit.start.md")]
    pub fn start(&mut self, root: G::VertexId) -> BfsRooted<'_, G> {
        self.raw.start(root);
        BfsRooted { raw: &mut self.raw }
    }

    #[doc = include_str!("../../docs/include/visit.start_all.md")]
    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> BfsMulti<'a, G, VisitAll<'a, G>>
    where
        G: VertexSet,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    #[doc = include_str!("../../docs/include/visit.start_multi.md")]
    pub fn start_multi<S>(&mut self, roots: S) -> BfsMulti<'_, G, S>
    where
        S: VisitRoots<G::VertexId>,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(roots),
        }
    }

    #[doc = include_str!("../../docs/include/visit.reset.md")]
    pub fn reset(&mut self) {
        self.raw.reset();
    }

    #[doc = include_str!("../../docs/include/visit.visited.md")]
    pub fn visited(&self) -> &impl VisitSet<G::VertexId> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for BfsRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexId;

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, G> Visitor<G> for BfsMulti<'a, G, S>
where
    S: VisitRoots<G::VertexId>,
    G: Neighbors + VertexSet,
{
    type Item = G::VertexId;

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi.next_multi(
            self.raw,
            |raw| raw.next(graph, |_, _| true),
            |vertex| graph.contains_vertex(vertex),
        )
    }
}
