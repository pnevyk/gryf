use super::*;

pub struct Bfs<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexId, RawBfs>,
}

pub struct BfsRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawBfs>,
}

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
    pub fn new(graph: &G) -> Self
    where
        G: GraphBase,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: G::VertexId) -> BfsRooted<'_, G> {
        self.raw.start(root);
        BfsRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> BfsMulti<'a, G, VisitAll<'a, G>>
    where
        G: VertexSet,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    pub fn start_multi<S>(&mut self, roots: S) -> BfsMulti<'_, G, S>
    where
        S: VisitRoots<G::VertexId>,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(roots),
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

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
