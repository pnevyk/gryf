pub(crate) mod raw;

use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasherDefault,
};

use rustc_hash::FxHashSet;

use raw::*;

use crate::{
    common::VisitSet,
    core::{
        index::UseVertexIndex, marker::EdgeType, EdgesBaseWeak, GraphBase, Neighbors, VerticesBase,
        VerticesBaseWeak,
    },
};

pub trait Visitor<G> {
    type Item;

    fn next(&mut self, graph: &G) -> Option<Self::Item>;

    fn iter<'a>(&'a mut self, graph: &'a G) -> Iter<'a, Self, G>
    where
        Self: Sized,
    {
        Iter {
            visitor: self,
            graph,
        }
    }

    fn into_iter(self, graph: &G) -> IntoIter<'_, Self, G>
    where
        Self: Sized,
    {
        IntoIter {
            visitor: self,
            graph,
        }
    }
}

pub struct Iter<'a, V, G> {
    visitor: &'a mut V,
    graph: &'a G,
}

impl<'a, V, G> Iterator for Iter<'a, V, G>
where
    V: Visitor<G>,
{
    type Item = V::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor.next(self.graph)
    }
}

pub struct IntoIter<'a, V, G> {
    visitor: V,
    graph: &'a G,
}

impl<'a, V, G> Iterator for IntoIter<'a, V, G>
where
    V: Visitor<G>,
{
    type Item = V::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor.next(self.graph)
    }
}

pub struct VisitAll<'a, G>
where
    G: VerticesBase,
{
    graph: &'a G,
    indices: G::VertexIndicesIter<'a>,
}

impl<'a, G> VisitAll<'a, G>
where
    G: VerticesBase,
{
    pub fn new(graph: &'a G) -> Self {
        Self {
            graph,
            indices: graph.vertex_indices(),
        }
    }
}

impl<G> VisitStarts<G::VertexIndex> for VisitAll<'_, G>
where
    G: VerticesBase,
{
    fn get_next(&mut self) -> Option<G::VertexIndex> {
        self.indices.next()
    }

    fn is_done(&mut self, visited: &impl VisitSet<G::VertexIndex>) -> bool {
        // Since we are holding a shared reference to the graph, it could not
        // have been mutated during traversal. In particular, the number of
        // vertices didn't change.
        visited.visited_count() == self.graph.vertex_count()
    }
}

pub struct Bfs<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexIndex, RawBfs>,
}

pub struct BfsRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawBfs>,
}

pub struct BfsMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitStarts<G::VertexIndex>,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawBfs>,
    multi: RawVisitMulti<G, UseVertexIndex, RawBfs, S>,
}

impl<G> Bfs<G>
where
    G: GraphBase,
{
    pub fn new(graph: &G) -> Self
    where
        G: VerticesBaseWeak,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: G::VertexIndex) -> BfsRooted<'_, G> {
        self.raw.start(root);
        BfsRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> BfsMulti<'a, G, VisitAll<G>>
    where
        G: VerticesBase,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    pub fn start_multi<S>(&mut self, starts: S) -> BfsMulti<'_, G, S>
    where
        S: VisitStarts<G::VertexIndex>,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<G::VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for BfsRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, G> Visitor<G> for BfsMulti<'a, G, S>
where
    S: VisitStarts<G::VertexIndex>,
    G: Neighbors + VerticesBase,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi.next_multi(
            self.raw,
            |raw| raw.next(graph, |_, _| true),
            |vertex| graph.contains_vertex(vertex),
        )
    }
}

pub struct Dfs<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexIndex, RawDfs>,
}

pub struct DfsRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfs>,
}

pub struct DfsMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitStarts<G::VertexIndex>,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfs>,
    multi: RawVisitMulti<G, UseVertexIndex, RawDfs, S>,
}

impl<G> Dfs<G>
where
    G: GraphBase,
{
    pub fn new(graph: &G) -> Self
    where
        G: VerticesBaseWeak,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: G::VertexIndex) -> DfsRooted<'_, G> {
        self.raw.start(root);
        DfsRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsMulti<'a, G, VisitAll<G>>
    where
        G: VerticesBase,
    {
        DfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    pub fn start_multi<S>(&mut self, starts: S) -> DfsMulti<'_, G, S>
    where
        S: VisitStarts<G::VertexIndex>,
    {
        DfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<G::VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, G> Visitor<G> for DfsMulti<'a, G, S>
where
    S: VisitStarts<G::VertexIndex>,
    G: Neighbors + VerticesBase,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi.next_multi(
            self.raw,
            |raw| raw.next(graph, |_, _| true),
            |vertex| graph.contains_vertex(vertex),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DfsEvent<G>
where
    G: GraphBase,
{
    Open {
        vertex: G::VertexIndex,
        time: Time,
    },
    TreeEdge {
        src: G::VertexIndex,
        dst: G::VertexIndex,
        edge: G::EdgeIndex,
    },
    BackEdge {
        src: G::VertexIndex,
        dst: G::VertexIndex,
        edge: G::EdgeIndex,
    },
    CrossForwardEdge {
        src: G::VertexIndex,
        dst: G::VertexIndex,
        edge: G::EdgeIndex,
    },
    Close {
        vertex: G::VertexIndex,
        time: Time,
    },
}

pub struct DfsEvents<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexIndex, RawDfsExtra>,
    closed: FxHashSet<G::VertexIndex>,
    is_directed: bool,
}

pub struct DfsEventsRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfsExtra>,
    closed: &'a mut FxHashSet<G::VertexIndex>,
    queue: VecDeque<DfsEvent<G>>,
    time: usize,
    is_directed: bool,
}

pub struct DfsEventsMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitStarts<G::VertexIndex>,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfsExtra>,
    multi: RawVisitMulti<G, UseVertexIndex, RawDfsExtra, S>,
    closed: &'a mut FxHashSet<G::VertexIndex>,
    queue: VecDeque<DfsEvent<G>>,
    time: usize,
    is_directed: bool,
}

impl<G> DfsEvents<G>
where
    G: GraphBase,
{
    pub fn new<Ty: EdgeType>(graph: &G) -> Self
    where
        G: VerticesBaseWeak + EdgesBaseWeak<Ty>,
    {
        let is_directed = graph.is_directed_weak();

        let raw = RawVisit::new(graph.vertex_count_hint());
        let closed = if is_directed {
            HashSet::with_capacity_and_hasher(raw.visited.capacity(), BuildHasherDefault::default())
        } else {
            // Discovered set (in raw) is used instead of closed set in
            // undirected graph because it is the only information needed for
            // back edge determination. See back edge signalling below for more
            // details.
            HashSet::default()
        };

        Self {
            raw,
            closed,
            is_directed,
        }
    }

    pub fn start(&mut self, root: G::VertexIndex) -> DfsEventsRooted<'_, G> {
        self.raw.start(RawDfsExtraItem::start(root));
        DfsEventsRooted {
            raw: &mut self.raw,
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
        }
    }

    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsEventsMulti<'a, G, VisitAll<G>>
    where
        G: VerticesBase,
    {
        DfsEventsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
        }
    }

    pub fn start_multi<S>(&mut self, starts: S) -> DfsEventsMulti<'_, G, S>
    where
        S: VisitStarts<G::VertexIndex>,
    {
        DfsEventsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<G::VertexIndex> {
        &self.raw.visited
    }

    fn process_next_callback(
        raw: &RawVisit<G, UseVertexIndex, RawDfsExtra>,
        raw_event: RawEvent<G>,
        closed: &mut FxHashSet<G::VertexIndex>,
        queue: &mut VecDeque<DfsEvent<G>>,
        is_directed: bool,
    ) -> bool {
        match raw_event {
            RawEvent::Popped { .. } => {}
            RawEvent::Push { vertex, src } => {
                queue.push_back(DfsEvent::TreeEdge {
                    src: src.0,
                    dst: vertex,
                    edge: src.1,
                });
            }
            RawEvent::Skip { vertex, src } => {
                let src = src.expect("src always available");

                if is_directed {
                    if !closed.contains(&vertex) {
                        queue.push_back(DfsEvent::BackEdge {
                            src: src.0,
                            dst: vertex,
                            edge: src.1,
                        });
                    } else {
                        queue.push_back(DfsEvent::CrossForwardEdge {
                            src: src.0,
                            dst: vertex,
                            edge: src.1,
                        });
                    }
                } else {
                    // `RawDfsExtra` traverses all neighbors of the vertex.
                    // However, this means that the edge back to the parent
                    // is also reported (as skipped, since the parent is
                    // already opened). We need to detect that and avoid
                    // signalling this edge as back edge. Due to the
                    // implementation details of `RawDfsExtra`, in the time
                    // of signalling `Skip` event, the item on top of the
                    // stack is the parent of the skipped vertex.
                    if !closed.contains(&vertex) {
                        let parent = raw.collection.0.last().map(RawDfsExtra::index);

                        if parent.as_ref() != Some(&vertex) {
                            queue.push_back(DfsEvent::BackEdge {
                                src: src.0,
                                dst: vertex,
                                edge: src.1,
                            });
                        }
                    }

                    // Cross-forward edges do not make sense in undirected
                    // graphs.
                }
            }
        }

        true
    }

    fn process_next_event(
        raw_extra_event: RawDfsExtraEvent<G>,
        closed: &mut FxHashSet<G::VertexIndex>,
        queue: &mut VecDeque<DfsEvent<G>>,
        time: &mut usize,
    ) {
        if let RawDfsExtraEvent::Close(ref vertex) = raw_extra_event {
            closed.insert(vertex.clone());
        }

        let event = match raw_extra_event {
            RawDfsExtraEvent::Open(vertex) => DfsEvent::Open {
                vertex,
                time: Time(*time),
            },
            RawDfsExtraEvent::Close(vertex) => DfsEvent::Close {
                vertex,
                time: Time(*time),
            },
        };

        *time += 1;
        queue.push_back(event);
    }
}

impl<'a, G> Visitor<G> for DfsEventsRooted<'a, G>
where
    G: Neighbors,
{
    type Item = DfsEvent<G>;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        if let Some(event) = self.queue.pop_front() {
            return Some(event);
        }

        if let Some(raw_extra_event) = self.raw.next(graph, |raw, raw_event| {
            DfsEvents::process_next_callback(
                raw,
                raw_event,
                self.closed,
                &mut self.queue,
                self.is_directed,
            )
        }) {
            DfsEvents::process_next_event(
                raw_extra_event,
                self.closed,
                &mut self.queue,
                &mut self.time,
            );
        };

        self.queue.pop_front()
    }
}

impl<'a, S, G> Visitor<G> for DfsEventsMulti<'a, G, S>
where
    G: Neighbors + VerticesBase,
    S: VisitStarts<G::VertexIndex>,
{
    type Item = DfsEvent<G>;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        if let Some(event) = self.queue.pop_front() {
            return Some(event);
        }

        if let Some(raw_extra_event) = self.multi.next_multi(
            self.raw,
            |raw| {
                raw.next(graph, |raw, raw_event| {
                    DfsEvents::process_next_callback(
                        raw,
                        raw_event,
                        self.closed,
                        &mut self.queue,
                        self.is_directed,
                    )
                })
            },
            |vertex| graph.contains_vertex(vertex),
        ) {
            DfsEvents::process_next_event(
                raw_extra_event,
                self.closed,
                &mut self.queue,
                &mut self.time,
            );
        }

        self.queue.pop_front()
    }
}

pub struct DfsPostOrder<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexIndex, RawDfsExtra>,
}

pub struct DfsPostOrderRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfsExtra>,
}

pub struct DfsPostOrderMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitStarts<G::VertexIndex>,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfsExtra>,
    multi: RawVisitMulti<G, UseVertexIndex, RawDfsExtra, S>,
}

impl<G> DfsPostOrder<G>
where
    G: GraphBase,
{
    pub fn new(graph: &G) -> Self
    where
        G: VerticesBaseWeak,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: G::VertexIndex) -> DfsPostOrderRooted<'_, G> {
        self.raw.start(RawDfsExtraItem::start(root));
        DfsPostOrderRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsPostOrderMulti<'a, G, VisitAll<G>>
    where
        G: VerticesBase,
    {
        DfsPostOrderMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    pub fn start_multi<S>(&mut self, starts: S) -> DfsPostOrderMulti<'_, G, S>
    where
        S: VisitStarts<G::VertexIndex>,
    {
        DfsPostOrderMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<G::VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsPostOrderRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        loop {
            if let RawDfsExtraEvent::Close(vertex) = self.raw.next(graph, |_, _| true)? {
                return Some(vertex);
            }
        }
    }
}

impl<'a, S, G> Visitor<G> for DfsPostOrderMulti<'a, G, S>
where
    G: Neighbors + VerticesBase,
    S: VisitStarts<G::VertexIndex>,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi
            .next_multi(
                self.raw,
                |raw| loop {
                    if let RawDfsExtraEvent::Close(vertex) = raw.next(graph, |_, _| true)? {
                        return Some(RawDfsExtraItem::<G>::closed(vertex));
                    }
                },
                |vertex| graph.contains_vertex(vertex),
            )
            .as_ref()
            .map(RawDfsExtra::index)
    }
}

pub struct DfsNoBacktrack<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexIndex, RawDfsNoBacktrack>,
}

pub struct DfsNoBacktrackRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfsNoBacktrack>,
}

pub struct DfsNoBacktrackMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitStarts<G::VertexIndex>,
{
    raw: &'a mut RawVisit<G, UseVertexIndex, RawDfsNoBacktrack>,
    multi: RawVisitMulti<G, UseVertexIndex, RawDfsNoBacktrack, S>,
}

impl<G> DfsNoBacktrack<G>
where
    G: GraphBase,
{
    pub fn new(graph: &G) -> Self
    where
        G: VerticesBaseWeak,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: G::VertexIndex) -> DfsNoBacktrackRooted<'_, G> {
        self.raw.start(root);
        DfsNoBacktrackRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsNoBacktrackMulti<'a, G, VisitAll<G>>
    where
        G: VerticesBase,
    {
        DfsNoBacktrackMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    pub fn start_multi<S>(&mut self, starts: S) -> DfsNoBacktrackMulti<'_, G, S>
    where
        S: VisitStarts<G::VertexIndex>,
    {
        DfsNoBacktrackMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<G::VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsNoBacktrackRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, G> Visitor<G> for DfsNoBacktrackMulti<'a, G, S>
where
    G: Neighbors + VerticesBase,
    S: VisitStarts<G::VertexIndex>,
{
    type Item = G::VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi.next_multi(
            self.raw,
            |raw| raw.next(graph, |_, _| true),
            |vertex| graph.contains_vertex(vertex),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            index::DefaultIndexing,
            marker::{Directed, Undirected},
            EdgesMut, VerticesMut,
        },
        storage::{AdjList, Stable},
    };

    use super::*;

    macro_rules! dfs_event {
        (open, $v:expr, $t:expr) => {
            DfsEvent::Open {
                vertex: $v,
                time: Time($t),
            }
        };
        (tree, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::TreeEdge {
                src: $u,
                dst: $v,
                edge: $e,
            }
        };
        (back, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::BackEdge {
                src: $u,
                dst: $v,
                edge: $e,
            }
        };
        (cross_forward, $e:expr, ($u:expr, $v:expr)) => {
            DfsEvent::CrossForwardEdge {
                src: $u,
                dst: $v,
                edge: $e,
            }
        };
        (close, $v:expr, $t:expr) => {
            DfsEvent::Close {
                vertex: $v,
                time: Time($t),
            }
        };
    }

    #[test]
    fn bfs_connected() {
        let mut graph = AdjList::<_, _, Undirected, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = Bfs::new(&graph).start(v0).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v1, v2, v3, v4, v5]);
    }

    #[test]
    fn dfs_connected() {
        let mut graph = AdjList::<_, _, Undirected, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = Dfs::new(&graph).start(v0).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v1, v4, v5, v2, v3]);
    }

    #[test]
    fn bfs_disconnected() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultIndexing>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Bfs::new(&graph).start(v2).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn dfs_disconnected() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultIndexing>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Dfs::new(&graph).start(v2).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn bfs_disconnected_all() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultIndexing>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Bfs::new(&graph)
            .start_all(&graph)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4, v3]);
    }

    #[test]
    fn dfs_disconnected_all() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultIndexing>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Dfs::new(&graph)
            .start_all(&graph)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4, v3]);
    }

    #[test]
    fn bfs_disconnected_multi() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultIndexing>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let vertices = Bfs::new(&graph)
            .start_multi([v0, v2].into_iter())
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4]);
    }

    #[test]
    fn bfs_disconnected_multi_mutation() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected, DefaultIndexing>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        graph.remove_vertex(&v1);

        let mut visit = Bfs::new(&graph);
        let mut visit = visit.start_multi([v0, v2].into_iter());

        // Remove one of the starts.
        graph.remove_vertex(&v0);

        let vertices = visit.iter(&graph).collect::<Vec<_>>();

        // Removing one of the start causes incomplete visit, but not a crash.
        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn dfs_events() {
        // The graph with edge types (assuming implementation details of
        // `AdjList`).
        //
        //                          +----------------- T -----+
        //                          |                         |
        //               +-- B --- (2) -- B --- (3) -- T --- (4)
        //               |                       |
        // (0) -- T --- (1)                      T
        //               |                       |
        //               +-- T --- (5) -- B --- (6) -- T --- (7)
        //                          |                         |
        //                          +---- T ------------------+

        let mut graph = AdjList::<_, _, Undirected, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());
        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v3, ());
        let e3 = graph.add_edge(&v3, &v4, ());
        let e4 = graph.add_edge(&v4, &v2, ());
        let e5 = graph.add_edge(&v1, &v5, ());
        let e6 = graph.add_edge(&v5, &v6, ());
        let e7 = graph.add_edge(&v6, &v7, ());
        let e8 = graph.add_edge(&v5, &v7, ());
        let e9 = graph.add_edge(&v3, &v6, ());

        let events = DfsEvents::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        let expected = vec![
            dfs_event!(open, v0, 0),
            dfs_event!(tree, e0, (v0, v1)),
            dfs_event!(open, v1, 1),
            dfs_event!(tree, e5, (v1, v5)),
            dfs_event!(open, v5, 2),
            dfs_event!(tree, e8, (v5, v7)),
            dfs_event!(open, v7, 3),
            dfs_event!(tree, e7, (v7, v6)),
            dfs_event!(open, v6, 4),
            dfs_event!(tree, e9, (v6, v3)),
            dfs_event!(open, v3, 5),
            dfs_event!(tree, e3, (v3, v4)),
            dfs_event!(open, v4, 6),
            dfs_event!(tree, e4, (v4, v2)),
            dfs_event!(open, v2, 7),
            dfs_event!(back, e2, (v2, v3)),
            dfs_event!(back, e1, (v2, v1)),
            dfs_event!(close, v2, 8),
            dfs_event!(close, v4, 9),
            dfs_event!(close, v3, 10),
            dfs_event!(back, e6, (v6, v5)),
            dfs_event!(close, v6, 11),
            dfs_event!(close, v7, 12),
            dfs_event!(close, v5, 13),
            dfs_event!(close, v1, 14),
            dfs_event!(close, v0, 15),
        ];

        assert_eq!(events, expected);
    }

    #[test]
    fn dfs_events_directed() {
        // The graph with edge types (assuming implementation details of
        // `AdjList`).
        //
        //                          +----------------- B -----+
        //                          v                         |
        //               +-- T --> (2) -- T --> (3) -- T --> (4)
        //               |                       |
        // (0) -- T --> (1)                      C
        //               |                       v
        //               +-- T --> (5) -- T --> (6) -- F --> (7)
        //                          |                         ʌ
        //                          +---- T ------------------+

        let mut graph = AdjList::<_, _, Directed, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());
        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());

        let e0 = graph.add_edge(&v0, &v1, ());
        let e1 = graph.add_edge(&v1, &v2, ());
        let e2 = graph.add_edge(&v2, &v3, ());
        let e3 = graph.add_edge(&v3, &v4, ());
        let e4 = graph.add_edge(&v4, &v2, ());
        let e5 = graph.add_edge(&v1, &v5, ());
        let e6 = graph.add_edge(&v5, &v6, ());
        let e7 = graph.add_edge(&v6, &v7, ());
        let e8 = graph.add_edge(&v5, &v7, ());
        let e9 = graph.add_edge(&v3, &v6, ());

        let events = DfsEvents::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        let expected = vec![
            dfs_event!(open, v0, 0),
            dfs_event!(tree, e0, (v0, v1)),
            dfs_event!(open, v1, 1),
            dfs_event!(tree, e5, (v1, v5)),
            dfs_event!(open, v5, 2),
            dfs_event!(tree, e8, (v5, v7)),
            dfs_event!(open, v7, 3),
            dfs_event!(close, v7, 4),
            dfs_event!(tree, e6, (v5, v6)),
            dfs_event!(open, v6, 5),
            dfs_event!(cross_forward, e7, (v6, v7)),
            dfs_event!(close, v6, 6),
            dfs_event!(close, v5, 7),
            dfs_event!(tree, e1, (v1, v2)),
            dfs_event!(open, v2, 8),
            dfs_event!(tree, e2, (v2, v3)),
            dfs_event!(open, v3, 9),
            dfs_event!(cross_forward, e9, (v3, v6)),
            dfs_event!(tree, e3, (v3, v4)),
            dfs_event!(open, v4, 10),
            dfs_event!(back, e4, (v4, v2)),
            dfs_event!(close, v4, 11),
            dfs_event!(close, v3, 12),
            dfs_event!(close, v2, 13),
            dfs_event!(close, v1, 14),
            dfs_event!(close, v0, 15),
        ];

        assert_eq!(events, expected);
    }

    #[test]
    fn dfs_events_isolated() {
        let mut graph = AdjList::<_, (), Undirected, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());

        let events = DfsEvents::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        let expected = vec![dfs_event!(open, v0, 0), dfs_event!(close, v0, 1)];

        assert_eq!(events, expected);
    }

    #[test]
    fn dfs_post_order() {
        let mut graph = AdjList::<_, _, Undirected, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = DfsPostOrder::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4, v3, v1, v0]);
    }

    #[test]
    fn dfs_no_backtrack() {
        let mut graph = AdjList::<_, _, Undirected, DefaultIndexing>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(&v0, &v1, ());
        graph.add_edge(&v1, &v2, ());
        graph.add_edge(&v1, &v3, ());
        graph.add_edge(&v1, &v4, ());
        graph.add_edge(&v2, &v5, ());
        graph.add_edge(&v5, &v4, ());

        let vertices = DfsNoBacktrack::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        // v3 is missing because it would be visited only if the algorithm would
        // continue from v1 after traversing the "long" branch.
        assert_eq!(vertices, vec![v0, v1, v2, v5, v4]);
    }
}
