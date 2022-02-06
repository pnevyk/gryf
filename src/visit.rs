pub mod raw;

use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasherDefault,
    marker::PhantomData,
};

use rustc_hash::FxHashSet;

use crate::infra::VisitSet;
use crate::traits::*;
use crate::{
    index::{EdgeIndex, VertexIndex},
    marker::EdgeType,
};

use raw::*;

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

pub struct VisitAll<'a, V, G>
where
    G: Vertices<V>,
{
    graph: &'a G,
    indices: G::VertexIndicesIter<'a>,
    ty: PhantomData<&'a V>,
}

impl<'a, V, G> VisitAll<'a, V, G>
where
    G: Vertices<V>,
{
    pub fn new(graph: &'a G) -> Self {
        Self {
            graph,
            indices: graph.vertex_indices(),
            ty: PhantomData,
        }
    }
}

impl<V, G> VisitStarts for VisitAll<'_, V, G>
where
    G: Vertices<V>,
{
    fn get_next(&mut self) -> Option<VertexIndex> {
        self.indices.next()
    }

    fn is_done(&mut self, visited: &impl VisitSet<VertexIndex>) -> bool {
        // Since we are holding a shared reference to the graph, it could not
        // have been mutated during traversal. In particular, the number of
        // vertices didn't change.
        visited.visited_count() == self.graph.vertex_count()
    }
}

pub struct Bfs {
    raw: RawVisit<RawBfs>,
}

pub struct BfsRooted<'a> {
    raw: &'a mut RawVisit<RawBfs>,
}

pub struct BfsMulti<'a, V, S>
where
    S: VisitStarts,
{
    raw: &'a mut RawVisit<RawBfs>,
    multi: RawVisitMulti<RawBfs, S>,
    ty: PhantomData<&'a V>,
}

impl Bfs {
    pub fn new<V, G>(graph: &G) -> Self
    where
        G: VerticesWeak<V>,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: VertexIndex) -> BfsRooted<'_> {
        self.raw.start(root);
        BfsRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a, V, G>(&'a mut self, graph: &'a G) -> BfsMulti<'a, V, VisitAll<V, G>>
    where
        G: Vertices<V>,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
            ty: PhantomData,
        }
    }

    pub fn start_multi<V, S>(&mut self, starts: S) -> BfsMulti<'_, V, S>
    where
        S: VisitStarts,
    {
        BfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
            ty: PhantomData,
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for BfsRooted<'a>
where
    G: Neighbors,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, V, G> Visitor<G> for BfsMulti<'a, V, S>
where
    S: VisitStarts,
    G: Neighbors + Vertices<V>,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi.next_multi(
            self.raw,
            |raw| raw.next(graph, |_, _| true),
            |vertex| graph.contains_vertex(*vertex),
        )
    }
}

pub struct Dfs {
    raw: RawVisit<RawDfs>,
}

pub struct DfsRooted<'a> {
    raw: &'a mut RawVisit<RawDfs>,
}

pub struct DfsMulti<'a, V, S>
where
    S: VisitStarts,
{
    raw: &'a mut RawVisit<RawDfs>,
    multi: RawVisitMulti<RawDfs, S>,
    ty: PhantomData<&'a V>,
}

impl Dfs {
    pub fn new<V, G>(graph: &G) -> Self
    where
        G: VerticesWeak<V>,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: VertexIndex) -> DfsRooted<'_> {
        self.raw.start(root);
        DfsRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a, V, G>(&'a mut self, graph: &'a G) -> DfsMulti<'a, V, VisitAll<V, G>>
    where
        G: Vertices<V>,
    {
        DfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
            ty: PhantomData,
        }
    }

    pub fn start_multi<V, S>(&mut self, starts: S) -> DfsMulti<'_, V, S>
    where
        S: VisitStarts,
    {
        DfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
            ty: PhantomData,
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsRooted<'a>
where
    G: Neighbors,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, V, G> Visitor<G> for DfsMulti<'a, V, S>
where
    S: VisitStarts,
    G: Neighbors + Vertices<V>,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi.next_multi(
            self.raw,
            |raw| raw.next(graph, |_, _| true),
            |vertex| graph.contains_vertex(*vertex),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum DfsEvent {
    Open {
        vertex: VertexIndex,
        time: Time,
    },
    TreeEdge {
        src: VertexIndex,
        dst: VertexIndex,
        edge: EdgeIndex,
    },
    BackEdge {
        src: VertexIndex,
        dst: VertexIndex,
        edge: EdgeIndex,
    },
    CrossForwardEdge {
        src: VertexIndex,
        dst: VertexIndex,
        edge: EdgeIndex,
    },
    Close {
        vertex: VertexIndex,
        time: Time,
    },
}

pub struct DfsEvents {
    raw: RawVisit<RawDfsExtra>,
    closed: FxHashSet<VertexIndex>,
    is_directed: bool,
}

pub struct DfsEventsRooted<'a> {
    raw: &'a mut RawVisit<RawDfsExtra>,
    closed: &'a mut FxHashSet<VertexIndex>,
    queue: VecDeque<DfsEvent>,
    time: usize,
    is_directed: bool,
}

pub struct DfsEventsMulti<'a, V, S>
where
    S: VisitStarts,
{
    raw: &'a mut RawVisit<RawDfsExtra>,
    multi: RawVisitMulti<RawDfsExtra, S>,
    closed: &'a mut FxHashSet<VertexIndex>,
    queue: VecDeque<DfsEvent>,
    time: usize,
    is_directed: bool,
    ty: PhantomData<&'a V>,
}

impl DfsEvents {
    pub fn new<V, E, Ty: EdgeType, G>(graph: &G) -> Self
    where
        G: VerticesWeak<V> + EdgesWeak<E, Ty>,
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

    pub fn start(&mut self, root: VertexIndex) -> DfsEventsRooted<'_> {
        self.raw.start(RawDfsExtraItem::start(root));
        DfsEventsRooted {
            raw: &mut self.raw,
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
        }
    }

    pub fn start_all<'a, V, G>(&'a mut self, graph: &'a G) -> DfsEventsMulti<'a, V, VisitAll<V, G>>
    where
        G: Vertices<V>,
    {
        DfsEventsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
            ty: PhantomData,
        }
    }

    pub fn start_multi<V, S>(&mut self, starts: S) -> DfsEventsMulti<'_, V, S>
    where
        S: VisitStarts,
    {
        DfsEventsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
            ty: PhantomData,
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<VertexIndex> {
        &self.raw.visited
    }

    fn process_next_callback(
        raw: &RawVisit<RawDfsExtra>,
        raw_event: RawEvent,
        closed: &mut FxHashSet<VertexIndex>,
        queue: &mut VecDeque<DfsEvent>,
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

                        if parent != Some(vertex) {
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
        raw_extra_event: RawDfsExtraEvent,
        closed: &mut FxHashSet<VertexIndex>,
        queue: &mut VecDeque<DfsEvent>,
        time: &mut usize,
    ) {
        if let RawDfsExtraEvent::Close(vertex) = raw_extra_event {
            closed.insert(vertex);
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

impl<'a, G> Visitor<G> for DfsEventsRooted<'a>
where
    G: Neighbors,
{
    type Item = DfsEvent;

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

impl<'a, S, V, G> Visitor<G> for DfsEventsMulti<'a, V, S>
where
    S: VisitStarts,
    G: Neighbors + Vertices<V>,
{
    type Item = DfsEvent;

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
            |vertex| graph.contains_vertex(*vertex),
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

pub struct DfsPostOrder {
    raw: RawVisit<RawDfsExtra>,
}

pub struct DfsPostOrderRooted<'a> {
    raw: &'a mut RawVisit<RawDfsExtra>,
}

pub struct DfsPostOrderMulti<'a, V, S>
where
    S: VisitStarts,
{
    raw: &'a mut RawVisit<RawDfsExtra>,
    multi: RawVisitMulti<RawDfsExtra, S>,
    ty: PhantomData<&'a V>,
}

impl DfsPostOrder {
    pub fn new<V, G>(graph: &G) -> Self
    where
        G: VerticesWeak<V>,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: VertexIndex) -> DfsPostOrderRooted<'_> {
        self.raw.start(RawDfsExtraItem::start(root));
        DfsPostOrderRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a, V, G>(
        &'a mut self,
        graph: &'a G,
    ) -> DfsPostOrderMulti<'a, V, VisitAll<V, G>>
    where
        G: Vertices<V>,
    {
        DfsPostOrderMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
            ty: PhantomData,
        }
    }

    pub fn start_multi<V, S>(&mut self, starts: S) -> DfsPostOrderMulti<'_, V, S>
    where
        S: VisitStarts,
    {
        DfsPostOrderMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
            ty: PhantomData,
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsPostOrderRooted<'a>
where
    G: Neighbors,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        loop {
            if let RawDfsExtraEvent::Close(vertex) = self.raw.next(graph, |_, _| true)? {
                return Some(vertex);
            }
        }
    }
}

impl<'a, S, V, G> Visitor<G> for DfsPostOrderMulti<'a, V, S>
where
    S: VisitStarts,
    G: Neighbors + Vertices<V>,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi
            .next_multi(
                self.raw,
                |raw| loop {
                    if let RawDfsExtraEvent::Close(vertex) = raw.next(graph, |_, _| true)? {
                        return Some(RawDfsExtraItem::closed(vertex));
                    }
                },
                |vertex| graph.contains_vertex(*vertex),
            )
            .as_ref()
            .map(RawDfsExtra::index)
    }
}

pub struct DfsNoBacktrack {
    raw: RawVisit<RawDfsNoBacktrack>,
}

pub struct DfsNoBacktrackRooted<'a> {
    raw: &'a mut RawVisit<RawDfsNoBacktrack>,
}

pub struct DfsNoBacktrackMulti<'a, V, S>
where
    S: VisitStarts,
{
    raw: &'a mut RawVisit<RawDfsNoBacktrack>,
    multi: RawVisitMulti<RawDfsNoBacktrack, S>,
    ty: PhantomData<&'a V>,
}

impl DfsNoBacktrack {
    pub fn new<V, G>(graph: &G) -> Self
    where
        G: VerticesWeak<V>,
    {
        Self {
            raw: RawVisit::new(graph.vertex_count_hint()),
        }
    }

    pub fn start(&mut self, root: VertexIndex) -> DfsNoBacktrackRooted<'_> {
        self.raw.start(root);
        DfsNoBacktrackRooted { raw: &mut self.raw }
    }

    pub fn start_all<'a, V, G>(
        &'a mut self,
        graph: &'a G,
    ) -> DfsNoBacktrackMulti<'a, V, VisitAll<V, G>>
    where
        G: Vertices<V>,
    {
        DfsNoBacktrackMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
            ty: PhantomData,
        }
    }

    pub fn start_multi<V, S>(&mut self, starts: S) -> DfsNoBacktrackMulti<'_, V, S>
    where
        S: VisitStarts,
    {
        DfsNoBacktrackMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(starts),
            ty: PhantomData,
        }
    }

    pub fn reset(&mut self) {
        self.raw.reset();
    }

    pub fn visited(&self) -> &impl VisitSet<VertexIndex> {
        &self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsNoBacktrackRooted<'a>
where
    G: Neighbors,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, V, G> Visitor<G> for DfsNoBacktrackMulti<'a, V, S>
where
    S: VisitStarts,
    G: Neighbors + Vertices<V>,
{
    type Item = VertexIndex;

    fn next(&mut self, graph: &G) -> Option<Self::Item> {
        self.multi.next_multi(
            self.raw,
            |raw| raw.next(graph, |_, _| true),
            |vertex| graph.contains_vertex(*vertex),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        marker::{Directed, Undirected},
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
        let mut graph = AdjList::<_, _, Undirected>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        let vertices = Bfs::new(&graph).start(v0).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v1, v2, v3, v4, v5]);
    }

    #[test]
    fn dfs_connected() {
        let mut graph = AdjList::<_, _, Undirected>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        let vertices = Dfs::new(&graph).start(v0).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v1, v4, v5, v2, v3]);
    }

    #[test]
    fn bfs_disconnected() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        graph.remove_vertex(v1);

        let vertices = Bfs::new(&graph).start(v2).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn dfs_disconnected() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        graph.remove_vertex(v1);

        let vertices = Dfs::new(&graph).start(v2).iter(&graph).collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4]);
    }

    #[test]
    fn bfs_disconnected_all() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        graph.remove_vertex(v1);

        let vertices = Bfs::new(&graph)
            .start_all(&graph)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4, v3]);
    }

    #[test]
    fn dfs_disconnected_all() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        graph.remove_vertex(v1);

        let vertices = Dfs::new(&graph)
            .start_all(&graph)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4, v3]);
    }

    #[test]
    fn bfs_disconnected_multi() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        graph.remove_vertex(v1);

        let vertices = Bfs::new(&graph)
            .start_multi([v0, v2].into_iter())
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v0, v2, v5, v4]);
    }

    #[test]
    fn bfs_disconnected_multi_mutation() {
        let mut graph = Stable::new(AdjList::<_, _, Undirected>::new());

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        graph.remove_vertex(v1);

        let mut visit = Bfs::new(&graph);
        let mut visit = visit.start_multi([v0, v2].into_iter());

        // Remove one of the starts.
        graph.remove_vertex(v0);

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

        let mut graph = AdjList::<_, _, Undirected>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());
        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());

        let e0 = graph.add_edge(v0, v1, ());
        let e1 = graph.add_edge(v1, v2, ());
        let e2 = graph.add_edge(v2, v3, ());
        let e3 = graph.add_edge(v3, v4, ());
        let e4 = graph.add_edge(v4, v2, ());
        let e5 = graph.add_edge(v1, v5, ());
        let e6 = graph.add_edge(v5, v6, ());
        let e7 = graph.add_edge(v6, v7, ());
        let e8 = graph.add_edge(v5, v7, ());
        let e9 = graph.add_edge(v3, v6, ());

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

        let mut graph = AdjList::<_, _, Directed>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());
        let v6 = graph.add_vertex(());
        let v7 = graph.add_vertex(());

        let e0 = graph.add_edge(v0, v1, ());
        let e1 = graph.add_edge(v1, v2, ());
        let e2 = graph.add_edge(v2, v3, ());
        let e3 = graph.add_edge(v3, v4, ());
        let e4 = graph.add_edge(v4, v2, ());
        let e5 = graph.add_edge(v1, v5, ());
        let e6 = graph.add_edge(v5, v6, ());
        let e7 = graph.add_edge(v6, v7, ());
        let e8 = graph.add_edge(v5, v7, ());
        let e9 = graph.add_edge(v3, v6, ());

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
        let mut graph = AdjList::<_, (), Undirected>::new();

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
        let mut graph = AdjList::<_, _, Undirected>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        let vertices = DfsPostOrder::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        assert_eq!(vertices, vec![v2, v5, v4, v3, v1, v0]);
    }

    #[test]
    fn dfs_no_backtrack() {
        let mut graph = AdjList::<_, _, Undirected>::new();

        let v0 = graph.add_vertex(());
        let v1 = graph.add_vertex(());
        let v2 = graph.add_vertex(());
        let v3 = graph.add_vertex(());
        let v4 = graph.add_vertex(());
        let v5 = graph.add_vertex(());

        graph.add_edge(v0, v1, ());
        graph.add_edge(v1, v2, ());
        graph.add_edge(v1, v3, ());
        graph.add_edge(v1, v4, ());
        graph.add_edge(v2, v5, ());
        graph.add_edge(v5, v4, ());

        let vertices = DfsNoBacktrack::new(&graph)
            .start(v0)
            .iter(&graph)
            .collect::<Vec<_>>();

        // v3 is missing because it would be visited only if the algorithm would
        // continue from v1 after traversing the "long" branch.
        assert_eq!(vertices, vec![v0, v1, v2, v5, v4]);
    }
}
