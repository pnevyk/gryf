//! Implementations of [depth-first search] (DFS) algorithm.
//!
//! [depth-first search]: https://en.wikipedia.org/wiki/Depth-first_search

use super::*;

/// Standard DFS traversal over the vertices of a graph.
pub struct Dfs<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexId, RawDfs>,
}

/// [`Dfs`] rooted in a single vertex.
pub struct DfsRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfs>,
}

/// [`Dfs`] with possibly multiple roots.
pub struct DfsMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitRoots<G::VertexId>,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfs>,
    multi: RawVisitMulti<G, UseVertexId, RawDfs, S>,
}

impl<G> Dfs<G>
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
    pub fn start(&mut self, root: G::VertexId) -> DfsRooted<'_, G> {
        self.raw.start(root);
        DfsRooted { raw: &mut self.raw }
    }

    #[doc = include_str!("../../docs/include/visit.start_all.md")]
    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsMulti<'a, G, VisitAll<'a, G>>
    where
        G: VertexSet,
    {
        DfsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    #[doc = include_str!("../../docs/include/visit.start_multi.md")]
    pub fn start_multi<S>(&mut self, roots: S) -> DfsMulti<'_, G, S>
    where
        S: VisitRoots<G::VertexId>,
    {
        DfsMulti {
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

    #[doc = include_str!("../../docs/include/visit.visited_mut.md")]
    pub fn visited_mut(&mut self) -> &mut impl VisitSet<G::VertexId> {
        &mut self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexId;

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, G> Visitor<G> for DfsMulti<'a, G, S>
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

/// Standard DFS traversal that produces [`DfsEvent`]s.
///
/// # Examples
///
/// ```
/// use std::collections::BTreeSet;
///
/// use gryf::{
///     adapt::Subgraph,
///     algo::{is_connected, is_cyclic},
///     visit::{DfsEvent, DfsEvents, Visitor},
///     Graph,
/// };
///
/// let mut graph = Graph::<_, (), _>::new_directed();
///
/// graph.extend_with_vertices(["a", "b", "c", "d", "e", "f", "g", "h"]);
/// graph.extend_with_edges([
///     (0, 1),
///     (0, 2),
///     (0, 7),
///     (1, 3),
///     (2, 4),
///     (3, 5),
///     (4, 3),
///     (4, 6),
///     (4, 7),
///     (5, 1),
/// ]);
///
/// // There is a cycle.
/// assert!(is_cyclic(&graph));
///
/// let root = graph.find_vertex("a").unwrap();
///
/// let spanning_tree_edges = DfsEvents::new(&graph)
///     .start(root)
///     .into_iter(&graph)
///     .filter_map(|event| {
///         if let DfsEvent::TreeEdge { edge, .. } = event {
///             Some(edge)
///         } else {
///             None
///         }
///     })
///     .collect::<BTreeSet<_>>();
///
/// let spanning_tree = Subgraph::with_state(graph, spanning_tree_edges)
///     .filter_edge(|edge, _, state| state.contains(edge));
///
/// // The subgraph is a spanning tree.
/// assert!(!is_cyclic(&spanning_tree));
/// assert!(is_connected(&spanning_tree));
/// ```
pub struct DfsEvents<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexId, RawDfsExtra>,
    closed: FxHashSet<G::VertexId>,
    is_directed: bool,
}

/// [`DfsEvents`] rooted in a single vertex.
pub struct DfsEventsRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfsExtra>,
    closed: &'a mut FxHashSet<G::VertexId>,
    queue: VecDeque<DfsEvent<G>>,
    time: usize,
    is_directed: bool,
}

/// [`DfsEvents`] with possibly multiple roots.
pub struct DfsEventsMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitRoots<G::VertexId>,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfsExtra>,
    multi: RawVisitMulti<G, UseVertexId, RawDfsExtra, S>,
    closed: &'a mut FxHashSet<G::VertexId>,
    queue: VecDeque<DfsEvent<G>>,
    time: usize,
    is_directed: bool,
}

impl<G> DfsEvents<G>
where
    G: GraphBase,
{
    #[doc = include_str!("../../docs/include/visit.new.md")]
    pub fn new(graph: &G) -> Self
    where
        G: GraphBase,
    {
        let is_directed = graph.is_directed();

        let raw = RawVisit::new(graph.vertex_count_hint());
        let closed = if is_directed {
            FxHashSet::with_capacity_and_hasher(raw.visited.capacity(), rustc_hash::FxBuildHasher)
        } else {
            // Discovered set (in raw) is used instead of closed set in
            // undirected graph because it is the only information needed for
            // back edge determination. See back edge signalling below for more
            // details.
            FxHashSet::default()
        };

        Self {
            raw,
            closed,
            is_directed,
        }
    }

    #[doc = include_str!("../../docs/include/visit.start.md")]
    pub fn start(&mut self, root: G::VertexId) -> DfsEventsRooted<'_, G> {
        self.raw.start(RawDfsExtraItem::start(root));
        DfsEventsRooted {
            raw: &mut self.raw,
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
        }
    }

    #[doc = include_str!("../../docs/include/visit.start_all.md")]
    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsEventsMulti<'a, G, VisitAll<'a, G>>
    where
        G: VertexSet,
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

    #[doc = include_str!("../../docs/include/visit.start_multi.md")]
    pub fn start_multi<S>(&mut self, roots: S) -> DfsEventsMulti<'_, G, S>
    where
        S: VisitRoots<G::VertexId>,
    {
        DfsEventsMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(roots),
            closed: &mut self.closed,
            queue: VecDeque::new(),
            time: 0,
            is_directed: self.is_directed,
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

    #[doc = include_str!("../../docs/include/visit.visited_mut.md")]
    pub fn visited_mut(&mut self) -> &mut impl VisitSet<G::VertexId> {
        &mut self.raw.visited
    }

    fn process_next_callback(
        raw: &RawVisit<G, UseVertexId, RawDfsExtra>,
        raw_event: RawEvent<G>,
        closed: &mut FxHashSet<G::VertexId>,
        queue: &mut VecDeque<DfsEvent<G>>,
        is_directed: bool,
    ) -> bool {
        match raw_event {
            RawEvent::Popped { .. } => {}
            RawEvent::Push { vertex, from } => {
                queue.push_back(DfsEvent::TreeEdge {
                    from: from.0,
                    to: vertex,
                    edge: from.1,
                });
            }
            RawEvent::Skip { vertex, from } => {
                let from = from.expect("edge tail vertex always available");

                if is_directed {
                    if !closed.contains(&vertex) {
                        queue.push_back(DfsEvent::BackEdge {
                            from: from.0,
                            to: vertex,
                            edge: from.1,
                        });
                    } else {
                        queue.push_back(DfsEvent::CrossForwardEdge {
                            from: from.0,
                            to: vertex,
                            edge: from.1,
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
                        let parent = raw.collection.0.last().map(RawDfsExtra::id);

                        if parent.as_ref() != Some(&vertex) {
                            queue.push_back(DfsEvent::BackEdge {
                                from: from.0,
                                to: vertex,
                                edge: from.1,
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
        closed: &mut FxHashSet<G::VertexId>,
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

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
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
    G: Neighbors + VertexSet,
    S: VisitRoots<G::VertexId>,
{
    type Item = DfsEvent<G>;

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
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

/// [Post-order DFS] traversal over vertices of a graph.
///
/// [Post-order DFS]: https://en.wikipedia.org/wiki/Tree_traversal#Post-order,_LRN
///
/// # Examples
///
/// ```
/// use gryf::{
///     visit::{DfsPostOrder, Visitor},
///     Graph,
/// };
///
/// let mut graph = Graph::<_, (), _>::new_directed();
///
/// graph.extend_with_vertices(["a", "b", "c"]);
/// graph.extend_with_edges([(0, 1), (0, 2)]);
///
/// let root = graph.find_vertex("a").unwrap();
///
/// let post_order_last = DfsPostOrder::new(&graph)
///     .start(root)
///     .into_iter(&graph)
///     .last()
///     .unwrap();
///
/// assert_eq!(post_order_last, root);
/// ```
pub struct DfsPostOrder<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexId, RawDfsExtra>,
}

/// [`DfsPostOrder`] rooted in a single vertex.
pub struct DfsPostOrderRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfsExtra>,
}

/// [`DfsPostOrder`] with possibly multiple roots.
pub struct DfsPostOrderMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitRoots<G::VertexId>,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfsExtra>,
    multi: RawVisitMulti<G, UseVertexId, RawDfsExtra, S>,
}

impl<G> DfsPostOrder<G>
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
    pub fn start(&mut self, root: G::VertexId) -> DfsPostOrderRooted<'_, G> {
        self.raw.start(RawDfsExtraItem::start(root));
        DfsPostOrderRooted { raw: &mut self.raw }
    }

    #[doc = include_str!("../../docs/include/visit.start_all.md")]
    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsPostOrderMulti<'a, G, VisitAll<'a, G>>
    where
        G: VertexSet,
    {
        DfsPostOrderMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    #[doc = include_str!("../../docs/include/visit.start_multi.md")]
    pub fn start_multi<S>(&mut self, roots: S) -> DfsPostOrderMulti<'_, G, S>
    where
        S: VisitRoots<G::VertexId>,
    {
        DfsPostOrderMulti {
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

    #[doc = include_str!("../../docs/include/visit.visited_mut.md")]
    pub fn visited_mut(&mut self) -> &mut impl VisitSet<G::VertexId> {
        &mut self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsPostOrderRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexId;

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
        loop {
            if let RawDfsExtraEvent::Close(vertex) = self.raw.next(graph, |_, _| true)? {
                return Some(vertex);
            }
        }
    }
}

impl<'a, S, G> Visitor<G> for DfsPostOrderMulti<'a, G, S>
where
    G: Neighbors + VertexSet,
    S: VisitRoots<G::VertexId>,
{
    type Item = G::VertexId;

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
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
            .map(RawDfsExtra::id)
    }
}

/// DFS traversal that does not backtrack to explore other branches of the
/// traversal tree after the first one is ended.
///
/// In other words, this traversal implementation always visits only a single
/// neighbor of a vertex, ignoring the other neighbors. This means that visiting
/// all vertices in the graph is **not guaranteed**.
///
/// This traversal algorithm is useful in a greedy initialization of some
/// algorithms before switching to the proper yet costly procedure.
pub struct DfsNoBacktrack<G>
where
    G: GraphBase,
{
    raw: RawVisit<G, UseVertexId, RawDfsNoBacktrack>,
}

/// [`DfsNoBacktrack`] rooted in a single vertex.
pub struct DfsNoBacktrackRooted<'a, G>
where
    G: GraphBase,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfsNoBacktrack>,
}

/// [`DfsNoBacktrack`] with possibly multiple roots.
pub struct DfsNoBacktrackMulti<'a, G, S>
where
    G: GraphBase,
    S: VisitRoots<G::VertexId>,
{
    raw: &'a mut RawVisit<G, UseVertexId, RawDfsNoBacktrack>,
    multi: RawVisitMulti<G, UseVertexId, RawDfsNoBacktrack, S>,
}

impl<G> DfsNoBacktrack<G>
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
    pub fn start(&mut self, root: G::VertexId) -> DfsNoBacktrackRooted<'_, G> {
        self.raw.start(root);
        DfsNoBacktrackRooted { raw: &mut self.raw }
    }

    #[doc = include_str!("../../docs/include/visit.start_all.md")]
    pub fn start_all<'a>(&'a mut self, graph: &'a G) -> DfsNoBacktrackMulti<'a, G, VisitAll<'a, G>>
    where
        G: VertexSet,
    {
        DfsNoBacktrackMulti {
            raw: &mut self.raw,
            multi: RawVisitMulti::new(VisitAll::new(graph)),
        }
    }

    #[doc = include_str!("../../docs/include/visit.start_multi.md")]
    pub fn start_multi<S>(&mut self, roots: S) -> DfsNoBacktrackMulti<'_, G, S>
    where
        S: VisitRoots<G::VertexId>,
    {
        DfsNoBacktrackMulti {
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

    #[doc = include_str!("../../docs/include/visit.visited_mut.md")]
    pub fn visited_mut(&mut self) -> &mut impl VisitSet<G::VertexId> {
        &mut self.raw.visited
    }
}

impl<'a, G> Visitor<G> for DfsNoBacktrackRooted<'a, G>
where
    G: Neighbors,
{
    type Item = G::VertexId;

    fn visit_next(&mut self, graph: &G) -> Option<Self::Item> {
        self.raw.next(graph, |_, _| true)
    }
}

impl<'a, S, G> Visitor<G> for DfsNoBacktrackMulti<'a, G, S>
where
    G: Neighbors + VertexSet,
    S: VisitRoots<G::VertexId>,
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
