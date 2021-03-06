use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasherDefault,
    marker::PhantomData,
};

use rustc_hash::FxHashSet;

use crate::infra::VisitSet;
use crate::traits::*;
use crate::{
    index::{EdgeIndex, IndexType, VertexIndex},
    marker::Outgoing,
};

pub trait TraversalCollection<T>: Default {
    fn push(&mut self, value: T);
    fn pop(&mut self) -> Option<T>;
    fn clear(&mut self);
}

pub struct Queue<T>(pub VecDeque<T>);

impl<T> Default for Queue<T> {
    fn default() -> Self {
        Self(VecDeque::new())
    }
}

impl<T> TraversalCollection<T> for Queue<T> {
    fn push(&mut self, value: T) {
        self.0.push_back(value);
    }

    fn pop(&mut self) -> Option<T> {
        self.0.pop_front()
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

#[derive(Debug)]
pub struct Stack<T>(pub Vec<T>);

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T> TraversalCollection<T> for Stack<T> {
    fn push(&mut self, value: T) {
        self.0.push(value);
    }

    fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

#[derive(Debug)]
pub struct Single<T>(pub Option<T>);

impl<T> Default for Single<T> {
    fn default() -> Self {
        Self(None)
    }
}

impl<T> TraversalCollection<T> for Single<T> {
    fn push(&mut self, value: T) {
        self.0 = Some(value);
    }

    fn pop(&mut self) -> Option<T> {
        self.0.take()
    }

    fn clear(&mut self) {
        self.0 = None;
    }
}

pub trait RawAlgo {
    type Index: IndexType;
    type Item;
    type Collection: TraversalCollection<Self::Item>;

    fn index(item: &Self::Item) -> Self::Index;
    fn start(index: Self::Index) -> Self::Item;
    fn visit_on_start() -> bool;
}

pub struct RawVisit<A: RawAlgo> {
    pub collection: A::Collection,
    // FixedBitSet cannot be used because there can be vertex additions/removals
    // during the visiting since the Visitors are detached from the graph.
    pub visited: FxHashSet<A::Index>,
}

impl<A: RawAlgo> RawVisit<A> {
    pub fn new(count_hint: Option<usize>) -> Self {
        let visited = count_hint
            .map(|count| HashSet::with_capacity_and_hasher(count, BuildHasherDefault::default()))
            .unwrap_or_else(FxHashSet::default);

        Self {
            collection: A::Collection::default(),
            visited,
        }
    }

    pub fn start(&mut self, root: A::Item) {
        if A::visit_on_start() {
            self.visited.visit(A::index(&root));
        }

        self.collection.clear();
        self.collection.push(root);
    }

    pub fn reset(&mut self) {
        self.collection.clear();
        self.visited.reset_visited();
    }
}

pub trait VisitStarts<T: IndexType = VertexIndex> {
    fn get_next(&mut self) -> Option<T>;

    #[allow(clippy::wrong_self_convention)]
    fn is_done(&mut self, _visited: &impl VisitSet<T>) -> bool {
        // By default, delegate the indication of being done for `Self::next` by
        // returning `None`.
        false
    }
}

struct VisitStartsIter<'a, T: IndexType, S: VisitStarts<T>> {
    starts: &'a mut S,
    ty: PhantomData<&'a T>,
}

impl<'a, T: IndexType, S: VisitStarts<T>> VisitStartsIter<'a, T, S> {
    fn new(starts: &'a mut S) -> Self {
        Self {
            starts,
            ty: PhantomData,
        }
    }
}

impl<T: IndexType, S: VisitStarts<T>> Iterator for VisitStartsIter<'_, T, S> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.starts.get_next()
    }
}

impl<T: IndexType, I> VisitStarts<T> for I
where
    I: Iterator<Item = T>,
{
    fn get_next(&mut self) -> Option<T> {
        self.next()
    }
}

pub struct RawVisitMulti<A, S> {
    pub starts: S,
    ty: PhantomData<A>,
}

impl<A: RawAlgo, S: VisitStarts<A::Index>> RawVisitMulti<A, S> {
    pub fn new(starts: S) -> Self {
        Self {
            starts,
            ty: PhantomData,
        }
    }

    pub fn next_multi<F, R, G>(
        &mut self,
        raw: &mut RawVisit<A>,
        mut get_next: F,
        is_still_valid: G,
    ) -> Option<R>
    where
        F: FnMut(&mut RawVisit<A>) -> Option<R>,
        G: Fn(&A::Index) -> bool,
    {
        match get_next(raw) {
            Some(next) => Some(next),
            None => {
                if self.starts.is_done(&raw.visited) {
                    return None;
                }

                // Get the next vertex that has not been visited yet if there is
                // any. Make sure that the index is still valid -- at the very
                // minimum if it is still in the graph (it could have been
                // removed during visiting).
                let root = VisitStartsIter::new(&mut self.starts)
                    .find(|v| !raw.visited.is_visited(*v) && is_still_valid(v))?;

                raw.start(A::start(root));
                get_next(raw)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RawEvent {
    Popped {
        vertex: VertexIndex,
    },
    Push {
        vertex: VertexIndex,
        src: (VertexIndex, EdgeIndex),
    },
    Skip {
        vertex: VertexIndex,
        src: Option<(VertexIndex, EdgeIndex)>,
    },
}

pub enum RawBfs {}

impl RawAlgo for RawBfs {
    type Index = VertexIndex;
    type Item = VertexIndex;
    type Collection = Queue<VertexIndex>;

    fn index(item: &Self::Item) -> Self::Index {
        *item
    }

    fn start(index: Self::Index) -> Self::Item {
        index
    }

    fn visit_on_start() -> bool {
        true
    }
}

impl RawVisit<RawBfs> {
    pub fn next<G, F>(&mut self, graph: &G, mut f: F) -> Option<VertexIndex>
    where
        G: Neighbors,
        F: FnMut(&Self, RawEvent) -> bool,
    {
        let v = self.collection.pop()?;

        let event = RawEvent::Popped { vertex: v };
        if !f(self, event) {
            return Some(v);
        }

        for n in graph.neighbors_directed(v, Outgoing) {
            let u = n.index();
            if self.visited.visit(u) {
                let event = RawEvent::Push {
                    vertex: u,
                    src: (v, n.edge()),
                };
                if f(self, event) {
                    self.collection.push(u);
                }
            } else {
                let event = RawEvent::Skip {
                    vertex: u,
                    src: Some((v, n.edge())),
                };
                f(self, event);
            }
        }

        Some(v)
    }
}

pub enum RawDfs {}

impl RawAlgo for RawDfs {
    type Index = VertexIndex;
    type Item = VertexIndex;
    type Collection = Stack<VertexIndex>;

    fn index(item: &Self::Item) -> Self::Index {
        *item
    }

    fn start(index: Self::Index) -> Self::Item {
        index
    }

    fn visit_on_start() -> bool {
        false
    }
}

impl RawVisit<RawDfs> {
    pub fn next<G, F>(&mut self, graph: &G, mut f: F) -> Option<VertexIndex>
    where
        G: Neighbors,
        F: FnMut(&Self, RawEvent) -> bool,
    {
        while let Some(v) = self.collection.pop() {
            if self.visited.visit(v) {
                let event = RawEvent::Popped { vertex: v };
                if !f(self, event) {
                    return Some(v);
                }

                for n in graph.neighbors_directed(v, Outgoing) {
                    let u = n.index();
                    if !self.visited.is_visited(u) {
                        let event = RawEvent::Push {
                            vertex: u,
                            src: (v, n.edge()),
                        };
                        if f(self, event) {
                            self.collection.push(u);
                        }
                    } else {
                        let event = RawEvent::Skip {
                            vertex: u,
                            src: Some((v, n.edge())),
                        };
                        f(self, event);
                    }
                }

                return Some(v);
            } else {
                let event = RawEvent::Skip {
                    vertex: v,
                    src: None,
                };
                f(self, event);
            }
        }

        None
    }
}

pub enum RawDfsExtra {}

#[derive(Debug, Clone, PartialEq)]
pub struct RawDfsExtraItem {
    vertex: VertexIndex,
    neighbors: Vec<(VertexIndex, EdgeIndex)>,
}

impl RawDfsExtraItem {
    pub fn start(root: VertexIndex) -> Self {
        Self {
            vertex: root,
            neighbors: Vec::new(),
        }
    }

    pub fn closed(vertex: VertexIndex) -> Self {
        Self {
            vertex,
            neighbors: Vec::new(),
        }
    }

    fn restart<G>(&mut self, graph: &G)
    where
        G: Neighbors,
    {
        self.neighbors.clear();
        self.neighbors.extend(
            graph
                .neighbors_directed(self.vertex, Outgoing)
                .map(|n| (n.index(), n.edge())),
        );
    }

    fn open<G>(vertex: VertexIndex, graph: &G) -> Self
    where
        G: Neighbors,
    {
        Self {
            vertex,
            neighbors: graph
                .neighbors_directed(vertex, Outgoing)
                .map(|n| (n.index(), n.edge()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawDfsExtraEvent {
    Open(VertexIndex),
    Close(VertexIndex),
}

// Stack of "iterators" as described in
// https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html.
// This is needed to be able to correctly detect back edges (not signalling two
// tree edges leading to a single vertex) but at the same time being a correct
// DFS traversal order (which rules out alternatives such as labelling just
// discovered vertices in a separate set). Providing a valid DFS order instead
// of "stack traversal" order (see reference) may be essential for some
// algorithms.
//
// This imposes some overhead compared to a straightforward recursive version.
// The users are free to implement the recursive algorithm if needed (we may
// provide it too in the future).
impl RawAlgo for RawDfsExtra {
    type Index = VertexIndex;
    type Item = RawDfsExtraItem;
    type Collection = Stack<RawDfsExtraItem>;

    fn index(item: &Self::Item) -> Self::Index {
        item.vertex
    }

    fn start(index: Self::Index) -> Self::Item {
        RawDfsExtraItem::start(index)
    }

    fn visit_on_start() -> bool {
        false
    }
}

impl RawVisit<RawDfsExtra> {
    pub fn next<G, F>(&mut self, graph: &G, mut f: F) -> Option<RawDfsExtraEvent>
    where
        G: Neighbors,
        F: FnMut(&Self, RawEvent) -> bool,
    {
        let mut v_item = self.collection.pop()?;
        let v = RawDfsExtra::index(&v_item);

        if self.collection.0.is_empty() && !self.visited.is_visited(v) {
            // If the vertex is the start and has not been expanded yet, we need
            // to initialize its neighbors and mark it as visited. This is the
            // consequence of chosen API, but it is considered an acceptable
            // tradeoff.
            v_item.restart(graph);
            self.collection.push(v_item);
            self.visited.visit(v);
            return Some(RawDfsExtraEvent::Open(v));
        }

        while let Some((u, e)) = v_item.neighbors.pop() {
            if self.visited.visit(u) {
                let event = RawEvent::Popped { vertex: v };
                if !f(self, event) {
                    continue;
                }

                let event = RawEvent::Push {
                    vertex: u,
                    src: (v, e),
                };
                if !f(self, event) {
                    continue;
                }

                // Not all neighbors processed yet. Return the vertex back to the
                // stack before pushing its neighbor.
                self.collection.push(v_item);

                // Open the neighbor and store its neighbors onto the stack so
                // they are processed in the next step.
                self.collection.push(RawDfsExtraItem::open(u, graph));
                return Some(RawDfsExtraEvent::Open(u));
            } else {
                let event = RawEvent::Skip {
                    vertex: u,
                    src: Some((v, e)),
                };
                f(self, event);
            }
        }

        // All neighbors exhausted.
        Some(RawDfsExtraEvent::Close(v))
    }
}

pub enum RawDfsNoBacktrack {}

impl RawAlgo for RawDfsNoBacktrack {
    type Index = VertexIndex;
    type Item = VertexIndex;
    type Collection = Single<VertexIndex>;

    fn index(item: &Self::Item) -> Self::Index {
        *item
    }

    fn start(index: Self::Index) -> Self::Item {
        index
    }

    fn visit_on_start() -> bool {
        true
    }
}

impl RawVisit<RawDfsNoBacktrack> {
    pub fn next<G, F>(&mut self, graph: &G, mut f: F) -> Option<VertexIndex>
    where
        G: Neighbors,
        F: FnMut(&Self, RawEvent) -> bool,
    {
        let v = self.collection.pop()?;

        let event = RawEvent::Popped { vertex: v };
        if !f(self, event) {
            return Some(v);
        }

        let mut neighbor_chosen = false;

        for n in graph.neighbors_directed(v, Outgoing) {
            let u = n.index();
            if !neighbor_chosen && self.visited.visit(u) {
                let event = RawEvent::Push {
                    vertex: u,
                    src: (v, n.edge()),
                };
                if f(self, event) {
                    self.collection.push(u);

                    // Take just one neighbor and ignore the rest, i.e., don't
                    // backtrack to this vertex. However, just mark this fact
                    // and continue the iteration to signal `Skip` events. In
                    // trivial cases when `f = |_, _| true` the compiler might
                    // be able to optimize to break the iteration.
                    neighbor_chosen = true;
                }
            } else {
                let event = RawEvent::Skip {
                    vertex: u,
                    src: Some((v, n.edge())),
                };
                f(self, event);
            }
        }

        Some(v)
    }
}
