use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasherDefault,
    marker::PhantomData,
};

use rustc_hash::FxHashSet;

use crate::core::{
    base::NeighborRef,
    id::{GraphIdTypes, IdType, UseId, UseVertexId},
    marker::Direction,
    GraphBase, Neighbors,
};

use super::VisitSet;

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

pub(crate) trait RawAlgo<Id: GraphIdTypes, U: UseId<Id>> {
    type Item;
    type Collection: TraversalCollection<Self::Item>;

    fn id(item: &Self::Item) -> U::Id;
    fn start(id: &U::Id) -> Self::Item;
    fn visit_on_start() -> bool;
}

pub(crate) struct RawVisit<Id: GraphIdTypes, U: UseId<Id>, A: RawAlgo<Id, U>> {
    pub collection: A::Collection,
    // FixedBitSet cannot be used because there can be vertex additions/removals
    // during the visiting since the Visitors are detached from the graph.
    pub visited: FxHashSet<U::Id>,
}

impl<Id: GraphIdTypes, U: UseId<Id>, A: RawAlgo<Id, U>> RawVisit<Id, U, A> {
    pub fn new(count_hint: Option<usize>) -> Self {
        let visited = count_hint
            .map(|count| HashSet::with_capacity_and_hasher(count, BuildHasherDefault::default()))
            .unwrap_or_default();

        Self {
            collection: A::Collection::default(),
            visited,
        }
    }

    pub fn start(&mut self, root: A::Item) {
        if A::visit_on_start() {
            self.visited.visit(A::id(&root));
        }

        self.collection.clear();
        self.collection.push(root);
    }

    pub fn reset(&mut self) {
        self.collection.clear();
        self.visited.reset_visited();
    }
}

pub trait VisitStarts<Id: IdType> {
    fn get_next(&mut self) -> Option<Id>;

    #[allow(clippy::wrong_self_convention)]
    fn is_done(&mut self, _visited: &impl VisitSet<Id>) -> bool {
        // By default, delegate the indication of being done for `Self::next` by
        // returning `None`.
        false
    }
}

struct VisitStartsIter<'a, Id: IdType, S: VisitStarts<Id>> {
    starts: &'a mut S,
    ty: PhantomData<&'a Id>,
}

impl<'a, Id: IdType, S: VisitStarts<Id>> VisitStartsIter<'a, Id, S> {
    fn new(starts: &'a mut S) -> Self {
        Self {
            starts,
            ty: PhantomData,
        }
    }
}

impl<Id: IdType, S: VisitStarts<Id>> Iterator for VisitStartsIter<'_, Id, S> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        self.starts.get_next()
    }
}

impl<Id: IdType, I> VisitStarts<Id> for I
where
    I: Iterator<Item = Id>,
{
    fn get_next(&mut self) -> Option<Id> {
        self.next()
    }
}

pub(crate) struct RawVisitMulti<Id, U, A, S> {
    pub starts: S,
    ty: PhantomData<(Id, U, A)>,
}

impl<Id: GraphIdTypes, U: UseId<Id>, A: RawAlgo<Id, U>, S: VisitStarts<U::Id>>
    RawVisitMulti<Id, U, A, S>
{
    pub fn new(starts: S) -> Self {
        Self {
            starts,
            ty: PhantomData,
        }
    }

    pub fn next_multi<F, R, G>(
        &mut self,
        raw: &mut RawVisit<Id, U, A>,
        mut get_next: F,
        is_still_valid: G,
    ) -> Option<R>
    where
        F: FnMut(&mut RawVisit<Id, U, A>) -> Option<R>,
        G: Fn(&U::Id) -> bool,
    {
        match get_next(raw) {
            Some(next) => Some(next),
            None => {
                if self.starts.is_done(&raw.visited) {
                    return None;
                }

                // Get the next vertex that has not been visited yet if there is
                // any. Make sure that the id is still valid -- at the very
                // minimum if it is still in the graph (it could have been
                // removed during visiting).
                let root = VisitStartsIter::new(&mut self.starts)
                    .find(|v| !raw.visited.is_visited(v) && is_still_valid(v))?;

                raw.start(A::start(&root));
                get_next(raw)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RawEvent<Id: GraphIdTypes> {
    Popped {
        #[allow(dead_code)]
        vertex: Id::VertexId,
    },
    Push {
        vertex: Id::VertexId,
        src: (Id::VertexId, Id::EdgeId),
    },
    Skip {
        vertex: Id::VertexId,
        src: Option<(Id::VertexId, Id::EdgeId)>,
    },
}

pub enum RawBfs {}

impl<Id: GraphIdTypes> RawAlgo<Id, UseVertexId> for RawBfs {
    type Item = Id::VertexId;
    type Collection = Queue<Id::VertexId>;

    fn id(item: &Id::VertexId) -> Id::VertexId {
        item.clone()
    }

    fn start(id: &Id::VertexId) -> Id::VertexId {
        id.clone()
    }

    fn visit_on_start() -> bool {
        true
    }
}

impl<G: GraphBase> RawVisit<G, UseVertexId, RawBfs> {
    pub fn next<F>(&mut self, graph: &G, mut f: F) -> Option<G::VertexId>
    where
        G: Neighbors,
        F: FnMut(&Self, RawEvent<G>) -> bool,
    {
        let v = self.collection.pop()?;

        let event = RawEvent::Popped { vertex: v.clone() };
        if !f(self, event) {
            return Some(v);
        }

        for n in graph.neighbors_directed(&v, Direction::Outgoing) {
            let u = n.id().into_owned();
            if self.visited.visit(u.clone()) {
                let event = RawEvent::Push {
                    vertex: u.clone(),
                    src: (v.clone(), n.edge().into_owned()),
                };
                if f(self, event) {
                    self.collection.push(u);
                }
            } else {
                let event = RawEvent::Skip {
                    vertex: u,
                    src: Some((v.clone(), n.edge().into_owned())),
                };
                f(self, event);
            }
        }

        Some(v)
    }
}

pub enum RawDfs {}

impl<Id: GraphIdTypes> RawAlgo<Id, UseVertexId> for RawDfs {
    type Item = Id::VertexId;
    type Collection = Stack<Id::VertexId>;

    fn id(item: &Id::VertexId) -> Id::VertexId {
        item.clone()
    }

    fn start(id: &Id::VertexId) -> Id::VertexId {
        id.clone()
    }

    fn visit_on_start() -> bool {
        false
    }
}

impl<G: GraphBase> RawVisit<G, UseVertexId, RawDfs> {
    pub fn next<F>(&mut self, graph: &G, mut f: F) -> Option<G::VertexId>
    where
        G: Neighbors,
        F: FnMut(&Self, RawEvent<G>) -> bool,
    {
        while let Some(v) = self.collection.pop() {
            if self.visited.visit(v.clone()) {
                let event = RawEvent::Popped { vertex: v.clone() };
                if !f(self, event) {
                    return Some(v);
                }

                for n in graph.neighbors_directed(&v, Direction::Outgoing) {
                    let u = n.id().into_owned();
                    if !self.visited.is_visited(&u) {
                        let event = RawEvent::Push {
                            vertex: u.clone(),
                            src: (v.clone(), n.edge().into_owned()),
                        };
                        if f(self, event) {
                            self.collection.push(u.clone());
                        }
                    } else {
                        let event = RawEvent::Skip {
                            vertex: u.clone(),
                            src: Some((v.clone(), n.edge().into_owned())),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawDfsExtraItem<Id: GraphIdTypes> {
    vertex: Id::VertexId,
    neighbors: Vec<(Id::VertexId, Id::EdgeId)>,
}

impl<Id: GraphIdTypes> RawDfsExtraItem<Id> {
    pub fn start(root: Id::VertexId) -> Self {
        Self {
            vertex: root,
            neighbors: Vec::new(),
        }
    }

    pub fn closed(vertex: Id::VertexId) -> Self {
        Self {
            vertex,
            neighbors: Vec::new(),
        }
    }

    fn restart<G>(&mut self, graph: &G)
    where
        G: Neighbors<VertexId = Id::VertexId, EdgeId = Id::EdgeId>,
    {
        self.neighbors.clear();
        self.neighbors.extend(
            graph
                .neighbors_directed(&self.vertex, Direction::Outgoing)
                .map(|n| (n.id().into_owned(), n.edge().into_owned())),
        );
    }

    fn open<G>(vertex: G::VertexId, graph: &G) -> Self
    where
        G: Neighbors<VertexId = Id::VertexId, EdgeId = Id::EdgeId>,
    {
        let neighbors = graph
            .neighbors_directed(&vertex, Direction::Outgoing)
            .map(|n| (n.id().into_owned(), n.edge().into_owned()))
            .collect();

        Self { vertex, neighbors }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RawDfsExtraEvent<Id: GraphIdTypes> {
    Open(Id::VertexId),
    Close(Id::VertexId),
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
impl<G: GraphBase> RawAlgo<G, UseVertexId> for RawDfsExtra {
    type Item = RawDfsExtraItem<G>;
    type Collection = Stack<RawDfsExtraItem<G>>;

    fn id(item: &RawDfsExtraItem<G>) -> G::VertexId {
        item.vertex.clone()
    }

    fn start(id: &G::VertexId) -> RawDfsExtraItem<G> {
        RawDfsExtraItem::start(id.clone())
    }

    fn visit_on_start() -> bool {
        false
    }
}

impl<Id: GraphBase> RawVisit<Id, UseVertexId, RawDfsExtra> {
    pub fn next<G, F>(&mut self, graph: &G, mut f: F) -> Option<RawDfsExtraEvent<G>>
    where
        G: Neighbors<VertexId = Id::VertexId, EdgeId = Id::EdgeId>,
        F: FnMut(&Self, RawEvent<G>) -> bool,
    {
        let mut v_item = self.collection.pop()?;
        let v = RawDfsExtra::id(&v_item);

        if self.collection.0.is_empty() && !self.visited.is_visited(&v) {
            // If the vertex is the start and has not been expanded yet, we need
            // to initialize its neighbors and mark it as visited. This is the
            // consequence of chosen API, but it is considered an acceptable
            // tradeoff.
            v_item.restart(graph);
            self.collection.push(v_item);
            self.visited.visit(v.clone());
            return Some(RawDfsExtraEvent::Open(v));
        }

        while let Some((u, e)) = v_item.neighbors.pop() {
            if self.visited.visit(u.clone()) {
                let event = RawEvent::Popped { vertex: v.clone() };
                if !f(self, event) {
                    continue;
                }

                let event = RawEvent::Push {
                    vertex: u.clone(),
                    src: (v.clone(), e),
                };
                if !f(self, event) {
                    continue;
                }

                // Not all neighbors processed yet. Return the vertex back to the
                // stack before pushing its neighbor.
                self.collection.push(v_item);

                // Open the neighbor and store its neighbors onto the stack so
                // they are processed in the next step.
                self.collection
                    .push(RawDfsExtraItem::open(u.clone(), graph));
                return Some(RawDfsExtraEvent::Open(u));
            } else {
                let event = RawEvent::Skip {
                    vertex: u,
                    src: Some((v.clone(), e)),
                };
                f(self, event);
            }
        }

        // All neighbors exhausted.
        Some(RawDfsExtraEvent::Close(v))
    }
}

pub enum RawDfsNoBacktrack {}

impl<Id: GraphIdTypes> RawAlgo<Id, UseVertexId> for RawDfsNoBacktrack {
    type Item = Id::VertexId;
    type Collection = Single<Id::VertexId>;

    fn id(item: &Id::VertexId) -> Id::VertexId {
        item.clone()
    }

    fn start(id: &Id::VertexId) -> Id::VertexId {
        id.clone()
    }

    fn visit_on_start() -> bool {
        true
    }
}

impl<G: GraphBase> RawVisit<G, UseVertexId, RawDfsNoBacktrack> {
    pub fn next<F>(&mut self, graph: &G, mut f: F) -> Option<G::VertexId>
    where
        G: Neighbors,
        F: FnMut(&Self, RawEvent<G>) -> bool,
    {
        let v = self.collection.pop()?;

        let event = RawEvent::Popped { vertex: v.clone() };
        if !f(self, event) {
            return Some(v);
        }

        let mut neighbor_chosen = false;

        for n in graph.neighbors_directed(&v, Direction::Outgoing) {
            let u = n.id().into_owned();
            if !neighbor_chosen && self.visited.visit(u.clone()) {
                let event = RawEvent::Push {
                    vertex: u.clone(),
                    src: (v.clone(), n.edge().into_owned()),
                };
                if f(self, event) {
                    self.collection.push(u.clone());

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
                    src: Some((v.clone(), n.edge().into_owned())),
                };
                f(self, event);
            }
        }

        Some(v)
    }
}
