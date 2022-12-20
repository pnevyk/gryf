use crate::core::{
    facts,
    marker::{Direction, EdgeType},
    Create,
};

pub fn create_complete<V, E, Ty: EdgeType, G>(vertex_count: usize) -> G
where
    V: Default,
    E: Default,
    G: Create<V, E, Ty>,
{
    let mut graph = G::with_capacity(
        vertex_count,
        facts::complete_graph_edge_count::<Ty>(vertex_count),
    );

    let vertices = (0..vertex_count)
        .map(|_| graph.add_vertex(V::default()))
        .collect::<Vec<_>>();

    for u in vertices.clone() {
        for v in vertices.clone() {
            if u == v {
                continue;
            }

            if !Ty::is_directed() && v > u {
                break;
            }

            graph.add_edge(&u, &v, E::default());
        }
    }

    graph
}

pub fn create_bipartite<V, E, Ty: EdgeType, G, F>(
    vertex_count_lhs: usize,
    vertex_count_rhs: usize,
    connect: F,
) -> G
where
    V: Default,
    G: Create<V, E, Ty>,
    F: Fn(&G::VertexIndex, &G::VertexIndex, Direction) -> Option<E>,
{
    let vertex_count = vertex_count_lhs + vertex_count_rhs;

    let mut graph = G::with_capacity(vertex_count, vertex_count);

    let vertices_lhs = (0..vertex_count_lhs)
        .map(|_| graph.add_vertex(V::default()))
        .collect::<Vec<_>>();

    let vertices_rhs = (0..vertex_count_rhs)
        .map(|_| graph.add_vertex(V::default()))
        .collect::<Vec<_>>();

    for dir in Ty::directions() {
        for i in vertices_lhs.clone() {
            for j in vertices_rhs.clone() {
                if let Some(edge) = connect(&i, &j, *dir) {
                    match dir {
                        Direction::Outgoing => graph.add_edge(&i, &j, edge),
                        Direction::Incoming => graph.add_edge(&j, &i, edge),
                    };
                }
            }
        }
    }

    graph
}

pub fn create_path<V, E, Ty: EdgeType, G>(vertex_count: usize) -> G
where
    V: Default,
    E: Default,
    G: Create<V, E, Ty>,
{
    if vertex_count == 0 {
        return G::default();
    }

    let mut graph = G::with_capacity(vertex_count, vertex_count - 1);
    let mut src = graph.add_vertex(V::default());

    for _ in 1..vertex_count {
        let dst = graph.add_vertex(V::default());
        graph.add_edge(&src, &dst, E::default());
        src = dst;
    }

    graph
}

#[cfg(any(feature = "proptest", feature = "arbitrary"))]
pub use random::*;

#[cfg(any(feature = "proptest", feature = "arbitrary"))]
mod random {
    use std::{fmt, io::Cursor, marker::PhantomData};

    use crate::{
        core::{
            index::NumIndexType, marker::EdgeType, Create, Edges, EdgesMut, Vertices, VerticesMut,
        },
        infra::export::{Dot, Export},
    };

    use crate::derive::{
        Edges, EdgesBase, EdgesBaseWeak, EdgesWeak, GraphBase, Guarantee, Neighbors, Vertices,
        VerticesBase, VerticesBaseWeak, VerticesWeak,
    };

    // TODO: Remove these imports once hygiene of procedural macros is fixed.
    use crate::common::CompactIndexMap;
    use crate::core::{
        marker::Direction, EdgesBase, EdgesBaseWeak, EdgesWeak, GraphBase, Guarantee, Neighbors,
        VerticesBase, VerticesBaseWeak, VerticesWeak, WeakRef,
    };

    #[cfg(feature = "proptest")]
    use proptest_derive::Arbitrary;

    #[derive(Clone, Copy, PartialEq, Eq)]
    #[cfg_attr(feature = "proptest", derive(Arbitrary))]
    pub enum MutOp<V, E, Ty: EdgeType> {
        #[cfg_attr(feature = "proptest", proptest(weight = 4))]
        AddVertex(V),
        #[cfg_attr(feature = "proptest", proptest(weight = 1))]
        RemoveVertex(usize),
        #[cfg_attr(feature = "proptest", proptest(weight = 10))]
        AddEdge(usize, usize, E, PhantomData<Ty>),
        #[cfg_attr(feature = "proptest", proptest(weight = 1))]
        RemoveEdge(usize, usize),
    }

    impl<V, E, Ty: EdgeType> fmt::Debug for MutOp<V, E, Ty>
    where
        V: fmt::Debug,
        E: fmt::Debug,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::AddVertex(vertex) => write!(f, "add vertex {:?}", vertex),
                Self::RemoveVertex(index) => write!(f, "remove vertex v{}", index),
                Self::AddEdge(src, dst, edge, _) => {
                    if Ty::is_directed() {
                        write!(f, "add edge v{} -> v{} {:?}", src, dst, edge)
                    } else {
                        write!(f, "add edge v{} -- v{} {:?}", src, dst, edge)
                    }
                }
                Self::RemoveEdge(src, dst) => {
                    if Ty::is_directed() {
                        write!(f, "remove edge v{} -> v{}", src, dst)
                    } else {
                        write!(f, "remove edge v{} -- v{}", src, dst)
                    }
                }
            }
        }
    }

    pub trait ApplyMutOps<V, E, Ty: EdgeType> {
        fn apply_one(&mut self, op: MutOp<V, E, Ty>);
        fn apply_many(&mut self, ops: impl Iterator<Item = MutOp<V, E, Ty>>) {
            for op in ops {
                self.apply_one(op);
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct ApplyOptions {
        pub loops: bool,
        pub multi_edges: bool,
        pub remove_vertices: bool,
        pub remove_edges: bool,
    }

    impl Default for ApplyOptions {
        fn default() -> Self {
            Self {
                loops: true,
                multi_edges: true,
                remove_vertices: true,
                remove_edges: true,
            }
        }
    }

    impl ApplyOptions {
        pub fn loops(mut self, allow: bool) -> Self {
            self.loops = allow;
            self
        }

        pub fn multi_edges(mut self, allow: bool) -> Self {
            self.multi_edges = allow;
            self
        }

        pub fn remove_vertices(mut self, allow: bool) -> Self {
            self.remove_vertices = allow;
            self
        }

        pub fn remove_edges(mut self, allow: bool) -> Self {
            self.remove_edges = allow;
            self
        }
    }

    pub struct Applier<'g, G> {
        graph: &'g mut G,
        options: ApplyOptions,
    }

    impl<'g, G> Applier<'g, G> {
        pub fn new(graph: &'g mut G) -> Self {
            Self::with_options(graph, ApplyOptions::default())
        }

        pub fn with_options(graph: &'g mut G, options: ApplyOptions) -> Self {
            Self { graph, options }
        }
    }

    impl<V, E, Ty: EdgeType, G> ApplyMutOps<V, E, Ty> for Applier<'_, G>
    where
        G: VerticesMut<V> + EdgesMut<E, Ty>,
        G::VertexIndex: NumIndexType,
    {
        fn apply_one(&mut self, op: MutOp<V, E, Ty>) {
            match op {
                MutOp::AddVertex(vertex) => {
                    self.graph.add_vertex(vertex);
                }
                MutOp::RemoveVertex(index) => {
                    if !self.options.remove_vertices {
                        return;
                    }

                    if self.graph.vertex_count() > 0 {
                        let map = self.graph.vertex_index_map();
                        let index = map.real(index % self.graph.vertex_count()).unwrap();
                        self.graph.remove_vertex(&index);
                    }
                }
                MutOp::AddEdge(src, dst, edge, _) => {
                    if self.graph.vertex_count() > 0 {
                        let map = self.graph.vertex_index_map();
                        let src = map.real(src % self.graph.vertex_count()).unwrap();
                        let dst = map.real(dst % self.graph.vertex_count()).unwrap();

                        if !self.options.loops && src == dst {
                            return;
                        }

                        if !self.options.multi_edges && self.graph.edge_index(&src, &dst).is_some()
                        {
                            return;
                        }

                        self.graph.add_edge(&src, &dst, edge);
                    }
                }
                MutOp::RemoveEdge(src, dst) => {
                    if !self.options.remove_edges {
                        return;
                    }

                    if self.graph.vertex_count() > 0 {
                        let map = self.graph.vertex_index_map();
                        let src = map.real(src % self.graph.vertex_count()).unwrap();
                        let dst = map.real(dst % self.graph.vertex_count()).unwrap();
                        if let Some(index) = self.graph.edge_index(&src, &dst) {
                            self.graph.remove_edge(&index);
                        }
                    }
                }
            }
        }
    }

    #[derive(
        GraphBase,
        VerticesBase,
        Vertices,
        EdgesBase,
        Edges,
        Neighbors,
        VerticesBaseWeak,
        VerticesWeak,
        EdgesBaseWeak,
        EdgesWeak,
        Guarantee,
    )]
    pub struct DebugGraph<V, E, Ty: EdgeType, G> {
        #[graph]
        graph: G,
        dot: Dot<V, E, Ty>,
        history: Vec<MutOp<V, E, Ty>>,
    }

    impl<V, E, Ty: EdgeType, G> DebugGraph<V, E, Ty, G>
    where
        V: fmt::Debug,
        E: fmt::Debug,
    {
        pub fn new(graph: G) -> Self {
            Self {
                graph,
                dot: Dot::new(None, |v| format!("{:?}", v), |e| format!("{:?}", e)),
                history: Vec::new(),
            }
        }
    }

    impl<V, E, Ty: EdgeType, G> Default for DebugGraph<V, E, Ty, G>
    where
        V: fmt::Debug,
        E: fmt::Debug,
        G: Default,
    {
        fn default() -> Self {
            Self::new(G::default())
        }
    }

    impl<V, E, Ty: EdgeType, G> Create<V, E, Ty> for DebugGraph<V, E, Ty, G>
    where
        V: Clone + fmt::Debug,
        E: Clone + fmt::Debug,
        G: Create<V, E, Ty>,
        G::VertexIndex: NumIndexType,
    {
        fn with_capacity(vertex_count: usize, edge_count: usize) -> Self {
            Self::new(G::with_capacity(vertex_count, edge_count))
        }
    }

    impl<V, E, Ty: EdgeType, G> VerticesMut<V> for DebugGraph<V, E, Ty, G>
    where
        V: Clone,
        G: VerticesMut<V>,
        G::VertexIndex: NumIndexType,
    {
        fn vertex_mut(&mut self, index: &Self::VertexIndex) -> Option<&mut V> {
            self.graph.vertex_mut(index)
        }

        fn add_vertex(&mut self, vertex: V) -> Self::VertexIndex {
            self.history.push(MutOp::AddVertex(vertex.clone()));
            self.graph.add_vertex(vertex)
        }

        fn remove_vertex(&mut self, index: &Self::VertexIndex) -> Option<V> {
            self.history.push(MutOp::RemoveVertex(index.to_usize()));
            self.graph.remove_vertex(index)
        }

        fn replace_vertex(&mut self, index: &Self::VertexIndex, vertex: V) -> V {
            self.graph.replace_vertex(index, vertex)
        }
    }

    impl<V, E, Ty: EdgeType, G> EdgesMut<E, Ty> for DebugGraph<V, E, Ty, G>
    where
        E: Clone,
        G: EdgesMut<E, Ty>,
        G::VertexIndex: NumIndexType,
    {
        fn edge_mut(&mut self, index: &Self::EdgeIndex) -> Option<&mut E> {
            self.graph.edge_mut(index)
        }

        fn add_edge(
            &mut self,
            src: &Self::VertexIndex,
            dst: &Self::VertexIndex,
            edge: E,
        ) -> Self::EdgeIndex {
            self.history.push(MutOp::AddEdge(
                src.to_usize(),
                dst.to_usize(),
                edge.clone(),
                PhantomData,
            ));
            self.graph.add_edge(src, dst, edge)
        }

        fn remove_edge(&mut self, index: &Self::EdgeIndex) -> Option<E> {
            if let Some((src, dst)) = self.endpoints(index) {
                self.history
                    .push(MutOp::RemoveEdge(src.to_usize(), dst.to_usize()));
            }
            self.graph.remove_edge(index)
        }

        fn replace_edge(&mut self, index: &Self::EdgeIndex, edge: E) -> E {
            self.graph.replace_edge(index, edge)
        }
    }

    impl<V, E, Ty: EdgeType, G> fmt::Debug for DebugGraph<V, E, Ty, G>
    where
        V: fmt::Debug,
        E: fmt::Debug,
        G: fmt::Debug + Vertices<V> + Edges<E, Ty>,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut exported = Cursor::new(Vec::new());
            self.dot.export(&self.graph, &mut exported).unwrap();
            let exported = String::from_utf8(exported.into_inner()).unwrap();

            struct Exported(String);

            impl fmt::Debug for Exported {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "\n\n{}\n", self.0)
                }
            }

            f.debug_struct("DebugGraph")
                .field("graph", &self.graph)
                .field("history", &self.history)
                .field("dot", &Exported(exported))
                .finish()
        }
    }
}
