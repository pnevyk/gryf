use std::{collections::BTreeMap, fmt, marker::PhantomData};

use proptest::{
    prelude::Rng,
    strategy::{Strategy, ValueTree},
};
use rustc_hash::FxHashSet;

use crate::{
    core::{
        create::Create,
        marker::{Directed, EdgeType, Undirected},
        GraphAdd,
    },
    graph::Graph,
};

use super::testing::AsDot;

pub fn graph<V: Strategy, E: Strategy, Ty: EdgeType>(
    vertex: V,
    edge: E,
) -> GraphStrategy<V, E, Ty> {
    GraphStrategy::new(vertex, edge)
}

pub fn graph_undirected<V: Strategy, E: Strategy>(
    vertex: V,
    edge: E,
) -> GraphStrategy<V, E, Undirected> {
    GraphStrategy::new(vertex, edge)
}

pub fn graph_directed<V: Strategy, E: Strategy>(
    vertex: V,
    edge: E,
) -> GraphStrategy<V, E, Directed> {
    GraphStrategy::new(vertex, edge)
}

#[derive(Debug, Clone)]
pub struct GraphValueTree<V: ValueTree, E: ValueTree, Ty: EdgeType, G> {
    vertices: Vec<V>,
    edges: Vec<(usize, usize, E)>,
    structure: Option<ShrinkStructureState>,
    attr: Option<ShrinkAttrState>,
    ty: PhantomData<Ty>,
    graph: PhantomData<G>,
    no_shrink: bool,
}

pub struct GraphStrategy<
    V: Strategy,
    E: Strategy,
    Ty: EdgeType,
    G = Graph<<V as Strategy>::Value, <E as Strategy>::Value, Ty>,
> {
    vertex: V,
    edge: E,
    ty: PhantomData<Ty>,
    graph: PhantomData<G>,
    params: StrategyParams,
}

// G is phantom data, we should not require Debug bound on it.
impl<V: Strategy, E: Strategy, Ty: EdgeType, G> fmt::Debug for GraphStrategy<V, E, Ty, G> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GraphStrategy")
            .field("vertex", &self.vertex)
            .field("edge", &self.edge)
            .field("ty", &self.ty)
            .field("graph", &self.graph)
            .field("params", &self.params)
            .finish()
    }
}

macro_rules! delegate_builder_fn {
    ($name:ident$(, $param:ident: $param_type:ty)*) => {
        #[doc = concat!("See [StrategyParams::", stringify!($name), "](StrategyParams::", stringify!($name), ") for details.")]
        pub fn $name(self, $($param: $param_type),*) -> Self {
            Self {
                params: self.params.$name($($param,)*),
                ..self
            }
        }
    }
}

impl<V: Strategy, E: Strategy, Ty: EdgeType> GraphStrategy<V, E, Ty> {
    pub fn new(vertex: V, edge: E) -> Self {
        Self::with_params(vertex, edge, StrategyParams::default())
    }

    pub fn new_in<G>(vertex: V, edge: E) -> GraphStrategy<V, E, Ty, G> {
        GraphStrategy::with_params_in(vertex, edge, StrategyParams::default())
    }

    pub fn with_params(vertex: V, edge: E, params: StrategyParams) -> Self {
        Self {
            vertex,
            edge,
            ty: PhantomData,
            graph: PhantomData,
            params,
        }
    }

    pub fn with_params_in<G>(
        vertex: V,
        edge: E,
        params: StrategyParams,
    ) -> GraphStrategy<V, E, Ty, G> {
        GraphStrategy {
            vertex,
            edge,
            ty: PhantomData,
            graph: PhantomData,
            params,
        }
    }

    // Builder pattern on the strategy itself to allow usage as in
    // `graph(0..10, 0..10).max_size(100).acyclic()`.
    delegate_builder_fn!(max_size, max_size: usize);
    delegate_builder_fn!(acyclic);
    delegate_builder_fn!(connected);
    delegate_builder_fn!(allow_loops);
    delegate_builder_fn!(multi_edge_prob, multi_edge_prob: f32);
    delegate_builder_fn!(model, model: RandomModel);
    delegate_builder_fn!(class, class: GraphClass);
    delegate_builder_fn!(density, density: f32);
    delegate_builder_fn!(sparse);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RandomModel {
    // Erdős–Rényi model
    Uniform,
    // Watts-Strogatz model
    SmallWorlds,
    // Barabási-Albert model
    PreferentialAttachment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphClass {
    Forest,
    Bipartite,
}

#[derive(Debug)]
pub struct StrategyParams {
    max_size: usize,
    acyclic: bool,
    connected: bool,
    allow_loops: bool,
    multi_edge_prob: f32,
    model: RandomModel,
    class: Option<GraphClass>,
    // (0, 1] - 1 means no limitation in choosing p, lower values artificially decrease chosen p
    density: f32,
}

impl Default for StrategyParams {
    fn default() -> Self {
        Self {
            max_size: 512,
            acyclic: false,
            connected: false,
            allow_loops: false,
            multi_edge_prob: 0.0,
            model: RandomModel::Uniform,
            class: None,
            density: 1.0,
        }
    }
}

impl StrategyParams {
    pub fn max_size(self, max_size: usize) -> Self {
        Self { max_size, ..self }
    }

    pub fn acyclic(self) -> Self {
        Self {
            acyclic: true,
            ..self
        }
    }

    pub fn connected(self) -> Self {
        Self {
            connected: true,
            ..self
        }
    }

    pub fn allow_loops(self) -> Self {
        Self {
            allow_loops: true,
            ..self
        }
    }

    pub fn multi_edge_prob(self, multi_edge_prob: f32) -> Self {
        assert!(
            (0.0..=0.1).contains(&multi_edge_prob),
            "multi edge probability must be in [0, 0.1] range"
        );
        Self {
            multi_edge_prob,
            ..self
        }
    }

    pub fn model(self, model: RandomModel) -> Self {
        Self { model, ..self }
    }

    pub fn class(self, class: GraphClass) -> Self {
        Self {
            class: Some(class),
            ..self
        }
    }

    pub fn density(self, density: f32) -> Self {
        assert!(
            density > 0.0 && density <= 1.0,
            "density must be in (0, 1] range"
        );
        Self { density, ..self }
    }

    pub fn sparse(self) -> Self {
        self.density(0.05)
    }
}

impl<V: Strategy, E: Strategy, Ty: EdgeType, G> Strategy for GraphStrategy<V, E, Ty, G>
where
    G: Create<V::Value, E::Value> + GraphAdd<V::Value, E::Value>,
{
    type Tree = GraphValueTree<V::Tree, E::Tree, Ty, G>;
    type Value = AsDot<V::Value, E::Value, G>;

    fn new_tree(
        &self,
        runner: &mut proptest::test_runner::TestRunner,
    ) -> proptest::strategy::NewTree<Self> {
        assert!(
            self.params.model == RandomModel::Uniform,
            "generation of graphs in other models is not supported yet"
        );

        // Some structural requirements (e.g., being connected) are difficult or
        // unfeasible to uphold during shrinking. For such cases, we fal back to
        // disable shrinking.
        let no_shrink = false;

        let n = runner.rng().gen_range(0..=self.params.max_size);
        let p = runner.rng().gen::<f32>() * self.params.density;

        // Efficient generation of large random networks
        // http://vlado.fmf.uni-lj.si/pub/networks/doc/ms/rndgen.pdf

        let mut vertices = Vec::with_capacity(n);

        while vertices.len() < n {
            vertices.push(self.vertex.new_tree(runner)?);
        }

        let m_guess = if n > 0 {
            ((n * (n - 1) / 2) as f32 * p).round() as usize
        } else {
            0
        };
        let mut edges = Vec::with_capacity(m_guess);

        let mut v = 1;
        let mut w = usize::MAX; // -1

        while v < n {
            let r: f32 = runner.rng().gen();
            w = w.wrapping_add(1) + ((1.0 - r).log10() / (1.0 - p).log10()).floor() as usize;

            if self.params.allow_loops {
                // Using `w > v` instead of `w >= v` to allow loops.
                while w > v && v < n {
                    w -= v;
                    v += 1;
                }
            } else {
                while w >= v && v < n {
                    w -= v;
                    v += 1;
                }
            }

            if v < n {
                if self.params.class == Some(GraphClass::Bipartite) {
                    // If the vertices belong to the same partition, determined
                    // by odd/even test, do not add the edge.
                    if v % 2 == w % 2 {
                        continue;
                    }
                }

                let e = self.edge.new_tree(runner)?;

                // For directed acyclic graph or in half of the cases, pick the
                // vertices such that v < w. In other cases, swap the vertices
                // so that a directed cycle is possible.
                let (s, t) =
                    if Ty::is_directed() && self.params.acyclic || runner.rng().gen_bool(0.5) {
                        (w, v)
                    } else {
                        (v, w)
                    };

                edges.push((s, t, e));

                // Possibly add multi edges.
                while runner.rng().gen_bool(self.params.multi_edge_prob as f64) {
                    let e = self.edge.new_tree(runner)?;
                    edges.push((s, t, e));
                }
            }
        }

        if self.params.connected {
            panic!("connected graphs are not supported yet");

            // TODO: Identify all connected components and connect these
            // components with new edges.
        }

        if (!Ty::is_directed() && self.params.acyclic)
            || self.params.class == Some(GraphClass::Forest)
        {
            panic!("acyclic undirected graphs or forests are not supported yet");

            // TODO: Run BFS to get a spanning tree and remove all edges that do
            // not belong to the spanning tree.
        }

        Ok(GraphValueTree {
            vertices,
            edges,
            structure: None,
            attr: None,
            ty: PhantomData,
            graph: PhantomData,
            no_shrink,
        })
    }
}

impl<V: ValueTree, E: ValueTree, Ty: EdgeType, G> ValueTree for GraphValueTree<V, E, Ty, G>
where
    G: Create<V::Value, E::Value> + GraphAdd<V::Value, E::Value>,
{
    type Value = AsDot<V::Value, E::Value, G>;

    fn current(&self) -> Self::Value {
        let empty = FxHashSet::default();
        let (removed_vertices, removed_edges) = match self.structure {
            Some(ref state) => (
                &state.current.removed_vertices,
                &state.current.removed_edges,
            ),
            None => (&empty, &empty),
        };

        let mut graph = G::with_capacity(
            self.vertices.len() - removed_vertices.len(),
            self.edges.len() - removed_edges.len(),
        );
        let mut ids = Vec::with_capacity(self.vertices.len());

        for (v, vertex) in self.vertices.iter().enumerate() {
            if !removed_vertices.contains(&v) {
                ids.push(Some(graph.add_vertex(vertex.current())));
            } else {
                ids.push(None);
            }
        }

        for (e, (src, dst, edge)) in self.edges.iter().enumerate() {
            if !removed_edges.contains(&e)
                && !removed_vertices.contains(src)
                && !removed_vertices.contains(dst)
            {
                let src = ids[*src].as_ref().unwrap();
                let dst = ids[*dst].as_ref().unwrap();
                graph.add_edge(src, dst, edge.current());
            }
        }

        AsDot::new(graph)
    }

    fn simplify(&mut self) -> bool {
        if self.no_shrink {
            return false;
        }

        // The strategy is fundamentally the same as for example default
        // strategy for Vec. First, we try to simplify the structure as much as
        // possible, then we switch to simplifying the attributes of vertices
        // and edges.
        //
        // The structure simplification starts with removing all vertices with
        // low degree. The idea is that this should get rid off uninteresting
        // parts of the graph very quickly thanks to its agresivity (many
        // vertices at once). Then we apply the standard strategy of simplifying
        // vertex and edge one by one, respectively. The same strategy is then
        // applied on the attributes.
        //
        // It is useful to start with vertices and then move to edges, because
        // successful removals of vertices may also remove some edges, having
        // larger effect than just removing edges individually.

        let structure = self
            .structure
            .get_or_insert_with(|| ShrinkStructureState::new(&self.vertices, &self.edges));
        let attr = self.attr.get_or_insert_with(ShrinkAttrState::new);

        structure.simplify(&self.vertices, &self.edges)
            || attr.simplify(&mut self.vertices, &mut self.edges, structure)
    }

    fn complicate(&mut self) -> bool {
        // The state is available, because it is initialized in simplify.
        let structure = self.structure.as_mut().unwrap();
        let attr = self.attr.as_mut().unwrap();

        structure.complicate(&self.vertices, &self.edges)
            || attr.complicate(&mut self.vertices, &mut self.edges)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ShrinkStructure {
    VertexWithDegree,
    Vertex(usize),
    Edge(usize),
}

#[derive(Debug, Clone)]
struct Removed {
    removed_vertices: FxHashSet<usize>,
    removed_edges: FxHashSet<usize>,
}

#[derive(Debug, Clone)]
struct ShrinkStructureState {
    current: Removed,
    high: Option<Removed>,
    command: Option<ShrinkStructure>,
    neighbors_out: BTreeMap<(usize, usize), usize>,
    neighbors_in: BTreeMap<(usize, usize), usize>,
}

impl ShrinkStructureState {
    pub fn new<V, E>(_vertices: &[V], edges: &[(usize, usize, E)]) -> Self {
        let mut neighbors_out = BTreeMap::new();
        let mut neighbors_in = BTreeMap::new();

        for &(src, dst, _) in edges {
            *neighbors_out.entry((src, dst)).or_default() += 1;
            *neighbors_in.entry((dst, src)).or_default() += 1;
        }

        Self {
            current: Removed {
                removed_vertices: Default::default(),
                removed_edges: Default::default(),
            },
            high: None,
            command: Some(ShrinkStructure::VertexWithDegree),
            neighbors_out,
            neighbors_in,
        }
    }

    pub fn simplify<V, E>(&mut self, vertices: &[V], edges: &[(usize, usize, E)]) -> bool {
        let command = match self.command {
            Some(command) => command,
            None => return false,
        };

        if self.current.removed_vertices.len() == vertices.len() {
            // Empty graph.
            return false;
        }

        self.high = Some(self.current.clone());

        let (remove_vertices, remove_edges, command) = match command {
            ShrinkStructure::VertexWithDegree => {
                let min_degree = (0..vertices.len())
                    .filter(|&v| self.vertex_exists(v))
                    .map(|v| self.degree(v))
                    .min()
                    .unwrap_or_default();

                let remove = (0..vertices.len())
                    .filter(|&v| self.vertex_exists(v))
                    .filter(|&v| self.degree(v) == min_degree)
                    .collect::<Vec<_>>();

                (remove, Vec::new(), Some(ShrinkStructure::VertexWithDegree))
            }
            ShrinkStructure::Vertex(v) => (vec![v], Vec::new(), self.next_command(vertices, edges)),
            ShrinkStructure::Edge(e) => (Vec::new(), vec![e], self.next_command(vertices, edges)),
        };

        for v in remove_vertices {
            self.current.removed_vertices.insert(v);

            for neighbors in [&mut self.neighbors_out, &mut self.neighbors_in] {
                neighbors.retain(|&(src, dst), _| !(src == v || dst == v));
            }
        }

        for e in remove_edges {
            let (src, dst, _) = edges[e];

            self.current.removed_edges.insert(e);
            self.neighbors_in.remove(&(src, dst));
            self.neighbors_in.remove(&(dst, src));
        }

        self.command = command;
        true
    }

    pub fn complicate<V, E>(&mut self, vertices: &[V], edges: &[(usize, usize, E)]) -> bool {
        self.current = match self.high.take() {
            Some(high) => high,
            None => return false,
        };

        if self.command == Some(ShrinkStructure::VertexWithDegree) {
            self.command = self.next_command(vertices, edges);
        }

        true
    }

    fn degree(&self, v: usize) -> usize {
        self.neighbors_out
            .range((v, 0)..=(v, usize::MAX))
            .chain(self.neighbors_in.range((v, 0)..=(v, usize::MAX)))
            .map(|(_, d)| d)
            .sum()
    }

    fn vertex_exists(&self, v: usize) -> bool {
        !self.current.removed_vertices.contains(&v)
    }

    fn edge_exists(&self, e: usize, (src, dst): (usize, usize)) -> bool {
        !(self.current.removed_vertices.contains(&src)
            || self.current.removed_vertices.contains(&dst)
            || self.current.removed_edges.contains(&e))
    }

    fn next_command<V, E>(
        &mut self,
        vertices: &[V],
        edges: &[(usize, usize, E)],
    ) -> Option<ShrinkStructure> {
        match self.command? {
            ShrinkStructure::VertexWithDegree => (0..vertices.len())
                .find(|&v| self.vertex_exists(v))
                .map(ShrinkStructure::Vertex),
            ShrinkStructure::Vertex(v) => ((v + 1)..vertices.len())
                .find(|&w| self.vertex_exists(w))
                .map(ShrinkStructure::Vertex)
                .or_else(|| {
                    (0..edges.len())
                        .find(|&e| {
                            let (src, dst, _) = edges[e];
                            self.edge_exists(e, (src, dst))
                        })
                        .map(ShrinkStructure::Edge)
                }),
            ShrinkStructure::Edge(e) => ((e + 1)..edges.len())
                .find(|&f| {
                    let (src, dst, _) = edges[f];
                    self.edge_exists(f, (src, dst))
                })
                .map(ShrinkStructure::Edge),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ShrinkAttr {
    Vertex(usize),
    Edge(usize),
}

#[derive(Debug, Clone)]
struct ShrinkAttrState {
    command: ShrinkAttr,
    previous: Option<ShrinkAttr>,
}

// The implementation is adapted from VecValueTree
// (https://github.com/proptest-rs/proptest/blob/ef305c4fadd7c0ba13a349f542da00d290116ccb/proptest/src/collection.rs#L603-L672).
impl ShrinkAttrState {
    pub fn new() -> Self {
        Self {
            command: ShrinkAttr::Vertex(0),
            previous: None,
        }
    }

    pub fn simplify<V, E>(
        &mut self,
        vertices: &mut [V],
        edges: &mut [(usize, usize, E)],
        structure: &ShrinkStructureState,
    ) -> bool
    where
        V: ValueTree,
        E: ValueTree,
    {
        loop {
            match self.command {
                ShrinkAttr::Vertex(v) => {
                    if v >= vertices.len() {
                        self.command = ShrinkAttr::Edge(0);
                    } else if structure.vertex_exists(v) && vertices[v].simplify() {
                        self.previous = Some(self.command);
                        return true;
                    } else {
                        self.command = ShrinkAttr::Vertex(v + 1);
                    }
                }
                ShrinkAttr::Edge(e) => {
                    if e >= edges.len() {
                        return false;
                    } else {
                        let (src, dst, edge) = &mut edges[e];
                        if structure.edge_exists(e, (*src, *dst)) && edge.simplify() {
                            self.previous = Some(self.command);
                            return true;
                        } else {
                            self.command = ShrinkAttr::Edge(e + 1);
                        }
                    }
                }
            }
        }
    }

    pub fn complicate<V, E>(&mut self, vertices: &mut [V], edges: &mut [(usize, usize, E)]) -> bool
    where
        V: ValueTree,
        E: ValueTree,
    {
        match self.previous {
            None => false,
            Some(ShrinkAttr::Vertex(v)) => {
                if vertices[v].complicate() {
                    true
                } else {
                    self.previous = None;
                    false
                }
            }
            Some(ShrinkAttr::Edge(e)) => {
                if edges[e].2.complicate() {
                    true
                } else {
                    self.previous = None;
                    false
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use proptest::{strategy::check_strategy_sanity, test_runner::TestRunner};

    use crate::core::{
        base::{EdgeRef, VertexRef},
        EdgeSet, GraphRef, VertexSet,
    };

    use super::*;

    #[test]
    #[ignore = "takes too long, run it only when the strategy is changed"]
    fn graph_strategy_sanity() {
        check_strategy_sanity(graph_undirected(0..100, 0..100).max_size(16), None);
    }

    #[test]
    fn simplifies_structure_and_data() {
        let strategy = graph_undirected(0..100, 0..100).max_size(64);
        let mut tree = strategy.new_tree(&mut TestRunner::deterministic()).unwrap();

        loop {
            let graph = tree.current();

            if graph.vertex_count() < 1 || graph.edge_count() < 1 {
                if !tree.complicate() {
                    break;
                }
            } else if !tree.simplify() {
                break;
            }
        }

        let graph = tree.current();

        // No loops and multi edges => two vertices connected with one edge.
        assert_eq!(graph.vertex_count(), 2);
        assert_eq!(graph.edge_count(), 1);

        // Attributes simplified too.
        for vertex in graph.vertices() {
            assert_eq!(*vertex.attr(), 0);
        }

        for edge in graph.edges() {
            assert_eq!(*edge.attr(), 0);
        }
    }
}
