#![allow(dead_code)]

use fastrand::Rng;
use petgraph::prelude::*;

pub const RANDOM_SEED: u64 = 0xef6f79ed30ba75a;

pub struct RandomEdges {
    v: usize,
    w: usize,
    n: usize,
    p: f32,
}

impl RandomEdges {
    pub fn new(vertex_bound: usize, p: f32) -> Self {
        Self::with_vertices(0, vertex_bound, p)
    }

    pub fn with_vertices(vertex_count: usize, vertex_bound: usize, p: f32) -> Self {
        Self {
            v: vertex_count + 1,
            w: usize::MAX,
            n: vertex_bound,
            p,
        }
    }

    pub fn next_edge(&mut self, rng: &mut Rng) -> Option<(usize, usize)> {
        // Based on the implementation in gryf::infra::proptest.

        let Self { v, w, .. } = self;
        let n = self.n;
        let p = self.p;

        if *v >= n {
            return None;
        }

        let r = rng.f32();
        *w = w.wrapping_add(1) + ((1.0 - r).log10() / (1.0 - p).log10()).floor() as usize;

        while *w >= *v && *v < n {
            *w -= *v;
            *v += 1;
        }

        if *v < n { Some((*v, *w)) } else { None }
    }
}

pub fn gryf_random<Ty: gryf::core::marker::EdgeType>(
    vertex_count: usize,
    density: f32,
    rng: &mut Rng,
) -> gryf::domain::Graph<u32, f32, Ty> {
    let mut graph = gryf::domain::Graph::new();

    for _ in 0..vertex_count {
        graph.add_vertex(rng.u32(0..100));
    }

    let mut edges = RandomEdges::new(vertex_count, density);

    while let Some((u, v)) = edges.next_edge(rng) {
        graph.add_edge(u, v, rng.f32());
    }

    graph
}

pub fn gryf_random_directed(
    vertex_count: usize,
    density: f32,
    rng: &mut Rng,
) -> gryf::domain::Graph<u32, f32, gryf::core::marker::Directed> {
    gryf_random(vertex_count, density, rng)
}

pub fn petgraph_random<Ty: petgraph::EdgeType>(
    vertex_count: usize,
    density: f32,
    rng: &mut Rng,
) -> petgraph::Graph<u32, f32, Ty> {
    let mut graph = petgraph::Graph::with_capacity(vertex_count, 0);

    for _ in 0..vertex_count {
        graph.add_node(rng.u32(0..100));
    }

    let mut edges = RandomEdges::new(vertex_count, density);

    while let Some((u, v)) = edges.next_edge(rng) {
        graph.add_edge(NodeIndex::new(u), NodeIndex::new(v), rng.f32());
    }

    graph
}

pub fn petgraph_random_directed(
    vertex_count: usize,
    density: f32,
    rng: &mut Rng,
) -> petgraph::Graph<u32, f32, petgraph::Directed> {
    petgraph_random(vertex_count, density, rng)
}
