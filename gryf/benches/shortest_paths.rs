mod common;

use common::{RANDOM_SEED, gryf_random_directed, petgraph_random_directed};
use fastrand::Rng;
use gryf::core::id::*;
use petgraph::prelude::*;

fn main() {
    divan::main();
}

#[divan::bench(consts = [100, 1000, 10000], args = [0.25, 0.75])]
fn gryf_dikstra_random<const N: usize>(bencher: divan::Bencher, density: f32) {
    let graph = gryf_random_directed(N, density, &mut Rng::with_seed(RANDOM_SEED));
    let start = VertexId::from_usize(0);

    bencher.bench(|| gryf::algo::ShortestPaths::on(&graph).dijkstra().run(start));
}

#[divan::bench(consts = [100, 1000, 10000], args = [0.25, 0.75])]
fn petgraph_dijkstra_random<const N: usize>(bencher: divan::Bencher, density: f32) {
    let graph = petgraph_random_directed(N, density, &mut Rng::with_seed(RANDOM_SEED));
    let start = NodeIndex::new(0);

    bencher.bench(|| petgraph::algo::dijkstra(&graph, start, None, |e| *e.weight()));
}

#[divan::bench(consts = [100, 1000, 10000], args = [0.25, 0.75])]
fn gryf_bellman_ford_random<const N: usize>(bencher: divan::Bencher, density: f32) {
    let graph = gryf_random_directed(N, density, &mut Rng::with_seed(RANDOM_SEED));
    let start = VertexId::from_usize(0);

    bencher.bench(|| {
        gryf::algo::ShortestPaths::on(&graph)
            .bellman_ford()
            .run(start)
    });
}

#[divan::bench(consts = [100, 1000, 10000], args = [0.25, 0.75])]
fn petgraph_bellman_ford_random<const N: usize>(bencher: divan::Bencher, density: f32) {
    let graph = petgraph_random_directed(N, density, &mut Rng::with_seed(RANDOM_SEED));
    let start = NodeIndex::new(0);

    bencher.bench(|| petgraph::algo::bellman_ford(&graph, start));
}
