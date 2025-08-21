mod common;

use common::{RANDOM_SEED, gryf_random_directed, petgraph_random_directed};
use fastrand::Rng;
use gryf::visit::Visitor;

fn main() {
    divan::main();
}

#[divan::bench(consts = [100, 1000], args = [0.05, 0.5, 0.95])]
fn gryf_dfs_random<const N: usize>(bencher: divan::Bencher, density: f32) {
    let graph = gryf_random_directed(N, density, &mut Rng::with_seed(RANDOM_SEED));

    bencher.bench(|| {
        gryf::algo::TopoSort::on(&graph)
            .dfs()
            .run()
            .iter(&graph)
            .collect::<Vec<_>>()
    });
}

#[divan::bench(consts = [100, 1000], args = [0.05, 0.5, 0.95])]
fn petgraph_dfs_random<const N: usize>(bencher: divan::Bencher, density: f32) {
    let graph = petgraph_random_directed(N, density, &mut Rng::with_seed(RANDOM_SEED));

    bencher.bench(|| petgraph::algo::toposort(&graph, None));
}

#[divan::bench(consts = [100, 1000], args = [0.05, 0.5, 0.95])]
fn gryf_kahn_random<const N: usize>(bencher: divan::Bencher, density: f32) {
    let graph = gryf_random_directed(N, density, &mut Rng::with_seed(RANDOM_SEED));

    bencher.bench(|| {
        gryf::algo::TopoSort::on(&graph)
            .kahn()
            .run()
            .collect::<Vec<_>>()
    });
}
