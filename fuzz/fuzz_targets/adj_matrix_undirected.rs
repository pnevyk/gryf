#![no_main]

use libfuzzer_sys::fuzz_target;

use gryf::{
    core::{id::DefaultId, marker::Undirected},
    infra::{arbitrary::MutOpsSeq, testing::check_consistency},
    storage::AdjMatrix,
};

fuzz_target!(|ops: MutOpsSeq<i8, i8>| {
    let mut graph = AdjMatrix::<_, _, Undirected, DefaultId>::new();

    for op in ops {
        op.apply(&mut graph);
        check_consistency(&graph)
            .as_ref()
            .map_err(ToString::to_string)
            .unwrap();
    }
});
