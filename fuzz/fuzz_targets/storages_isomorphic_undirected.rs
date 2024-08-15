#![no_main]

use libfuzzer_sys::fuzz_target;

use gryf::{
    core::{error::AddEdgeErrorKind, id::DefaultId, marker::Undirected},
    infra::{
        arbitrary::{MutOpResult, MutOpsSeq},
        testing::check_potential_isomorphism,
    },
    storage::{AdjList, AdjMatrix, EdgeList},
};

fuzz_target!(|ops: MutOpsSeq<i8, i8>| {
    let mut adj_list = AdjList::<_, _, Undirected, DefaultId>::new();
    let mut adj_list_multi = AdjList::<_, _, Undirected, DefaultId>::new();

    let mut adj_matrix = AdjMatrix::<_, _, Undirected, DefaultId>::new();

    let mut edge_list = EdgeList::<_, _, Undirected, DefaultId>::new();
    let mut edge_list_multi = EdgeList::<_, _, Undirected, DefaultId>::new();

    for op in ops {
        let op_result = op.clone().apply(&mut adj_matrix);

        // If we want to compare AdjMatrix with others, we can't add multi edges
        // to the other storages.
        let is_multi_edge = matches!(op_result, MutOpResult::AddEdge(Err(error)) if error.kind == AddEdgeErrorKind::MultiEdge);

        if !is_multi_edge {
            op.clone().apply(&mut adj_list);
            op.clone().apply(&mut edge_list);
        }

        op.clone().apply(&mut adj_list_multi);
        op.apply(&mut edge_list_multi);

        let results = [
            (
                check_potential_isomorphism(&adj_list, &adj_matrix),
                "AdjList <-> AdjMatrix",
            ),
            (
                check_potential_isomorphism(&adj_list, &edge_list),
                "AdjList <-> EdgeList",
            ),
            (
                check_potential_isomorphism(&adj_matrix, &edge_list),
                "AdjMatrix <-> EdgeList",
            ),
            (
                check_potential_isomorphism(&adj_list_multi, &edge_list_multi),
                "AdjList(multi) <-> EdgeList(multi)",
            ),
        ];

        if results.iter().any(|(result, _)| result.is_err()) {
            let mut report = "storages are not isomorphic:".to_string();

            for (result, label) in results {
                let result = match result {
                    Ok(_) => "OK".to_string(),
                    Err(error) => error.to_string(),
                };
                report.push_str(format!("\n  {label}: {result}").as_str());
            }

            panic!("{report}");
        }
    }
});
