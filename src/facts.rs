use crate::marker::EdgeType;

pub fn complete_graph_edge_count<Ty: EdgeType>(vertex_count: usize) -> usize {
    if Ty::is_directed() {
        vertex_count * (vertex_count - 1)
    } else {
        vertex_count * (vertex_count - 1) / 2
    }
}
