use gryf::{
    core::id::{CustomId, EdgeId, VertexId},
    domain::Graph,
    storage::AdjList,
};

fn main() {
    let mut graph =
        Graph::new_directed_in(AdjList::with_id::<CustomId<VertexId<u8>, EdgeId<u16>>>());

    let hello = graph.add_vertex("hello");
    let world = graph.add_vertex("world");

    graph.add_edge(hello, world, ());
}
