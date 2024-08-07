use std::iter;

use gryf::prelude::*;
use gryf::{
    core::{
        id::{EdgeId, IdType, VertexId},
        marker::{Directed, Direction},
        weak::WeakRef,
        GraphBase, Neighbors,
    },
    visit::Dfs,
};

// https://stackoverflow.com/questions/58870416/can-you-explain-implicit-graphsin-graph-theory-with-a-simple-example/58887179#58887179
struct Collatz;

fn to_vertex(n: u64) -> VertexId {
    VertexId::from_bits(n)
}

fn to_num(v: VertexId) -> u64 {
    v.as_bits()
}

struct Neighbor {
    src: VertexId,
}

impl NeighborRef<VertexId, EdgeId> for Neighbor {
    fn id(&self) -> WeakRef<'_, VertexId> {
        let n = self.src.as_bits();
        let c = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
        WeakRef::Owned(to_vertex(c))
    }

    fn edge(&self) -> WeakRef<'_, EdgeId> {
        WeakRef::Owned(EdgeId::from_bits(self.src.as_bits()))
    }

    fn src(&self) -> WeakRef<'_, VertexId> {
        self.src.into()
    }

    fn dir(&self) -> Direction {
        Direction::Outgoing
    }
}

impl GraphBase for Collatz {
    type VertexId = VertexId;
    type EdgeId = EdgeId;
    type EdgeType = Directed;
}

impl Neighbors for Collatz {
    type NeighborRef<'a> = Neighbor;

    type NeighborsIter<'a> = iter::Once<Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &VertexId) -> Self::NeighborsIter<'_> {
        iter::once(Neighbor { src: *src })
    }

    fn neighbors_directed(&self, src: &VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        assert_eq!(dir, Direction::Outgoing, "incoming edges are not available");
        iter::once(Neighbor { src: *src })
    }
}

fn main() {
    let collatz = Collatz;

    let sequence = Dfs::new(&collatz)
        .start(to_vertex(9))
        .iter(&collatz)
        .map(to_num)
        .collect::<Vec<_>>();
    println!("A Collatz sequence: {sequence:?}");
}
