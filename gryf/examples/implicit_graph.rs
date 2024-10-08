use std::iter;

use gryf::{
    core::{
        base::NeighborReference,
        borrow::OwnableRef,
        id::{EdgeId, IdType, VertexId},
        marker::{Directed, Direction},
        GraphBase, Neighbors,
    },
    visit::{dfs::Dfs, Visitor},
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
    pred: VertexId,
}

impl NeighborReference<VertexId, EdgeId> for Neighbor {
    fn id(&self) -> OwnableRef<'_, VertexId> {
        let n = self.pred.as_bits();
        let c = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
        OwnableRef::Owned(to_vertex(c))
    }

    fn edge(&self) -> OwnableRef<'_, EdgeId> {
        OwnableRef::Owned(EdgeId::from_bits(self.pred.as_bits()))
    }

    fn pred(&self) -> OwnableRef<'_, VertexId> {
        self.pred.into()
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

    fn neighbors_undirected(&self, from: &VertexId) -> Self::NeighborsIter<'_> {
        iter::once(Neighbor { pred: *from })
    }

    fn neighbors_directed(&self, from: &VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        assert_eq!(dir, Direction::Outgoing, "incoming edges are not available");
        iter::once(Neighbor { pred: *from })
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
