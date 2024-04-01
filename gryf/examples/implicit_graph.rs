use std::iter;

use gryf::prelude::*;
use gryf::{
    core::{
        index::{EdgeIndex, VertexIndex},
        marker::{Directed, Direction},
        GraphBase, Neighbors, VerticesBaseWeak, VerticesWeak, WeakRef,
    },
    visit::Dfs,
};

// https://stackoverflow.com/questions/58870416/can-you-explain-implicit-graphsin-graph-theory-with-a-simple-example/58887179#58887179
struct Collatz;

fn to_vertex(n: u64) -> VertexIndex {
    VertexIndex::from_bits(n)
}

fn to_num(v: VertexIndex) -> u64 {
    v.to_bits()
}

struct Neighbor {
    src: VertexIndex,
}

impl NeighborRef<VertexIndex, EdgeIndex> for Neighbor {
    fn index(&self) -> WeakRef<'_, VertexIndex> {
        let n = self.src.to_bits();
        let c = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
        WeakRef::Owned(to_vertex(c))
    }

    fn edge(&self) -> WeakRef<'_, EdgeIndex> {
        WeakRef::Owned(EdgeIndex::from_bits(self.src.to_bits()))
    }

    fn src(&self) -> WeakRef<'_, VertexIndex> {
        self.src.into()
    }

    fn dir(&self) -> Direction {
        Direction::Outgoing
    }
}

impl GraphBase for Collatz {
    type VertexIndex = VertexIndex;
    type EdgeIndex = EdgeIndex;
    type EdgeType = Directed;
}

impl Neighbors for Collatz {
    type NeighborRef<'a> = Neighbor;

    type NeighborsIter<'a> = iter::Once<Self::NeighborRef<'a>>
    where
        Self: 'a;

    fn neighbors(&self, src: &VertexIndex) -> Self::NeighborsIter<'_> {
        iter::once(Neighbor { src: *src })
    }

    fn neighbors_directed(&self, src: &VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        assert_eq!(dir, Direction::Outgoing, "incoming edges are not available");
        iter::once(Neighbor { src: *src })
    }
}

impl VerticesBaseWeak for Collatz {
    fn vertex_count_hint(&self) -> Option<usize> {
        None
    }

    fn vertex_bound_hint(&self) -> Option<usize> {
        None
    }
}

impl VerticesWeak<u64> for Collatz {
    fn vertex_weak(&self, index: &VertexIndex) -> Option<WeakRef<'_, u64>> {
        Some(index.to_bits().into())
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
