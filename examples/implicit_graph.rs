#![feature(generic_associated_types)]

use std::iter;

use gryf::prelude::*;
use gryf::{marker::Direction, visit::Dfs, IndexType};

// https://stackoverflow.com/questions/58870416/can-you-explain-implicit-graphsin-graph-theory-with-a-simple-example/58887179#58887179
struct Collatz;

impl Collatz {
    pub fn new() -> Self {
        Self
    }

    pub fn vertex_index(&self, n: u64) -> VertexIndex {
        VertexIndex::from_bits(n)
    }
}

struct Neighbor {
    src: VertexIndex,
}

impl NeighborRef for Neighbor {
    fn index(&self) -> VertexIndex {
        let n = self.src.to_bits();
        let c = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
        VertexIndex::from_bits(c)
    }

    fn edge(&self) -> EdgeIndex {
        self.src.to_usize().into()
    }

    fn src(&self) -> VertexIndex {
        self.src
    }

    fn dir(&self) -> Direction {
        Outgoing
    }
}

impl Neighbors for Collatz {
    type NeighborRef<'a> = Neighbor;

    type NeighborsIter<'a>
    where
        Self: 'a,
    = iter::Once<Self::NeighborRef<'a>>;

    fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
        iter::once(Neighbor { src })
    }

    fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
        assert_eq!(dir, Direction::Outgoing, "incoming edges are not available");
        iter::once(Neighbor { src })
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
    fn vertex_weak(&self, index: VertexIndex) -> Option<WeakRef<'_, u64>> {
        Some(WeakRef::owned(index.to_bits()))
    }
}

fn main() {
    let collatz = Collatz::new();

    let sequence = Dfs::new(&collatz)
        .start(collatz.vertex_index(9))
        .iter(&collatz)
        .map(|v| *collatz.vertex_weak(v).unwrap())
        .collect::<Vec<_>>();
    println!("A Collatz sequence: {:?}", sequence);
}
