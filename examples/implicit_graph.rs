#![feature(generic_associated_types)]

use std::iter;

use gryf::{marker::Direction, prelude::*, IndexType};

// https://stackoverflow.com/questions/58870416/can-you-explain-implicit-graphsin-graph-theory-with-a-simple-example/58887179#58887179
struct Collatz;

impl Collatz {
    pub fn new() -> Self {
        Self
    }

    pub fn vertex_index(&self, n: u64) -> VertexIndex {
        VertexIndex::from_bits(n)
    }

    pub fn vertex_value(&self, index: VertexIndex) -> u64 {
        index.to_bits()
    }
}

struct Neighbor {
    src: VertexIndex,
}

impl NeighborRef for Neighbor {
    fn index(&self) -> VertexIndex {
        let n = self.src.to_bits();
        // https://stackoverflow.com/questions/58870416/can-you-explain-implicit-graphsin-graph-theory-with-a-simple-example/58887179#58887179
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
        Direction::Outgoing
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

fn main() {
    let collatz = Collatz::new();

    let mut n = collatz.vertex_index(9);

    // TODO: Use DFS visitor once it is designed and implemented.

    print!("A Collatz sequence: {}", collatz.vertex_value(n));
    while collatz.vertex_value(n) != 1 {
        n = collatz.neighbors(n).next().unwrap().index();
        print!(" -> {}", collatz.vertex_value(n));
    }

    println!();
}
