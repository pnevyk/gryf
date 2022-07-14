#![feature(generic_associated_types)]

use gryf::algo::{shortest_paths::unit, ShortestPaths};
use gryf::prelude::*;
use gryf::{marker::Direction, IndexType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ChessSquare(pub usize, pub usize);

impl IndexType for ChessSquare {}

struct Chessboard;

impl GraphBase for Chessboard {
    type VertexIndex = ChessSquare;
    type EdgeIndex = ();
}

impl VerticesBaseWeak for Chessboard {
    fn vertex_count_hint(&self) -> Option<usize> {
        Some(8 * 8)
    }

    fn vertex_bound_hint(&self) -> Option<usize> {
        Some(8 * 8)
    }
}

impl EdgesBaseWeak<Undirected> for Chessboard {
    fn edge_count_hint(&self) -> Option<usize> {
        None
    }

    fn edge_bound_hint(&self) -> Option<usize> {
        None
    }

    fn endpoints_weak(
        &self,
        _index: &Self::EdgeIndex,
    ) -> Option<(Self::VertexIndex, Self::VertexIndex)> {
        None
    }

    fn edge_index_weak(
        &self,
        _src: &Self::VertexIndex,
        _dst: &Self::VertexIndex,
    ) -> Option<Self::EdgeIndex> {
        None
    }
}

impl EdgesWeak<(), Undirected> for Chessboard {
    fn edge_weak(&self, _index: &Self::EdgeIndex) -> Option<WeakRef<'_, ()>> {
        Some(WeakRef::Owned(()))
    }
}

impl Neighbors for Chessboard {
    type NeighborRef<'a> = (ChessSquare, (), ChessSquare, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = ChessNeighborsIter
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexIndex) -> Self::NeighborsIter<'_> {
        ChessNeighborsIter {
            src: *src,
            index: 0,
            dir: Outgoing,
        }
    }

    fn neighbors_directed(
        &self,
        src: &Self::VertexIndex,
        dir: Direction,
    ) -> Self::NeighborsIter<'_> {
        ChessNeighborsIter {
            src: *src,
            index: 0,
            dir,
        }
    }
}

struct ChessNeighborsIter {
    src: ChessSquare,
    index: usize,
    dir: Direction,
}

impl Iterator for ChessNeighborsIter {
    type Item = (ChessSquare, (), ChessSquare, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // +---+---+---+
            // | 0 | 1 | 2 |
            // +---+---+---+
            // | 3 | S | 4 |
            // +---+---+---+
            // | 5 | 6 | 7 |
            // +---+---+---+
            let ChessSquare(x, y) = self.src;
            let (x, y) = match self.index {
                0 => (x.wrapping_sub(1), y + 1),
                1 => (x, y + 1),
                2 => (x + 1, y + 1),
                3 => (x.wrapping_sub(1), y),
                4 => (x + 1, y),
                5 => (x.wrapping_sub(1), y.wrapping_sub(1)),
                6 => (x, y.wrapping_sub(1)),
                7 => (x + 1, y.wrapping_sub(1)),
                _ => return None,
            };

            self.index += 1;

            if x >= 8 || y >= 8 {
                // Position outside of board.
                continue;
            }

            return Some((ChessSquare(x, y), (), self.src, self.dir));
        }
    }
}

fn main() {
    let start = ChessSquare(0, 0);
    let end = ChessSquare(4, 2);

    let path = ShortestPaths::run_dijkstra(&Chessboard, start, Some(end), unit).unwrap();

    println!(
        "{} moves by king to get from {:?} to {:?}",
        path.dist(&end).unwrap(),
        start,
        end
    );
}
