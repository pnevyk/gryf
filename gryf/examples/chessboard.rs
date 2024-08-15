use std::fmt;

use gryf::{
    algo::ShortestPaths,
    core::{
        base::NeighborRef,
        id::{IdType, IntegerIdType},
        marker::{Direction, Undirected},
        GraphBase, Neighbors,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChessSquare {
    file: u8,
    rank: u8,
}

pub enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

impl ChessSquare {
    fn new(file: File, rank: u8) -> Self {
        assert!((1..=8).contains(&rank));
        let file = file as u8;
        let rank = rank - 1;
        Self { file, rank }
    }
}

impl IdType for ChessSquare {
    fn sentinel() -> Self {
        Self {
            file: u8::MAX,
            rank: u8::MAX,
        }
    }

    fn is_integer() -> bool {
        true
    }

    fn as_bits(&self) -> u64 {
        (self.rank * 8 + self.file) as u64
    }

    fn from_bits(bits: u64) -> Self {
        let file = (bits % 8) as u8;
        let rank = (bits / 8) as u8;
        Self { file, rank }
    }
}

impl IntegerIdType for ChessSquare {}

impl From<usize> for ChessSquare {
    fn from(value: usize) -> Self {
        ChessSquare::from_usize(value)
    }
}

impl From<ChessSquare> for usize {
    fn from(value: ChessSquare) -> Self {
        value.as_usize()
    }
}

impl fmt::Display for ChessSquare {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file = (b'a' + self.file) as char;
        let rank = self.rank + 1;
        f.write_fmt(format_args!("{file}{rank}"))
    }
}

pub struct KingMove;

impl GraphBase for KingMove {
    type VertexId = ChessSquare;
    type EdgeId = (ChessSquare, ChessSquare);
    type EdgeType = Undirected;

    fn vertex_count_hint(&self) -> Option<usize> {
        Some(8 * 8)
    }
}

impl Neighbors for KingMove {
    type NeighborRef<'a> = NeighborRef<ChessSquare, (ChessSquare, ChessSquare)>
    where
        Self: 'a;

    type NeighborsIter<'a> = ChessNeighborsIter
    where
        Self: 'a;

    fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_> {
        ChessNeighborsIter {
            from: *from,
            index: 0,
            dir: Direction::Outgoing,
        }
    }

    fn neighbors_directed(&self, from: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
        ChessNeighborsIter {
            from: *from,
            index: 0,
            dir,
        }
    }
}

pub struct ChessNeighborsIter {
    from: ChessSquare,
    index: usize,
    dir: Direction,
}

impl Iterator for ChessNeighborsIter {
    type Item = NeighborRef<ChessSquare, (ChessSquare, ChessSquare)>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // +---+---+---+
            // | 0 | 1 | 2 |
            // +---+---+---+
            // | 3 | S | 4 |
            // +---+---+---+
            // | 5 | 6 | 7 |
            // +---+---+---+
            let ChessSquare { file: x, rank: y } = self.from;
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

            let to = ChessSquare { file: x, rank: y };
            return Some(NeighborRef {
                id: to,
                edge: (self.from, to),
                pred: self.from,
                dir: self.dir,
            });
        }
    }
}

fn main() {
    let start = ChessSquare::new(File::E, 1);
    let end = ChessSquare::new(File::D, 4);

    let path = ShortestPaths::on(&KingMove)
        .goal(end)
        .unit_weight()
        .bfs()
        .run(start)
        .unwrap();

    println!(
        "{} moves by king to get from {} to {}",
        path[end], start, end
    );
}
