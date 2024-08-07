use gryf::{
    algo::ShortestPaths,
    core::{
        id::IdType,
        marker::{Direction, Undirected},
        GraphBase, Neighbors,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ChessSquare(pub usize, pub usize);

impl IdType for ChessSquare {
    fn sentinel() -> Self {
        Self(usize::MAX, usize::MAX)
    }

    fn is_integer() -> bool {
        false
    }

    fn as_bits(&self) -> u64 {
        panic!("unsupported")
    }

    fn from_bits(_: u64) -> Self {
        panic!("unsupported")
    }
}

struct Chessboard;

impl GraphBase for Chessboard {
    type VertexId = ChessSquare;
    type EdgeId = (ChessSquare, ChessSquare);
    type EdgeType = Undirected;

    fn vertex_count_hint(&self) -> Option<usize> {
        Some(8 * 8)
    }
}

impl Neighbors for Chessboard {
    type NeighborRef<'a> = (ChessSquare, (ChessSquare, ChessSquare), ChessSquare, Direction)
    where
        Self: 'a;

    type NeighborsIter<'a> = ChessNeighborsIter
    where
        Self: 'a;

    fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_> {
        ChessNeighborsIter {
            src: *src,
            index: 0,
            dir: Direction::Outgoing,
        }
    }

    fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
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
    type Item = (
        ChessSquare,
        (ChessSquare, ChessSquare),
        ChessSquare,
        Direction,
    );

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

            let dst = ChessSquare(x, y);
            return Some((dst, (self.src, dst), self.src, self.dir));
        }
    }
}

fn main() {
    let start = ChessSquare(0, 0);
    let end = ChessSquare(4, 2);

    let path = ShortestPaths::on(&Chessboard)
        .goal(end)
        .unit_weight()
        .bfs()
        .run(start)
        .unwrap();

    println!(
        "{} moves by king to get from {:?} to {:?}",
        path[end], start, end
    );
}
