use std::collections::HashMap;
use std::fmt::Display;
use std::io::{self, Write};
use std::marker::PhantomData;

use crate::index::IndexType;
use crate::marker::EdgeType;
use crate::traits::*;

pub trait Export<G> {
    fn export<W: Write>(&self, graph: &G, out: &mut W) -> io::Result<()>;
}

pub struct Dot<V, E, Ty: EdgeType> {
    name: String,
    get_vertex_label: Box<dyn Fn(&V) -> String>,
    get_edge_label: Box<dyn Fn(&E) -> String>,
    ty: PhantomData<Ty>,
}

impl<V, E, Ty: EdgeType> Dot<V, E, Ty> {
    pub fn new<FV, FE>(name: Option<String>, get_vertex_label: FV, get_edge_label: FE) -> Self
    where
        FV: Fn(&V) -> String + 'static,
        FE: Fn(&E) -> String + 'static,
    {
        Self {
            name: name.unwrap_or_else(|| String::from("G")),
            get_vertex_label: Box::new(get_vertex_label),
            get_edge_label: Box::new(get_edge_label),
            ty: PhantomData,
        }
    }
}

impl<V: Display, E: Display, Ty: EdgeType> Dot<V, E, Ty> {
    pub fn with_display(name: Option<String>) -> Self {
        Self::new(name, |v| format!("{}", v), |e| format!("{}", e))
    }
}

impl<V, E, Ty: EdgeType, G> Export<G> for Dot<V, E, Ty>
where
    G: Vertices<V> + Edges<E, Ty>,
{
    fn export<W: Write>(&self, graph: &G, out: &mut W) -> io::Result<()> {
        if Ty::is_directed() {
            out.write_all(b"digraph ")?;
        } else {
            out.write_all(b"graph ")?;
        }

        let mut indexer = Indexer::new();

        out.write_all(self.name.as_bytes())?;
        out.write_all(b" {\n")?;

        for vertex in graph.vertices() {
            out.write_all(
                format!(
                    "    v{} [label={:?}];\n",
                    indexer.get(vertex.index()),
                    (self.get_vertex_label)(vertex.data())
                )
                .as_bytes(),
            )?;
        }

        for edge in graph.edges() {
            let line = if Ty::is_directed() { "->" } else { "--" };
            out.write_all(
                format!(
                    "    v{} {} v{} [label={:?}];\n",
                    indexer.get(edge.src()),
                    line,
                    indexer.get(edge.dst()),
                    (self.get_edge_label)(edge.data())
                )
                .as_bytes(),
            )?;
        }

        out.write_all(b"}\n")?;

        Ok(())
    }
}

#[derive(Debug)]
struct Indexer<Idx>(HashMap<Idx, usize>);

impl<Idx: IndexType> Indexer<Idx> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&mut self, idx: &Idx) -> usize {
        let new_idx = self.0.len();
        *self.0.entry(idx.clone()).or_insert(new_idx)
    }
}
