use std::{
    collections::HashMap,
    fmt::Display,
    io::{self, Cursor, Write},
};

use crate::core::{
    GraphRef,
    base::{EdgeReference, VertexReference},
    id::IdType,
};

pub trait Export<G> {
    fn export<W: Write>(&self, graph: &G, out: &mut W) -> io::Result<()>;
}

pub struct Dot<V, E> {
    name: String,
    get_vertex_label: Box<dyn Fn(&V) -> String>,
    get_edge_label: Box<dyn Fn(&E) -> String>,
}

impl<V, E> Dot<V, E> {
    pub fn new<FV, FE>(name: Option<String>, get_vertex_label: FV, get_edge_label: FE) -> Self
    where
        FV: Fn(&V) -> String + 'static,
        FE: Fn(&E) -> String + 'static,
    {
        Self {
            name: name.unwrap_or_else(|| String::from("G")),
            get_vertex_label: Box::new(get_vertex_label),
            get_edge_label: Box::new(get_edge_label),
        }
    }

    pub fn to_string<G>(&self, graph: &G) -> String
    where
        G: GraphRef<V, E>,
    {
        let mut cursor = Cursor::new(Vec::new());
        self.export(graph, &mut cursor)
            .expect("writing to vec in cursor does not fail");

        String::from_utf8(cursor.into_inner()).expect("dot format is text format")
    }
}

impl<V: Display, E: Display> Dot<V, E> {
    pub fn with_display(name: Option<String>) -> Self {
        Self::new(name, |v| format!("{v}"), |e| format!("{e}"))
    }
}

impl<V, E, G> Export<G> for Dot<V, E>
where
    G: GraphRef<V, E>,
{
    fn export<W: Write>(&self, graph: &G, out: &mut W) -> io::Result<()> {
        if graph.is_directed() {
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
                    indexer.get(vertex.id()),
                    (self.get_vertex_label)(vertex.attr())
                )
                .as_bytes(),
            )?;
        }

        for edge in graph.edges() {
            let line = if graph.is_directed() { "->" } else { "--" };
            out.write_all(
                format!(
                    "    v{} {} v{} [label={:?}];\n",
                    indexer.get(edge.from()),
                    line,
                    indexer.get(edge.to()),
                    (self.get_edge_label)(edge.attr())
                )
                .as_bytes(),
            )?;
        }

        out.write_all(b"}\n")?;

        Ok(())
    }
}

#[derive(Debug)]
struct Indexer<Id>(HashMap<Id, usize>);

impl<I: IdType> Indexer<I> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&mut self, idx: &I) -> usize {
        let new_idx = self.0.len();
        *self.0.entry(idx.clone()).or_insert(new_idx)
    }
}
