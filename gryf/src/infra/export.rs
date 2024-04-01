use std::{
    collections::HashMap,
    fmt::Display,
    io::{self, Cursor, Write},
    marker::PhantomData,
};

use crate::core::{id::IdType, marker::EdgeType, EdgeRef, Edges, VertexRef, Vertices};

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

    pub fn to_string<G>(&self, graph: &G) -> String
    where
        G: Vertices<V> + Edges<E, Ty>,
    {
        let mut cursor = Cursor::new(Vec::new());
        self.export(graph, &mut cursor)
            .expect("writing to vec in cursor does not fail");

        String::from_utf8(cursor.into_inner()).expect("dot format is text format")
    }
}

impl<V: Display, E: Display, Ty: EdgeType> Dot<V, E, Ty> {
    pub fn with_display(name: Option<String>) -> Self {
        Self::new(name, |v| format!("{v}"), |e| format!("{e}"))
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
                    indexer.get(vertex.id()),
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
struct Indexer<Id>(HashMap<Id, usize>);

impl<Id: IdType> Indexer<Id> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&mut self, idx: &Id) -> usize {
        let new_idx = self.0.len();
        *self.0.entry(idx.clone()).or_insert(new_idx)
    }
}
