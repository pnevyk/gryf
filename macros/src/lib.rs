mod util;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type};

#[proc_macro_derive(Vertices, attributes(graph))]
pub fn vertices(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { Vertices<V> })],
    );

    let implemented = quote! {
        impl #impl_generics Vertices<V> for #name #ty_generics #where_clause {
            type VertexRef<'a, T: 'a> = <#field_type as Vertices<V>>::VertexRef<'a, T>;

            type VertexIndicesIter<'a>
            where
                Self: 'a,
            = <#field_type as Vertices<V>>::VertexIndicesIter<'a>;

            type VerticesIter<'a, T: 'a>
            where
                Self: 'a,
            = <#field_type as Vertices<V>>::VerticesIter<'a, T>;

            fn vertex_count(&self) -> usize {
                self.#field_name.vertex_count()
            }

            fn vertex_bound(&self) -> usize {
                self.#field_name.vertex_bound()
            }

            fn vertex(&self, index: VertexIndex) -> Option<&V> {
                self.#field_name.vertex(index)
            }

            fn vertex_indices(&self) -> Self::VertexIndicesIter<'_> {
                self.#field_name.vertex_indices()
            }

            fn vertices(&self) -> Self::VerticesIter<'_, V> {
                self.#field_name.vertices()
            }

            fn contains_vertex(&self, index: VertexIndex) -> bool {
                self.#field_name.contains_vertex(index)
            }

            fn vertex_index_map(&self) -> CompactIndexMap<VertexIndex> {
                self.#field_name.vertex_index_map()
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(VerticesMut, attributes(graph))]
pub fn vertices_mut(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { VerticesMut<V> })],
    );

    let implemented = quote! {
        impl #impl_generics VerticesMut<V> for #name #ty_generics #where_clause {
            fn vertex_mut(&mut self, index: VertexIndex) -> Option<&mut V> {
                self.#field_name.vertex_mut(index)
            }

            fn add_vertex(&mut self, vertex: V) -> VertexIndex {
                self.#field_name.add_vertex(vertex)
            }

            fn remove_vertex(&mut self, index: VertexIndex) -> Option<V> {
                self.#field_name.remove_vertex(index)
            }

            fn replace_vertex(&mut self, index: VertexIndex, vertex: V) -> V {
                self.#field_name.replace_vertex(index, vertex)
            }

            fn clear(&mut self) {
                self.#field_name.clear();
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(VerticesWeak)]
pub fn vertices_weak(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            // The derived implementation is based on non-weak functionality.
            (
                syn::parse_str::<Type>("Self").unwrap(),
                quote! { Vertices<V> },
            ),
        ],
    );

    let implemented = quote! {
        impl #impl_generics VerticesWeak<V> for #name #ty_generics #where_clause {
            fn vertex_count_hint(&self) -> Option<usize> {
                Some(self.vertex_count())
            }

            fn vertex_bound_hint(&self) -> Option<usize> {
                Some(self.vertex_bound())
            }

            fn vertex_weak(&self, index: VertexIndex) -> Option<WeakRef<'_, V>> {
                self.vertex(index).map(|vertex| WeakRef::borrowed(vertex))
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(Edges, attributes(graph))]
pub fn edges(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["E", "Ty"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            (field_type.clone(), quote! { Edges<E, Ty> }),
            (syn::parse_str::<Type>("Ty").unwrap(), quote! { EdgeType }),
        ],
    );

    let implemented = quote! {
        impl #impl_generics Edges<E, Ty> for #name #ty_generics #where_clause {
            type EdgeRef<'a, T: 'a> = <#field_type as Edges<E, Ty>>::EdgeRef<'a, T>;

            type EdgeIndicesIter<'a>
            where
                Self: 'a,
            = <#field_type as Edges<E, Ty>>::EdgeIndicesIter<'a>;

            type EdgesIter<'a, T: 'a>
            where
                Self: 'a,
            = <#field_type as Edges<E, Ty>>::EdgesIter<'a, T>;

            fn edge_count(&self) -> usize {
                self.#field_name.edge_count()
            }

            fn edge_bound(&self) -> usize {
                self.#field_name.edge_bound()
            }

            fn edge(&self, index: EdgeIndex) -> Option<&E> {
                self.#field_name.edge(index)
            }

            fn endpoints(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
                self.#field_name.endpoints(index)
            }

            fn edge_index(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
                self.#field_name.edge_index(src, dst)
            }

            fn edge_indices(&self) -> Self::EdgeIndicesIter<'_> {
                self.#field_name.edge_indices()
            }

            fn edges(&self) -> Self::EdgesIter<'_, E> {
                self.#field_name.edges()
            }

            fn contains_edge(&self, index: EdgeIndex) -> bool {
                self.#field_name.contains_edge(index)
            }

            fn edge_index_map(&self) -> CompactIndexMap<EdgeIndex> {
                self.#field_name.edge_index_map()
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(EdgesMut, attributes(graph))]
pub fn edges_mut(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["E", "Ty"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            (field_type.clone(), quote! { EdgesMut<E, Ty> }),
            (syn::parse_str::<Type>("Ty").unwrap(), quote! { EdgeType }),
        ],
    );

    let implemented = quote! {
        impl #impl_generics EdgesMut<E, Ty> for #name #ty_generics #where_clause {
            fn edge_mut(&mut self, index: EdgeIndex) -> Option<&mut E> {
                self.#field_name.edge_mut(index)
            }

            fn add_edge(&mut self, src: VertexIndex, dst: VertexIndex, edge: E) -> EdgeIndex {
                self.#field_name.add_edge(src, dst, edge)
            }

            fn remove_edge(&mut self, index: EdgeIndex) -> Option<E> {
                self.#field_name.remove_edge(index)
            }

            fn replace_edge(&mut self, index: EdgeIndex, edge: E) -> E {
                self.#field_name.replace_edge(index, edge)
            }

            fn clear_edges(&mut self) {
                self.#field_name.clear_edges();
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(EdgesWeak)]
pub fn edges_weak(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["E", "Ty"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            // The derived implementation is based on non-weak functionality.
            (
                syn::parse_str::<Type>("Self").unwrap(),
                quote! { Edges<E, Ty> },
            ),
            (syn::parse_str::<Type>("Ty").unwrap(), quote! { EdgeType }),
        ],
    );

    let implemented = quote! {
        impl #impl_generics EdgesWeak<E, Ty> for #name #ty_generics #where_clause {
            fn edge_count_hint(&self) -> Option<usize> {
                Some(self.edge_count())
            }

            fn edge_bound_hint(&self) -> Option<usize> {
                Some(self.edge_bound())
            }

            fn edge_weak(&self, index: EdgeIndex) -> Option<WeakRef<'_, E>> {
                self.edge(index).map(|edge| WeakRef::borrowed(edge))
            }

            fn endpoints_weak(&self, index: EdgeIndex) -> Option<(VertexIndex, VertexIndex)> {
                self.endpoints(index)
            }

            fn edge_index_weak(&self, src: VertexIndex, dst: VertexIndex) -> Option<EdgeIndex> {
                self.edge_index(src, dst)
            }

            fn contains_edge_weak(&self, index: EdgeIndex) -> bool {
                self.contains_edge(index)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(MultiEdges, attributes(graph))]
pub fn multi_edges(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["E", "Ty"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            (field_type.clone(), quote! { MultiEdges<E, Ty> }),
            (syn::parse_str::<Type>("Ty").unwrap(), quote! { EdgeType }),
        ],
    );

    let implemented = quote! {
        impl #impl_generics MultiEdges<E, Ty> for #name #ty_generics #where_clause {
            type MultiEdgeIndicesIter<'a>
            where
                Self: 'a,
            = <#field_type as MultiEdges<E, Ty>>::MultiEdgeIndicesIter<'a>;

            fn multi_edge_index(
                &self,
                src: VertexIndex,
                dst: VertexIndex,
            ) -> Self::MultiEdgeIndicesIter<'_> {
                self.#field_name.multi_edge_index(src, dst)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(Neighbors, attributes(graph))]
pub fn neighbors(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec![]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { Neighbors })],
    );

    let implemented = quote! {
        impl #impl_generics Neighbors for #name #ty_generics #where_clause {
            type NeighborRef<'a> = <#field_type as Neighbors>::NeighborRef<'a>;

            type NeighborsIter<'a>
            where
                Self: 'a,
            = <#field_type as Neighbors>::NeighborsIter<'a>;

            fn neighbors(&self, src: VertexIndex) -> Self::NeighborsIter<'_> {
                self.#field_name.neighbors(src)
            }

            fn neighbors_directed(&self, src: VertexIndex, dir: Direction) -> Self::NeighborsIter<'_> {
                self.#field_name.neighbors_directed(src, dir)
            }

            fn degree(&self, index: VertexIndex) -> usize {
                self.#field_name.degree(index)
            }

            fn degree_directed(&self, index: VertexIndex, dir: Direction) -> usize {
                self.#field_name.degree_directed(index, dir)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(Guarantee, attributes(graph))]
pub fn guarantee(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { Guarantee })],
    );

    let implemented = quote! {
        impl #impl_generics Guarantee for #name #ty_generics #where_clause {
            fn is_loop_free() -> bool {
                #field_type::is_loop_free()
            }

            fn has_paths_only() -> bool {
                #field_type::has_paths_only()
            }

            fn has_trees_only() -> bool {
                #field_type::has_trees_only()
            }

            fn has_bipartite_only() -> bool {
                #field_type::has_bipartite_only()
            }

            fn is_connected<Ty: EdgeType>() -> bool {
                #field_type::is_connected::<Ty>()
            }
        }
    };

    TokenStream::from(implemented)
}
