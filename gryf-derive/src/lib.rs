mod util;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type};

#[proc_macro_derive(GraphBase, attributes(graph))]
pub fn graph_base(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { GraphBase })],
    );

    let implemented = quote! {
        impl #impl_generics GraphBase for #name #ty_generics #where_clause {
            type VertexId = <#field_type as GraphBase>::VertexId;
            type EdgeId = <#field_type as GraphBase>::EdgeId;
            type EdgeType = <#field_type as GraphBase>::EdgeType;
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(VerticesBase, attributes(graph))]
pub fn vertices_base(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { VerticesBase })],
    );

    let implemented = quote! {
        impl #impl_generics VerticesBase for #name #ty_generics #where_clause {
            type VertexIdsIter<'a> = <#field_type as VerticesBase>::VertexIdsIter<'a>
            where
                Self: 'a;

            fn vertex_count(&self) -> usize {
                <#field_type as VerticesBase>::vertex_count(&self.#field_name)
            }

            fn vertex_bound(&self) -> usize {
                <#field_type as VerticesBase>::vertex_bound(&self.#field_name)
            }

            fn vertex_ids(&self) -> Self::VertexIdsIter<'_> {
                <#field_type as VerticesBase>::vertex_ids(&self.#field_name)
            }

            fn contains_vertex(&self, id: &Self::VertexId) -> bool {
                <#field_type as VerticesBase>::contains_vertex(&self.#field_name, id)
            }

            fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
            where
                Self::VertexId: IntegerIdType
            {
                <#field_type as VerticesBase>::vertex_id_map(&self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

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
            type VertexRef<'a> = <#field_type as Vertices<V>>::VertexRef<'a>
            where
                Self: 'a,
                V: 'a;

            type VerticesIter<'a> = <#field_type as Vertices<V>>::VerticesIter<'a>
            where
                Self: 'a,
                V: 'a;

            fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
                <#field_type as Vertices<V>>::vertex(&self.#field_name, id)
            }

            fn vertices(&self) -> Self::VerticesIter<'_> {
                <#field_type as Vertices<V>>::vertices(&self.#field_name)
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
            fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
                <#field_type as VerticesMut<V>>::vertex_mut(&mut self.#field_name, id)
            }

            fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
                <#field_type as VerticesMut<V>>::try_add_vertex(&mut self.#field_name, vertex)
            }

            fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
                <#field_type as VerticesMut<V>>::remove_vertex(&mut self.#field_name, id)
            }

            fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
                <#field_type as VerticesMut<V>>::replace_vertex(&mut self.#field_name, id, vertex)
            }

            fn clear(&mut self) {
                <#field_type as VerticesMut<V>>::clear(&mut self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(VerticesBaseWeak)]
pub fn vertices_base_weak(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            // The derived implementation is based on non-weak functionality.
            (
                syn::parse_str::<Type>("Self").unwrap(),
                quote! { VerticesBase },
            ),
        ],
    );

    let implemented = quote! {
        impl #impl_generics VerticesBaseWeak for #name #ty_generics #where_clause {
            fn vertex_count_hint(&self) -> Option<usize> {
                Some(<Self as VerticesBase>::vertex_count(self))
            }

            fn vertex_bound_hint(&self) -> Option<usize> {
                Some(<Self as VerticesBase>::vertex_bound(self))
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
            fn vertex_weak(&self, id: &Self::VertexId) -> Option<WeakRef<'_, V>> {
                <Self as Vertices<V>>::vertex(self, id).map(|vertex| WeakRef::Borrowed(vertex))
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(EdgesBase, attributes(graph))]
pub fn edges_base(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["Ty"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            (field_type.clone(), quote! { EdgesBase<Ty> }),
            (syn::parse_str::<Type>("Ty").unwrap(), quote! { EdgeType }),
        ],
    );

    let implemented = quote! {
        impl #impl_generics EdgesBase<Ty> for #name #ty_generics #where_clause {
            type EdgeIdsIter<'a> = <#field_type as EdgesBase<Ty>>::EdgeIdsIter<'a>
            where
                Self: 'a;
            type EdgeIdIter<'a> = <#field_type as EdgesBase<Ty>>::EdgeIdIter<'a>
            where
                Self: 'a;

            fn edge_count(&self) -> usize {
                <#field_type as EdgesBase<Ty>>::edge_count(&self.#field_name)
            }

            fn edge_bound(&self) -> usize {
                <#field_type as EdgesBase<Ty>>::edge_bound(&self.#field_name)
            }

            fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
                <#field_type as EdgesBase<Ty>>::endpoints(&self.#field_name, id)
            }

            fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
                <#field_type as EdgesBase<Ty>>::edge_id(&self.#field_name, src, dst)
            }

            fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
                <#field_type as EdgesBase<Ty>>::edge_ids(&self.#field_name)
            }

            fn contains_edge(&self, id: &Self::EdgeId) -> bool {
                <#field_type as EdgesBase<Ty>>::contains_edge(&self.#field_name, id)
            }

            fn edge_id_any(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId> {
                <#field_type as EdgesBase<Ty>>::edge_id_any(&self.#field_name, src, dst)
            }

            fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
            where
                Self::EdgeId: IntegerIdType
            {
                <#field_type as EdgesBase<Ty>>::edge_id_map(&self.#field_name)
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
            type EdgeRef<'a> = <#field_type as Edges<E, Ty>>::EdgeRef<'a>
            where
                Self: 'a,
                E: 'a;

            type EdgesIter<'a> = <#field_type as Edges<E, Ty>>::EdgesIter<'a>
            where
                Self: 'a,
                E: 'a;

            fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
                <#field_type as Edges<E, Ty>>::edge(&self.#field_name, id)
            }

            fn edges(&self) -> Self::EdgesIter<'_> {
                <#field_type as Edges<E, Ty>>::edges(&self.#field_name)
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
            fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
                <#field_type as EdgesMut<E, Ty>>::edge_mut(&mut self.#field_name, id)
            }

            fn try_add_edge(
                &mut self,
                src: &Self::VertexId,
                dst: &Self::VertexId,
                edge: E,
            ) -> Result<Self::EdgeId, AddEdgeError<E>> {
                <#field_type as EdgesMut<E, Ty>>::try_add_edge(&mut self.#field_name, src, dst, edge)
            }

            fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
                <#field_type as EdgesMut<E, Ty>>::remove_edge(&mut self.#field_name, id)
            }

            fn replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> E {
                <#field_type as EdgesMut<E, Ty>>::replace_edge(&mut self.#field_name, id, edge)
            }

            fn clear_edges(&mut self) {
                <#field_type as EdgesMut<E, Ty>>::clear_edges(&mut self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(EdgesBaseWeak)]
pub fn edges_base_weak(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["Ty"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            // The derived implementation is based on non-weak functionality.
            (
                syn::parse_str::<Type>("Self").unwrap(),
                quote! { EdgesBase<Ty> },
            ),
            (syn::parse_str::<Type>("Ty").unwrap(), quote! { EdgeType }),
        ],
    );

    let implemented = quote! {
        impl #impl_generics EdgesBaseWeak<Ty> for #name #ty_generics #where_clause {
            fn edge_count_hint(&self) -> Option<usize> {
                Some(<Self as EdgesBase<Ty>>::edge_count(self))
            }

            fn edge_bound_hint(&self) -> Option<usize> {
                Some(<Self as EdgesBase<Ty>>::edge_bound(self))
            }

            fn endpoints_weak(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
                <Self as EdgesBase<Ty>>::endpoints(self, id)
            }

            fn edge_id_weak(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId> {
                <Self as EdgesBase<Ty>>::edge_id_any(self, src, dst)
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
            fn edge_weak(&self, id: &Self::EdgeId) -> Option<WeakRef<'_, E>> {
                <Self as Edges<E, Ty>>::edge(self, id).map(|edge| WeakRef::Borrowed(edge))
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

    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["Ty"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![
            (field_type.clone(), quote! { MultiEdges<Ty> }),
            (syn::parse_str::<Type>("Ty").unwrap(), quote! { EdgeType }),
        ],
    );

    let implemented = quote! {
        impl #impl_generics MultiEdges<Ty> for #name #ty_generics #where_clause {}
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
            type NeighborRef<'a> = <#field_type as Neighbors>::NeighborRef<'a>
            where
                Self: 'a;

            type NeighborsIter<'a> = <#field_type as Neighbors>::NeighborsIter<'a>
            where
                Self: 'a;

            fn neighbors(&self, src: &Self::VertexId) -> Self::NeighborsIter<'_> {
                <#field_type as Neighbors>::neighbors(&self.#field_name, src)
            }

            fn neighbors_directed(&self, src: &Self::VertexId, dir: Direction) -> Self::NeighborsIter<'_> {
                <#field_type as Neighbors>::neighbors_directed(&self.#field_name, src, dir)
            }

            fn degree(&self, id: &Self::VertexId) -> usize {
                <#field_type as Neighbors>::degree(&self.#field_name, id)
            }

            fn degree_directed(&self, id: &Self::VertexId, dir: Direction) -> usize {
                <#field_type as Neighbors>::degree_directed(&self.#field_name, id, dir)
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

            fn is_connected() -> bool {
                #field_type::is_connected()
            }
        }
    };

    TokenStream::from(implemented)
}
