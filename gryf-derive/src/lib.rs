mod util;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(GraphBase, attributes(graph))]
pub fn graph_base(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
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

            fn vertex_count_hint(&self) -> Option<usize> {
                <#field_type as GraphBase>::vertex_count_hint(&self.#field_name)
            }

            fn edge_count_hint(&self) -> Option<usize> {
                <#field_type as GraphBase>::edge_count_hint(&self.#field_name)
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

#[proc_macro_derive(VertexSet, attributes(graph))]
pub fn vertex_set(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { VertexSet })],
    );

    let implemented = quote! {
        impl #impl_generics VertexSet for #name #ty_generics #where_clause {
            type VertexIdsIter<'a> = <#field_type as VertexSet>::VertexIdsIter<'a>
            where
                Self: 'a;

            fn vertex_ids(&self) -> Self::VertexIdsIter<'_> {
                <#field_type as VertexSet>::vertex_ids(&self.#field_name)
            }

            fn vertex_count(&self) -> usize {
                <#field_type as VertexSet>::vertex_count(&self.#field_name)
            }

            fn vertex_bound(&self) -> usize
            where
                Self::VertexId: IntegerIdType,
            {
                <#field_type as VertexSet>::vertex_bound(&self.#field_name)
            }

            fn contains_vertex(&self, id: &Self::VertexId) -> bool {
                <#field_type as VertexSet>::contains_vertex(&self.#field_name, id)
            }

            fn vertex_id_map(&self) -> CompactIdMap<Self::VertexId>
            where
                Self::VertexId: IntegerIdType
            {
                <#field_type as VertexSet>::vertex_id_map(&self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(EdgeSet, attributes(graph))]
pub fn edge_set(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause =
        util::augment_where_clause(where_clause, vec![(field_type.clone(), quote! { EdgeSet })]);

    let implemented = quote! {
        impl #impl_generics EdgeSet for #name #ty_generics #where_clause {
            type EdgeIdsIter<'a> = <#field_type as EdgeSet>::EdgeIdsIter<'a>
            where
                Self: 'a;

            type EdgeIdIter<'a> = <#field_type as EdgeSet>::EdgeIdIter<'a>
            where
                Self: 'a;

            fn edge_ids(&self) -> Self::EdgeIdsIter<'_> {
                <#field_type as EdgeSet>::edge_ids(&self.#field_name)
            }

            fn edge_id(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Self::EdgeIdIter<'_> {
                <#field_type as EdgeSet>::edge_id(&self.#field_name, src, dst)
            }

            fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
                <#field_type as EdgeSet>::endpoints(&self.#field_name, id)
            }

            fn edge_count(&self) -> usize {
                <#field_type as EdgeSet>::edge_count(&self.#field_name)
            }

            fn edge_bound(&self) -> usize
            where
                Self::EdgeId: IntegerIdType,
            {
                <#field_type as EdgeSet>::edge_bound(&self.#field_name)
            }

            fn contains_edge(&self, id: &Self::EdgeId) -> bool {
                <#field_type as EdgeSet>::contains_edge(&self.#field_name, id)
            }

            fn edge_id_any(&self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<Self::EdgeId> {
                <#field_type as EdgeSet>::edge_id_any(&self.#field_name, src, dst)
            }

            fn edge_id_map(&self) -> CompactIdMap<Self::EdgeId>
            where
                Self::EdgeId: IntegerIdType
            {
                <#field_type as EdgeSet>::edge_id_map(&self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphRef, attributes(graph))]
pub fn graph_ref(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { GraphRef<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics GraphRef<V, E> for #name #ty_generics #where_clause {
            type VertexRef<'a> = <#field_type as GraphRef<V, E>>::VertexRef<'a>
            where
                Self: 'a,
                V: 'a;

            type VerticesIter<'a> = <#field_type as GraphRef<V, E>>::VerticesIter<'a>
            where
                Self: 'a,
                V: 'a;

            type EdgeRef<'a> = <#field_type as GraphRef<V, E>>::EdgeRef<'a>
            where
                Self: 'a,
                E: 'a;

            type EdgesIter<'a> = <#field_type as GraphRef<V, E>>::EdgesIter<'a>
            where
                Self: 'a,
                E: 'a;

            fn vertices(&self) -> Self::VerticesIter<'_> {
                <#field_type as GraphRef<V, E>>::vertices(&self.#field_name)
            }

            fn edges(&self) -> Self::EdgesIter<'_> {
                <#field_type as GraphRef<V, E>>::edges(&self.#field_name)
            }

            fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
                <#field_type as GraphRef<V, E>>::vertex(&self.#field_name, id)
            }

            fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
                <#field_type as GraphRef<V, E>>::edge(&self.#field_name, id)
            }

            fn find_vertex(&self, vertex: &V) -> Option<Self::VertexId>
            where
                V: Eq,
            {
                <#field_type as GraphRef<V, E>>::find_vertex(&self.#field_name, vertex)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphWeak, attributes(graph))]
pub fn graph_weak(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { GraphWeak<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics GraphWeak<V, E> for #name #ty_generics #where_clause {
            fn vertex_weak(&self, id: &Self::VertexId) -> Option<WeakRef<'_, V>> {
                <#field_type as GraphWeak<V, E>>::vertex_weak(&self.#field_name, id)
            }

            fn edge_weak(&self, id: &Self::EdgeId) -> Option<WeakRef<'_, E>> {
                <#field_type as GraphWeak<V, E>>::edge_weak(&self.#field_name, id)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphMut, attributes(graph))]
pub fn graph_mut(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { GraphMut<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics GraphMut<V, E> for #name #ty_generics #where_clause {
            fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
                <#field_type as GraphMut<V, E>>::vertex_mut(&mut self.#field_name, id)
            }

            fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
                <#field_type as GraphMut<V, E>>::edge_mut(&mut self.#field_name, id)
            }

            fn try_replace_vertex(
                &mut self,
                id: &Self::VertexId,
                vertex: V,
            ) -> Result<V, ReplaceVertexError<V>> {
                <#field_type as GraphMut<V, E>>::try_replace_vertex(&mut self.#field_name, id, vertex)
            }

            fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
                <#field_type as GraphMut<V, E>>::replace_vertex(&mut self.#field_name, id, vertex)
            }

            fn try_replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> Result<E, ReplaceEdgeError<E>> {
                <#field_type as GraphMut<V, E>>::try_replace_edge(&mut self.#field_name, id, edge)
            }

            fn replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> E {
                <#field_type as GraphMut<V, E>>::replace_edge(&mut self.#field_name, id, edge)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphAdd, attributes(graph))]
pub fn graph_add(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { GraphAdd<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics GraphAdd<V, E> for #name #ty_generics #where_clause {
            fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>> {
                <#field_type as GraphAdd<V, E>>::try_add_vertex(&mut self.#field_name, vertex)
            }

            fn try_add_edge(
                &mut self,
                src: &Self::VertexId,
                dst: &Self::VertexId,
                edge: E,
            ) -> Result<Self::EdgeId, AddEdgeError<E>> {
                <#field_type as GraphAdd<V, E>>::try_add_edge(&mut self.#field_name, src, dst, edge)
            }

            fn add_vertex(&mut self, vertex: V) -> Self::VertexId {
                <#field_type as GraphAdd<V, E>>::add_vertex(&mut self.#field_name, vertex)
            }

            fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, AddVertexError<V>>
            where
                V: Eq,
            {
                <#field_type as GraphAdd<V, E>>::try_get_or_add_vertex(&mut self.#field_name, vertex)
            }

            fn get_or_add_vertex(&mut self, vertex: V) -> Self::VertexId
            where
                V: Eq,
            {
                <#field_type as GraphAdd<V, E>>::get_or_add_vertex(&mut self.#field_name, vertex)
            }

            fn add_edge(&mut self, src: &Self::VertexId, dst: &Self::VertexId, edge: E) -> Self::EdgeId {
                <#field_type as GraphAdd<V, E>>::add_edge(&mut self.#field_name, src, dst, edge)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphFull, attributes(graph))]
pub fn graph_full(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { GraphFull<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics GraphFull<V, E> for #name #ty_generics #where_clause {
            fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
                <#field_type as GraphFull<V, E>>::remove_vertex(&mut self.#field_name, id)
            }

            fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
                <#field_type as GraphFull<V, E>>::remove_edge(&mut self.#field_name, id)
            }

            fn clear(&mut self) {
                <#field_type as GraphFull<V, E>>::clear(&mut self.#field_name)
            }

            fn remove_edge_between(&mut self, src: &Self::VertexId, dst: &Self::VertexId) -> Option<E> {
                <#field_type as GraphFull<V, E>>::remove_edge_between(&mut self.#field_name, src, dst)
            }

            fn clear_edges(&mut self) {
                <#field_type as GraphFull<V, E>>::clear_edges(&mut self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(MultiEdge, attributes(graph))]
pub fn multi_edge(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { MultiEdge })],
    );

    let implemented = quote! {
        impl #impl_generics MultiEdge for #name #ty_generics #where_clause {}
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
