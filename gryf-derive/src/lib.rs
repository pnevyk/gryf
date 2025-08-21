mod util;

use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

#[proc_macro_derive(GraphBase, attributes(graph, gryf_crate))]
pub fn graph_base(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::GraphBase })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::GraphBase for #name #ty_generics #where_clause {
            type VertexId = <#field_type as #gryf::core::GraphBase>::VertexId;
            type EdgeId = <#field_type as #gryf::core::GraphBase>::EdgeId;
            type EdgeType = <#field_type as #gryf::core::GraphBase>::EdgeType;

            fn vertex_count_hint(&self) -> Option<usize> {
                <#field_type as #gryf::core::GraphBase>::vertex_count_hint(&self.#field_name)
            }

            fn edge_count_hint(&self) -> Option<usize> {
                <#field_type as #gryf::core::GraphBase>::edge_count_hint(&self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(Neighbors, attributes(graph, gryf_crate))]
pub fn neighbors(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::Neighbors })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::Neighbors for #name #ty_generics #where_clause {
            type NeighborRef<'a> = <#field_type as #gryf::core::Neighbors>::NeighborRef<'a>
            where
                Self: 'a;

            type NeighborsIter<'a> = <#field_type as #gryf::core::Neighbors>::NeighborsIter<'a>
            where
                Self: 'a;

            fn neighbors_undirected(&self, from: &Self::VertexId) -> Self::NeighborsIter<'_> {
                <#field_type as #gryf::core::Neighbors>::neighbors_undirected(&self.#field_name, from)
            }

            fn neighbors_directed(&self, from: &Self::VertexId, dir: #gryf::core::marker::Direction) -> Self::NeighborsIter<'_> {
                <#field_type as #gryf::core::Neighbors>::neighbors_directed(&self.#field_name, from, dir)
            }

            fn degree_undirected(&self, id: &Self::VertexId) -> usize {
                <#field_type as #gryf::core::Neighbors>::degree_undirected(&self.#field_name, id)
            }

            fn degree_directed(&self, id: &Self::VertexId, dir: #gryf::core::marker::Direction) -> usize {
                <#field_type as #gryf::core::Neighbors>::degree_directed(&self.#field_name, id, dir)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(VertexSet, attributes(graph, gryf_crate))]
pub fn vertex_set(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::VertexSet })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::VertexSet for #name #ty_generics #where_clause {
            type VerticesByIdIter<'a> = <#field_type as #gryf::core::VertexSet>::VerticesByIdIter<'a>
            where
                Self: 'a;

            fn vertices_by_id(&self) -> Self::VerticesByIdIter<'_> {
                <#field_type as #gryf::core::VertexSet>::vertices_by_id(&self.#field_name)
            }

            fn vertex_count(&self) -> usize {
                <#field_type as #gryf::core::VertexSet>::vertex_count(&self.#field_name)
            }

            fn vertex_bound(&self) -> usize
            where
                Self::VertexId: #gryf::core::id::IntegerIdType,
            {
                <#field_type as #gryf::core::VertexSet>::vertex_bound(&self.#field_name)
            }

            fn contains_vertex(&self, id: &Self::VertexId) -> bool {
                <#field_type as #gryf::core::VertexSet>::contains_vertex(&self.#field_name, id)
            }

            fn vertex_id_map(&self) -> #gryf::core::id::CompactIdMap<Self::VertexId>
            where
                Self::VertexId: #gryf::core::id::IntegerIdType
            {
                <#field_type as #gryf::core::VertexSet>::vertex_id_map(&self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(EdgeSet, attributes(graph, gryf_crate))]
pub fn edge_set(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::EdgeSet })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::EdgeSet for #name #ty_generics #where_clause {
            type EdgesByIdIter<'a> = <#field_type as #gryf::core::EdgeSet>::EdgesByIdIter<'a>
            where
                Self: 'a;

            type EdgeIdIter<'a> = <#field_type as #gryf::core::EdgeSet>::EdgeIdIter<'a>
            where
                Self: 'a;

            fn edges_by_id(&self) -> Self::EdgesByIdIter<'_> {
                <#field_type as #gryf::core::EdgeSet>::edges_by_id(&self.#field_name)
            }

            fn edge_id(&self, from: &Self::VertexId, to: &Self::VertexId) -> Self::EdgeIdIter<'_> {
                <#field_type as #gryf::core::EdgeSet>::edge_id(&self.#field_name, from, to)
            }

            fn endpoints(&self, id: &Self::EdgeId) -> Option<(Self::VertexId, Self::VertexId)> {
                <#field_type as #gryf::core::EdgeSet>::endpoints(&self.#field_name, id)
            }

            fn edge_count(&self) -> usize {
                <#field_type as #gryf::core::EdgeSet>::edge_count(&self.#field_name)
            }

            fn edge_bound(&self) -> usize
            where
                Self::EdgeId: #gryf::core::id::IntegerIdType,
            {
                <#field_type as #gryf::core::EdgeSet>::edge_bound(&self.#field_name)
            }

            fn contains_edge(&self, id: &Self::EdgeId) -> bool {
                <#field_type as #gryf::core::EdgeSet>::contains_edge(&self.#field_name, id)
            }

            fn contains_edge_between(&self, from: &Self::VertexId, to: &Self::VertexId) -> bool {
                <#field_type as #gryf::core::EdgeSet>::contains_edge_between(&self.#field_name, from, to)
            }

            fn edge_id_any(&self, from: &Self::VertexId, to: &Self::VertexId) -> Option<Self::EdgeId> {
                <#field_type as #gryf::core::EdgeSet>::edge_id_any(&self.#field_name, from, to)
            }

            fn edge_id_map(&self) -> #gryf::core::id::CompactIdMap<Self::EdgeId>
            where
                Self::EdgeId: #gryf::core::id::IntegerIdType
            {
                <#field_type as #gryf::core::EdgeSet>::edge_id_map(&self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphRef, attributes(graph, gryf_crate))]
pub fn graph_ref(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::GraphRef<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::GraphRef<V, E> for #name #ty_generics #where_clause {
            type VertexRef<'a> = <#field_type as #gryf::core::GraphRef<V, E>>::VertexRef<'a>
            where
                Self: 'a,
                V: 'a;

            type VerticesIter<'a> = <#field_type as #gryf::core::GraphRef<V, E>>::VerticesIter<'a>
            where
                Self: 'a,
                V: 'a;

            type EdgeRef<'a> = <#field_type as #gryf::core::GraphRef<V, E>>::EdgeRef<'a>
            where
                Self: 'a,
                E: 'a;

            type EdgesIter<'a> = <#field_type as #gryf::core::GraphRef<V, E>>::EdgesIter<'a>
            where
                Self: 'a,
                E: 'a;

            fn vertices(&self) -> Self::VerticesIter<'_> {
                <#field_type as #gryf::core::GraphRef<V, E>>::vertices(&self.#field_name)
            }

            fn edges(&self) -> Self::EdgesIter<'_> {
                <#field_type as #gryf::core::GraphRef<V, E>>::edges(&self.#field_name)
            }

            fn vertex(&self, id: &Self::VertexId) -> Option<&V> {
                <#field_type as #gryf::core::GraphRef<V, E>>::vertex(&self.#field_name, id)
            }

            fn edge(&self, id: &Self::EdgeId) -> Option<&E> {
                <#field_type as #gryf::core::GraphRef<V, E>>::edge(&self.#field_name, id)
            }

            fn find_vertex(&self, vertex: &V) -> Option<Self::VertexId>
            where
                V: Eq,
            {
                <#field_type as #gryf::core::GraphRef<V, E>>::find_vertex(&self.#field_name, vertex)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphMut, attributes(graph, gryf_crate))]
pub fn graph_mut(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::GraphMut<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::GraphMut<V, E> for #name #ty_generics #where_clause {
            fn vertex_mut(&mut self, id: &Self::VertexId) -> Option<&mut V> {
                <#field_type as #gryf::core::GraphMut<V, E>>::vertex_mut(&mut self.#field_name, id)
            }

            fn edge_mut(&mut self, id: &Self::EdgeId) -> Option<&mut E> {
                <#field_type as #gryf::core::GraphMut<V, E>>::edge_mut(&mut self.#field_name, id)
            }

            fn try_replace_vertex(
                &mut self,
                id: &Self::VertexId,
                vertex: V,
            ) -> Result<V, #gryf::core::error::ReplaceVertexError<V>> {
                <#field_type as #gryf::core::GraphMut<V, E>>::try_replace_vertex(&mut self.#field_name, id, vertex)
            }

            fn replace_vertex(&mut self, id: &Self::VertexId, vertex: V) -> V {
                <#field_type as #gryf::core::GraphMut<V, E>>::replace_vertex(&mut self.#field_name, id, vertex)
            }

            fn try_replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> Result<E, #gryf::core::error::ReplaceEdgeError<E>> {
                <#field_type as #gryf::core::GraphMut<V, E>>::try_replace_edge(&mut self.#field_name, id, edge)
            }

            fn replace_edge(&mut self, id: &Self::EdgeId, edge: E) -> E {
                <#field_type as #gryf::core::GraphMut<V, E>>::replace_edge(&mut self.#field_name, id, edge)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphAdd, attributes(graph, gryf_crate))]
pub fn graph_add(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::GraphAdd<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::GraphAdd<V, E> for #name #ty_generics #where_clause {
            fn try_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, #gryf::core::error::AddVertexError<V>> {
                <#field_type as #gryf::core::GraphAdd<V, E>>::try_add_vertex(&mut self.#field_name, vertex)
            }

            fn try_add_edge(
                &mut self,
                from: &Self::VertexId,
                to: &Self::VertexId,
                edge: E,
            ) -> Result<Self::EdgeId, #gryf::core::error::AddEdgeError<E>> {
                <#field_type as #gryf::core::GraphAdd<V, E>>::try_add_edge(&mut self.#field_name, from, to, edge)
            }

            fn add_vertex(&mut self, vertex: V) -> Self::VertexId {
                <#field_type as #gryf::core::GraphAdd<V, E>>::add_vertex(&mut self.#field_name, vertex)
            }

            fn try_get_or_add_vertex(&mut self, vertex: V) -> Result<Self::VertexId, #gryf::core::error::AddVertexError<V>>
            where
                V: Eq,
            {
                <#field_type as #gryf::core::GraphAdd<V, E>>::try_get_or_add_vertex(&mut self.#field_name, vertex)
            }

            fn get_or_add_vertex(&mut self, vertex: V) -> Self::VertexId
            where
                V: Eq,
            {
                <#field_type as #gryf::core::GraphAdd<V, E>>::get_or_add_vertex(&mut self.#field_name, vertex)
            }

            fn add_edge(&mut self, from: &Self::VertexId, to: &Self::VertexId, edge: E) -> Self::EdgeId {
                <#field_type as #gryf::core::GraphAdd<V, E>>::add_edge(&mut self.#field_name, from, to, edge)
            }

            fn try_add_edge_connecting(
                &mut self,
                from: V,
                to: V,
                edge: E,
            ) -> Result<Self::EdgeId, #gryf::core::error::AddEdgeConnectingError<V, E>>
            where
                V: Eq,
            {
                <#field_type as #gryf::core::GraphAdd<V, E>>::try_add_edge_connecting(&mut self.#field_name, from, to, edge)
            }

            fn add_edge_connecting(&mut self, from: V, to: V, edge: E) -> Self::EdgeId
            where
                V: Eq,
            {
                <#field_type as #gryf::core::GraphAdd<V, E>>::add_edge_connecting(&mut self.#field_name, from, to, edge)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(GraphFull, attributes(graph, gryf_crate))]
pub fn graph_full(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let impl_generics = util::augment_impl_generics_if_necessary(impl_generics, vec!["V", "E"]);
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::GraphFull<V, E> })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::GraphFull<V, E> for #name #ty_generics #where_clause {
            fn remove_vertex(&mut self, id: &Self::VertexId) -> Option<V> {
                <#field_type as #gryf::core::GraphFull<V, E>>::remove_vertex(&mut self.#field_name, id)
            }

            fn remove_edge(&mut self, id: &Self::EdgeId) -> Option<E> {
                <#field_type as #gryf::core::GraphFull<V, E>>::remove_edge(&mut self.#field_name, id)
            }

            fn clear(&mut self) {
                <#field_type as #gryf::core::GraphFull<V, E>>::clear(&mut self.#field_name)
            }

            fn remove_edges_between(&mut self, from: &Self::VertexId, to: &Self::VertexId) {
                <#field_type as #gryf::core::GraphFull<V, E>>::remove_edges_between(&mut self.#field_name, from, to)
            }

            fn remove_edge_any_between(&mut self, from: &Self::VertexId, to: &Self::VertexId) -> Option<E> {
                <#field_type as #gryf::core::GraphFull<V, E>>::remove_edge_any_between(&mut self.#field_name, from, to)
            }

            fn clear_edges(&mut self) {
                <#field_type as #gryf::core::GraphFull<V, E>>::clear_edges(&mut self.#field_name)
            }
        }
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(MultiEdge, attributes(graph, gryf_crate))]
pub fn multi_edge(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::props::MultiEdge })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::props::MultiEdge for #name #ty_generics #where_clause {}
    };

    TokenStream::from(implemented)
}

#[proc_macro_derive(Guarantee, attributes(graph, gryf_crate))]
pub fn guarantee(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let gryf = util::get_gryf_path(&input);

    let name = &input.ident;
    let field = util::get_graph_field(&input);

    let field_type = &field.ty;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let where_clause = util::augment_where_clause(
        where_clause,
        vec![(field_type.clone(), quote! { #gryf::core::props::Guarantee })],
    );

    let implemented = quote! {
        impl #impl_generics #gryf::core::props::Guarantee for #name #ty_generics #where_clause {
            fn is_loop_free() -> bool {
                <#field_type as #gryf::core::props::Guarantee>::is_loop_free()
            }

            fn has_paths_only() -> bool {
                <#field_type as #gryf::core::props::Guarantee>::has_paths_only()
            }

            fn has_trees_only() -> bool {
                <#field_type as #gryf::core::props::Guarantee>::has_trees_only()
            }

            fn has_bipartite_only() -> bool {
                <#field_type as #gryf::core::props::Guarantee>::has_bipartite_only()
            }

            fn is_connected() -> bool {
                <#field_type as #gryf::core::props::Guarantee>::is_connected()
            }
        }
    };

    TokenStream::from(implemented)
}
