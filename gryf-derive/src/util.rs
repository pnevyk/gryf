use std::collections::VecDeque;

use proc_macro2::{Punct, Spacing, Span, TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use syn::{
    parse_quote, Data, DataStruct, DeriveInput, Field, Fields, Ident, ImplGenerics, Path, Type,
    WhereClause,
};

pub fn get_gryf_path(input: &DeriveInput) -> Path {
    let is_gryf_crate = input
        .attrs
        .iter()
        .any(|attr| attr.path.is_ident("gryf_crate"));

    if is_gryf_crate {
        parse_quote! { crate }
    } else {
        parse_quote! { ::gryf }
    }
}

pub fn get_graph_field(input: &DeriveInput) -> &Field {
    let field = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => fields
            .named
            .iter()
            .find(|field| field.attrs.iter().any(|attr| attr.path.is_ident("graph"))),
        _ => panic!("unsupported type (use struct with named fields)"),
    };

    field.expect("graph attribute is required")
}

pub fn augment_impl_generics_if_necessary(
    impl_generics: ImplGenerics,
    generics: Vec<&str>,
) -> TokenStream2 {
    let generics = generics
        .into_iter()
        .map(|generic| syn::parse_str::<Ident>(generic).unwrap())
        .collect::<Vec<_>>();

    let tokens = impl_generics.to_token_stream();

    let iter = AugmentImplGenerics::new(generics, tokens);
    iter.collect::<TokenStream2>()
}

struct AugmentImplGenerics {
    generics: Vec<Ident>,
    iter: proc_macro2::token_stream::IntoIter,
    queued: VecDeque<TokenTree>,
}

impl AugmentImplGenerics {
    pub fn new(generics: Vec<Ident>, tokens: TokenStream2) -> Self {
        let mut queued = VecDeque::new();

        if tokens.is_empty() {
            queued.push_back(TokenTree::Punct(Punct::new('<', Spacing::Alone)));

            for generic in generics.iter() {
                queued.push_back(TokenTree::Ident(generic.clone()));
                queued.push_back(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
            }

            queued.push_back(TokenTree::Punct(Punct::new('>', Spacing::Alone)));
        }

        Self {
            generics,
            iter: tokens.into_iter(),
            queued,
        }
    }
}

impl Iterator for AugmentImplGenerics {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.queued.pop_front() {
            return Some(token);
        }

        let token = self.iter.next()?;

        match token {
            TokenTree::Ident(ref ident) => {
                self.generics.retain(|generic| ident != generic);
                Some(token)
            }
            TokenTree::Punct(ref punct) if punct.as_char() == '>' => {
                for generic in self.generics.iter() {
                    self.queued
                        .push_back(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
                    self.queued.push_back(TokenTree::Ident(generic.clone()));
                }

                self.queued.push_back(token);
                self.queued.pop_front()
            }
            _ => Some(token),
        }
    }
}

pub fn augment_where_clause(
    where_clause: Option<&WhereClause>,
    bounds: Vec<(Type, TokenStream2)>,
) -> TokenStream2 {
    let bounds = bounds
        .into_iter()
        .map(|(ty, bound)| quote! { #ty: #bound })
        .collect::<Vec<_>>();

    let tokens = where_clause
        .map(|wc| wc.to_token_stream())
        .unwrap_or_default();

    let iter = AugmentWhereClause::new(bounds, tokens);
    iter.collect::<TokenStream2>()
}

struct AugmentWhereClause {
    bounds: Vec<TokenStream2>,
    iter: proc_macro2::token_stream::IntoIter,
    queued: VecDeque<TokenTree>,
}

impl AugmentWhereClause {
    pub fn new(bounds: Vec<TokenStream2>, tokens: TokenStream2) -> Self {
        let mut queued = VecDeque::new();

        if tokens.is_empty() {
            queued.push_back(TokenTree::Ident(Ident::new("where", Span::call_site())));

            for bound in bounds.iter() {
                for token in bound.clone() {
                    queued.push_back(token);
                }

                queued.push_back(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
            }
        }

        Self {
            bounds,
            iter: tokens.into_iter(),
            queued,
        }
    }
}

impl Iterator for AugmentWhereClause {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.queued.pop_front() {
            return Some(token);
        }

        let token = self.iter.next()?;

        match token {
            TokenTree::Ident(ref ident) => {
                if ident == &Ident::new("where", Span::call_site()) {
                    for bound in self.bounds.iter() {
                        for token in bound.clone() {
                            self.queued.push_back(token);
                        }

                        self.queued
                            .push_back(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
                    }
                }

                Some(token)
            }
            _ => Some(token),
        }
    }
}
