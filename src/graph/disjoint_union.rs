use crate::index::IndexType;
use crate::marker::EdgeType;
use crate::traits::*;

// A set of graphs of the same family that form disconnected components.
pub struct DisjointUnion<G, const MAX: usize = 1024> {
    components: Vec<G>,
}

#[derive(Debug)]
pub enum DisjointUnionError {
    MaximumCardinalityExceeded,
}

impl<G, const MAX: usize> DisjointUnion<G, MAX> {
    pub fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn cardinality(&self) -> usize {
        self.components.len()
    }

    pub fn cardinality_max(&self) -> usize {
        MAX
    }

    pub fn components(&self) -> ComponentsIter<G, MAX> {
        ComponentsIter {
            iter: self.components.iter().enumerate(),
        }
    }

    pub fn components_mut(&mut self) -> ComponentsIterMut<G, MAX> {
        ComponentsIterMut {
            iter: self.components.iter_mut().enumerate(),
        }
    }
}

impl<G, const MAX: usize> DisjointUnion<G, MAX>
where
    G: Default,
{
    pub fn add(&mut self) -> Result<ComponentMut<G, MAX>, DisjointUnionError> {
        let index = self.components.len();

        if index == self.cardinality_max() {
            return Err(DisjointUnionError::MaximumCardinalityExceeded);
        }

        self.components.push(G::default());
        let component = self.components.last_mut().unwrap();
        Ok(ComponentMut::new(component, index))
    }
}

impl<G, const MAX: usize> Default for DisjointUnion<G, MAX> {
    fn default() -> Self {
        Self::new()
    }
}

impl<G, const MAX: usize> Guarantee for DisjointUnion<G, MAX>
where
    G: Guarantee,
{
    fn is_loop_free() -> bool {
        G::is_loop_free()
    }

    fn has_paths_only() -> bool {
        G::has_paths_only()
    }

    fn has_trees_only() -> bool {
        G::has_trees_only()
    }

    fn has_bipartite_only() -> bool {
        G::has_bipartite_only()
    }

    fn is_connected<Ty: EdgeType>() -> bool {
        false
    }
}

pub struct Component<'du, G, const MAX: usize> {
    component: &'du G,
    index: ComponentIndex<MAX>,
}

impl<'du, G, const MAX: usize> Component<'du, G, MAX> {
    fn new(component: &'du G, index: usize) -> Self {
        Self {
            component,
            index: ComponentIndex::new(index),
        }
    }
}

pub struct ComponentMut<'du, G, const MAX: usize> {
    component: &'du mut G,
    index: ComponentIndex<MAX>,
}

impl<'du, G, const MAX: usize> ComponentMut<'du, G, MAX> {
    fn new(component: &'du mut G, index: usize) -> Self {
        Self {
            component,
            index: ComponentIndex::new(index),
        }
    }
}

// TODO: impl Vertices, ... for DisjoinUnion, Component and ComponentMut

pub struct ComponentsIter<'du, G, const MAX: usize> {
    iter: std::iter::Enumerate<std::slice::Iter<'du, G>>,
}

impl<'du, G, const MAX: usize> Iterator for ComponentsIter<'du, G, MAX> {
    type Item = Component<'du, G, MAX>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(index, component)| Component::new(component, index))
    }
}

pub struct ComponentsIterMut<'du, G, const MAX: usize> {
    iter: std::iter::Enumerate<std::slice::IterMut<'du, G>>,
}

impl<'du, G, const MAX: usize> Iterator for ComponentsIterMut<'du, G, MAX> {
    type Item = ComponentMut<'du, G, MAX>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(index, component)| ComponentMut::new(component, index))
    }
}

// Component index is encoded into most significant bits of vertex/edge indices.
struct ComponentIndex<const MAX: usize> {
    component: usize,
}

impl<const MAX: usize> ComponentIndex<MAX> {
    pub const fn new(component: usize) -> Self {
        Self { component }
    }

    pub fn extract<I: IndexType>(index: I) -> Self {
        let bits = index.to_bits();
        let component = (bits >> Self::n_index_bits()) as usize;
        Self { component }
    }

    pub fn augment<I: IndexType>(&self, index: I) -> I {
        let mask = (self.component as u64) << Self::n_index_bits();
        I::from_bits(index.to_bits() | mask)
    }

    pub fn clean<I: IndexType>(&self, index: I) -> I {
        let mask = u64::MAX >> Self::n_mask_bits();
        I::from_bits(index.to_bits() & mask)
    }

    pub const fn to_usize(&self) -> usize {
        self.component
    }

    const fn n_mask_bits() -> u32 {
        let n_mask_bits = (MAX as u64).log2();
        if MAX.is_power_of_two() {
            n_mask_bits
        } else {
            n_mask_bits + 1
        }
    }

    const fn n_index_bits() -> u32 {
        u64::BITS - Self::n_mask_bits()
    }
}

// pub type Forest<V, E, S> = DisjointUnion<Tree<V, E, S>>;
// pub type LinearForest<V, E, S> = DisjointUnion<Path<V, E, S>>;

#[cfg(test)]
mod tests {
    use crate::VertexIndex;

    use super::*;

    #[test]
    fn component_index_extract_power_of_two() {
        let index = VertexIndex::from_bits(0x0080_0000_0000_002a);
        let component = ComponentIndex::<1024>::extract(index);
        assert_eq!(component.to_usize(), 2);
    }

    #[test]
    fn component_index_extract_not_power_of_two() {
        let index = VertexIndex::from_bits(0x0080_0000_0000_002a);
        let component = ComponentIndex::<1023>::extract(index);
        assert_eq!(component.to_usize(), 2);
    }

    #[test]
    fn component_index_augment_power_of_two() {
        let index = VertexIndex::new(42);
        let component = ComponentIndex::<1024>::new(2);

        let actual = component.augment(index);
        let expected = VertexIndex::from_bits(0x0080_0000_0000_002a);

        assert_eq!(actual, expected);
    }

    #[test]
    fn component_index_augment_not_power_of_two() {
        let index = VertexIndex::new(42);
        let component = ComponentIndex::<1023>::new(2);

        let actual = component.augment(index);
        let expected = VertexIndex::from_bits(0x0080_0000_0000_002a);

        assert_eq!(actual, expected);
    }

    #[test]
    fn component_index_clean_power_of_two() {
        let index = VertexIndex::from_bits(0x0080_0000_0000_002a);
        let component = ComponentIndex::<1024>::new(2);

        let actual = component.clean(index);
        let expected = VertexIndex::new(42);

        assert_eq!(actual, expected);
    }

    #[test]
    fn component_index_clean_not_power_of_two() {
        let index = VertexIndex::from_bits(0x0080_0000_0000_002a);
        let component = ComponentIndex::<1023>::new(2);

        let actual = component.clean(index);
        let expected = VertexIndex::new(42);

        assert_eq!(actual, expected);
    }
}
