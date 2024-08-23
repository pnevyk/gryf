Returns the highest edge ID currently in use.

This might be useful in algorithms that use arrays. Before using this, check
whether [`CompactIdMap`](crate::core::id::CompactIdMap) returned by
[`edge_id_map`](crate::core::EdgeSet::edge_id_map) doesn't satisfy your needs
first.
