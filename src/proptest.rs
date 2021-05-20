// Implement strategy for generating sequences of actions. This offers not only
// generating a graph, i.e., the result value of applying the operations, but
// also the path to it, so it is useful in testing storages for edge cases and
// discovering the circumstances that lead to it. It might be actually even
// simpler to implement proptest-like reducing by discarding operations (and
// adding them back on `complicate`). Moreover, it allows to test the
// correctness of the storages by applying the same sequence of operations on a
// petgraph graph and then checking isomorphism.
