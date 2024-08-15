Returns the out or in degree of the given vertex, depending on the edge
direction argument.

In undirected graphs the direction is ignored and the returned value is the same
as from [degree_undirected](Self::degree_undirected).

# Panics

Panics if the vertex does not exist.
