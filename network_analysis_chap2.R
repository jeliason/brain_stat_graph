library(sand)
data(karate)

hist(degree(karate))

hist(graph.strength(karate))

library(igraph)
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
                   4-7, 5-6, 6-7)
V(g)
E(g)
str(g)
?str
plot(g)
?plot
dg <- graph.formula(1-+2, 1-+3, 2++3)
plot(dg)

dg <- graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)
str(dg)
plot(dg)
# alternatively
V(dg)$name <- c("Sam", "Mary", "Tom")

E(dg)
get.adjacency(g)

h <- induced.subgraph(g, 1:5)
h
# alternatively
h <- g - vertices(c(6,7))
h

h <- h + vertices(c(6,7))
h
g <- h + edges(c(4,6),c(4,7),c(5,6),c(6,7))
g

h1 <- h
h2 <- graph.formula(4-6, 4-7, 5-6, 6-7)
g <- graph.union(h1,h2)
g
plot(g)

V(dg)$name
V(dg)$gender <- c("M","F","M")
V(dg)$color <- "red"
plot(dg)

is.weighted(g)

wg <- g
plot(wg)
V(wg)$color <- "blue"
plot(wg)
?ecount
E(wg)$weight <- runif(ecount(wg))
is.weighted(wg)
lapply(E(wg), FUN=function(x){x$weight <- NULL})
# how to remove an attribute from an object?
is.weighted(wg)
E(wg)$weight
E(wg)[3:5]

g$name <- "Toy Graph"
g

library(sand)
elist.lazega
v.attr.lazega
g.lazega <- graph.data.frame(elist.lazega, 
                             directed="FALSE", 
                             vertices=v.attr.lazega)
g.lazega$name <- "Lazega Lawyers"

vcount(g.lazega)
ecount(g.lazega)

list.vertex.attributes(g.lazega)

is.simple(g)

g
mg <- g + edge(3,2)
is.simple(mg)

E(mg)$weight <- 1
wg2 <- simplify(mg)
is.simple(wg2)
E(wg2)$weight

g
neighbors(g,5)
degree(g)

degree(dg, mode="in")
degree(dg, mode="out")

# walk -> alternating sequence of vertices and edges
# indicating movement on graph
# trail (path) -> no repeating edge (vertex) 
# circuit -> same beginning and end
# cycle -> at least length 3, same beginning and end,
# but every other vertex different
# reachable(u,v) -> exists walk from u -> v
# connected - every vertex is reachable from every other
# component - all vertices are connected, but not connected
# to anything in the supergraph

is.connected(g)
clusters(g)

# weakly connected - underlying graph is connected
# strongly connected - reachable by a directed walk

is.connected(dg, mode="weak")
is.connected(dg, mode="strong")
# distance - shortest path
# diameter - longest distance
diameter(g, weights=NA)

# complete  - all vertices are joined
g.full <- graph.full(7)
# regular - every vertex has same degree
g.ring <- graph.ring(7)
# tree - no cycles(each vertex, excl leafs,  is ancestor of two)
g.tree <- graph.tree(7, children=2, mode="undirected")
# k-star - one root, k leaves
g.star <- graph.star(7, mode="undirected")
par(mfrow=c(2, 2))
plot(g.full)
plot(g.ring)
plot(g.tree)
plot(g.star)

is.dag(dg)

# bipartite - vertices in  two disjoint sets,
# each edge has an endpoint in either set
g.bip <- graph.formula(actor1:actor2:actor3,
                       movie1:movie2, actor1:actor2 - movie1,
                       actor2:actor3 - movie2)
V(g.bip)$type <- grepl("^movie", V(g.bip)$name)

g.bip
plot(g.bip)

# projection - "connections" within vertex subsets
# based on adjacencies with common vertices in other subset
proj <- bipartite.projection(g.bip)
proj[1]
proj[2]
