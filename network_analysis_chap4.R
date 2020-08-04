
library(sand)
par(mfrow=c(1,1))
data(karate)
hist(degree(karate), col="lightblue", xlim=c(0,20),
     xlab="Vertex Degree", ylab="Frequency", main="")

hist(graph.strength(karate), col="pink",
     xlab="Vertex Strength", ylab="Frequency", main="")

library(igraphdata)
data(yeast)
d.yeast <- degree(yeast)
hist(d.yeast,col="blue",
     xlab="Degree", ylab="Frequency",
     main="Degree Distribution")

dd.yeast <- degree.distribution(yeast)
d <- 1:max(d.yeast)-1
ind <- (dd.yeast != 0)
plot(d[ind], dd.yeast[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

a.nn.deg.yeast <- graph.knn(yeast,V(yeast))$knn
plot(d.yeast, a.nn.deg.yeast, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))

# degree is measure of centrality
# also closeness, betweenness and eigenvector centrality

# closeness - 1/sum_{u}(dist(v,u)) (vertex matters if close to others)
# betweenness - number of shortest paths that go through vertex
# eigenvalue - based on eigenvectors of adj matrix

A <- get.adjacency(karate, sparse=FALSE)
library(network)
g <- network::as.network.matrix(A)
library(sna)
sna::gplot.target(g, degree(g), main="Degree",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray")

# Can also use closeness(), betweenness() and evcent()$vector

# Similar measures exist for directed graphs - can define hubs and authorities as 
# eigenvector solutions of A^TA and AA^T

l <- layout.kamada.kawai(aidsblog)
par(mfrow=c(1,2))
plot(aidsblog, layout=l, main="Hubs", vertex.label="",
               vertex.size=10 * sqrt(hub.score(aidsblog)$vector))
plot(aidsblog, layout=l, main="Authorities",
               vertex.label="", vertex.size=10 *
               sqrt(authority.score(aidsblog)$vector))

# Edge characteristics
# Edge betweenness is # of shortest paths an edge is on
eb <- edge.betweenness(karate)
E(karate)[order(eb, decreasing=T)[1:3]]

# Can changes to a line graph to use other characteristics on edges

# Cohesion

table(sapply(cliques(karate), length))
# Cliques - subsets of V that are completely connected
# can also defined maximal cliques, which are not subsets of larger cliques

table(sapply(maximal.cliques(karate), length))

# clique number - size of largest clique
clique.number(yeast)
clique.number(karate)
# Weakenend notions of cliques, e.g. k-core, where is a subgraph in which all V 
# have degree at least k

cores <- graph.coreness(karate)
sna::gplot.target(g, cores, circ.lab = FALSE,
                  circ.col="skyblue", usearrows = FALSE,
                  vertex.col=cores, edge.col="darkgray")


# dyads and triads - in DG, consist of pairs (triples) of V and the various 'states'
# they can be in, ie, from mull subgraph to one in which there are mutual edges between all V
# they are a rudimentary kind of motif
# can take censuses of the different states of dyads/triads
detach("package:sna")
detach("package:network")

aidsblog <- simplify(aidsblog)
dyad.census(aidsblog)
# can see asymmetry in how blogs reference each other
triad.census((aidsblog))

# density can be defined depending on the subgraph in which you examine the density

ego.instr <- induced_subgraph(karate,
                              neighborhood(karate, 1, 1)[[1]])
ego.admin <- induced_subgraph(karate,
                              neighborhood(karate, 1, 34)[[1]])
edge_density(karate)
edge_density(ego.instr)
edge_density(ego.admin)

# We can see that in the immediate networks around the instructor and the admin, the density is much higher

# clustering coefficient/transitivity
# summarizes relative frequency with which connected triples close to form triangles (global clustering)

transitivity(karate)
transitivity(karate, "local", vids=c(1,34)) # can also define this locally

# does a graph separate into distinc subgraphs? This is associated with flow of
# information in the graph. Connected component is maximally connected - ie, every vertex is connected to every other vertex,
# and every other vertex in G is not connected to any vertex in the subgracomps <- decompose.graph(yeast)
comps <- decompose.graph(yeast)
table(sapply(comps,vcount))
# can see a giant component here
yeast.gc <- decompose.graph(yeast)[[1]]
# often giant components have smallworld property, ie, most V are not connected, but
# are very reachable from many (or all) other nodes. Additionally, clustering is relatively high (why?)
# Average path length scales log, rather than linear
average.path.length(yeast.gc)
diameter(yeast.gc)

transitivity(yeast.gc)

# vertex (edge) connectivity is the largest integer k such that G is k-vertex(edge)-connected.
# k-connected means that the removal of any k vertices (edges) leaves the graph connected.

vertex.connectivity(yeast.gc)
edge.connectivity(yeast.gc)
# So we can see removing even one V or E will destroy connectedness and break into separate components

# Such a well-placed vertex(edge) is called a cut vertex or articulation point
# Can provide a sense of where a network is vulnerable

yeast.cut.vertices <- articulation.points(yeast.gc)
length(yeast.cut.vertices)/vcount(yeast.gc)

# Menger's theorem -> k-vertex-connected iff all V pairs can be connected by k vertex-disjoint paths
# Relates robustness of graph to richness of distinct paths running through it

# for digraphs - distinction between strong and weak connectivity can be large
is.connected(aidsblog, mode=c("weak"))
is.connected(aidsblog, mode = c("strong"))

# Additionally, there is a strongly connected component but it is rather small
# (are able to get from one place to another by some path)
aidsblog.scc <- clusters(aidsblog, mode = c("strong"))
table(aidsblog.scc$csize)

# Partitioning (community detection)
# Cohesive subset is generally a subset that is well-connected internally, but relatively
# well-separated from remaining vertices

# Hierarchical clustering
# Define a cost function, eg, modularity
# either agglomerative (coarsening of partitions) -> {least costly merge} or divisive (refining) -> {least costly split}

# modularity -> fraction of edges that fall within groups - minus expected fraction of edges in a group if edges were distributed at random
# large values may indicate the partitioning capturs important group structure

kc <- cluster_fast_greedy(karate)
length(kc)
sizes(kc)
membership(kc)
par(mfrow=c(1,1))
plot(kc, karate)
# can plot den
plot_dendrogram(kc)

# Spectral partitioning
# G will consist of K connected components iff first K evals are 0, and the rest > 0 (and increasing)
# smallest eval is always zero, with evec = identity, if network is connected (why?)
# if we think G has nearly 2 components, would expect lambda_2 to be close to 0
# lambda_2 closely related to many measures of graph structure/connectivity
# Fiedler vector (value) - lambda_2, e_2. Can partition graph based on sign in e_2

k.lap <- laplacian_matrix(karate)
eig.anal <- eigen(k.lap)

plot(eig.anal$values, col="blue",
     ylab="Eigenvalues of Graph Laplacian")

f.vec <- eig.anal$vectors[, 33] # this is Fiedler vector, as can be seen in graph

faction <- get.vertex.attribute(karate, "Faction")
f.colors <- as.character(length(faction))
f.colors[faction == 1] <- "red"
f.colors[faction == 2] <- "cyan"
plot(f.vec, pch=16, xlab="Actor Number",
     ylab="Fiedler Vector Entry", col=f.colors)
abline(0, 0, lwd=2, col="lightgray")
# can see this exactly captures partitioning as indicated by faction labels
# can also partition based o matrix related to modularity, instead of the Laplacian

# Validation of partitioning
# can compare externally defined notion of class membership w/ classes from partitioning

func.class <- get.vertex.attribute(yeast.gc, "Class")
table(func.class)

yc <- cluster_fast_greedy(yeast.gc)
c.m <- membership(yc)

table(c.m, func.class, useNA=c("no"))
# Can see some of the classes are clustered strongly in certain communities, while
# others are not so strongly defined

# Assortativity coefficient (and mixing)
# in range [-1, 1] -> equal to 1 if only V of same category are linked, -1 if each edge connects V
# of different categories

assortativity.nominal(yeast, (V(yeast)$Class=="P")+1, directed = F)

# common use is summarizing degree-degree correlation of adjacent matrices
assortativity.degree(yeast)
