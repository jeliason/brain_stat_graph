library(igraph)
N = 50
k = 4
m = 10000

# g <- k.regular.game(N, k)
# 
# l <- layout.kamada.kawai(g)
# plot(g, layout=l)

set.seed(1)

graphs <- replicate(m, k.regular.game(N, k), simplify = FALSE)


close <- lapply(graphs, closeness)
close.mean <- unlist(lapply(close, mean))
hist(close.mean, col="lightblue", xlim=c(min(close.mean),max(close.mean)),
     xlab="Mean closeness", ylab="Frequency", main="", breaks = 30)

