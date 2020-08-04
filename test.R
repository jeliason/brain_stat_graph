library(R.matlab)
library(ggplot2)
library(igraph)
library(haven)
df <- readMat("Coactivation_matrix.mat")
attributes(df)

df$Coactivation.matrix
dim(df$Coactivation.matrix)[[1]]
sum(df$Coactivation.matrix != 0)
df$Coord
max(df$Coactivation.matrix)

threshold <- 0.1
graph <- graph_from_adjacency_matrix(df$Coactivation.matrix, weighted = T, mode = "undirected", diag = F)
bin.graph <- graph_from_adjacency_matrix(df$Coactivation.matrix > threshold, mode = "lower", diag = F)

degrees <- degree(graph)
df <- data.frame(degrees)
colnames(df) <- c("Original")
bin.df <- data.frame(degree(bin.graph))
colnames(bin.df) <- c("Binarized")

ggplot(melt(cbind(df, bin.df), value.name = "Degrees"), aes(Degrees)) + 
  geom_histogram(aes(y=..density..), bins=30,
                 color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + 
  facet_wrap(.~variable)


commun <- cluster_louvain(graph)
bin.commun <- cluster_louvain(bin.graph)

fc <- cluster_fast_greedy(bin.graph)
plot(commun, graph)
l <- layout.kamada.kawai(bin.graph)

plot(bin.commun, bin.graph, layout=l)
plot_dendrogram(fc)

filepath = '~/Data/ADHD_200/ADHD200_CC200/'
list.files(filepath, pattern = ".*connectivity.*")

files = list.files(filepath, pattern = ".*connectivity.*")

sapply(matrices, function(x) x[[1]])

popular2data <- read_sav(file = "https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%202/popularity/SPSS/popular2.sav?raw=true")
popular2data <- select(popular2data, pupil, class, extrav, sex, texp, popular) # we select just the variables we will use
head(popular2data) # we have a look at the first 6 observations

interceptonlymodeltest <- brm(popular ~ 1 + (1 | class), 
                              data   = popular2data, 
                              warmup = 100, 
                              iter   = 200, 
                              chains = 2, 
                              inits  = "random",
                              cores  = 2)  #the cores function tells STAN to make use of 2 CPU cores simultaneously instead of just 1.

print(interceptonlymodeltest)
