library(brms)
library(haven)

popular2data <- read_sav(file = "https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%202/popularity/SPSS/popular2.sav?raw=true")

popular2data <- select(popular2data, pupil, class, extrav, sex, texp, popular) # we select just the variables we will use
head(popular2data) # we have a look at the first 6 observations

ggplot(data  = popular2data,
       aes(x = extrav,
           y = popular))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
  theme_minimal()+
  labs(title = "Popularity vs. Extraversion")

ggplot(data  = popular2data,
       aes(x = extrav,
           y = popular))+
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  geom_smooth(method = lm,
              se     = FALSE, 
              col    = "black",
              size   = .5, 
              alpha  = .8)+ # to add regression line
  theme_minimal()+
  labs(title    = "Popularity vs. Extraversion",
       subtitle = "add regression line")

ggplot(data    = popular2data,
       aes(x   = extrav,
           y   = popular,
           col = class))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  labs(title    = "Popularity vs. Extraversion",
       subtitle = "add colours for different classes")

ggplot(data      = popular2data,
       aes(x     = extrav,
           y     = popular,
           col   = class,
           group = class))+ #to add the colours for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting purposes
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "Popularity vs. Extraversion",
       subtitle = "add colours for different classes and regression lines")


interceptonlymodeltest <- brm(popular ~ 1 + (1 | class), 
                              data   = popular2data, 
                              warmup = 100, 
                              iter   = 200, 
                              chains = 2, 
                              inits  = "random",
                              cores  = 2)  #the cores function tells STAN to make use of 2 CPU cores simultaneously instead of just 1.

summary(interceptonlymodeltest)

interceptonlymodel <- brm(popular ~ 1 + (1|class),  
                          data = popular2data, 
                          warmup = 1000, iter = 3000, 
                          cores = 2, chains = 2, 
                          seed = 123) #to run the model

summary(interceptonlymodel)
launch_shinystan(interceptonlymodel)


sample_econ_pa <- function() {
  sample_graph <- graph.empty(n = Nv, directed = F)
  eta <- 5.37
  gamma <- 1.81
  get_distance <- function(existing_node_idx, new_node_idx) {
    existing_node <- V(sample_graph)[existing_node_idx]
    new_node <- V(sample_graph)[new_node_idx]
    temp <- d %>%
      filter(group == paste(new_node, existing_node, sep = '-') || group == paste(existing_node, new_node, sep = '-'))
    temp$dist
  }
  normalizing_constant <- function(existing_nodes, new_node_idx) {
    dists <- sapply(existing_nodes, get_distance, new_node_idx = new_node)
    degrees <- sapply(existing_nodes, degree, g = sample_graph)
    probs <- (degrees^gamma)*(dists^eta)
    sum(probs)
  }
  calculate_prob <- function(existing_node_idx, new_node_idx, existing_nodes) {
    p <- (degree(sample_graph, existing_node_idx)^gamma)*(get_distance(existing_node_idx, new_node_idx)^eta)
    p <- p / normalizing_constant(existing_nodes, new_node_idx)
  }
  for(new_node_idx in seq_along(levels(d$region.x))) {
    new_node <- levels(d$region.x)[[new_node_idx]]
    existing_nodes <- V(sample_graph)
    print(new_node)
    set_vertex_attr(sample_graph, "region", new_node)
    for(existing_node in existing_nodes) {
      print(paste('Existing node:', existing_node))
      prob <- calculate_prob(existing_node_idx, new_node_idx, existing_nodes)
      if(rbinom(1,1,prob)) {
        add_edges(sample_graph, c(new_node_idx, existing_node))
      }
    }
  }
  return(sample_graph)
}

sample_econ_pa()