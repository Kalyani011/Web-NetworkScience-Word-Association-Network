# Edge density of the clusters.
require(igraph)

subgraph_density <- function(graph, vertices) { 
  graph %>%
    induced_subgraph(vertices) %>% 
   edge_density()
   
}


#function to print out measurements 
cluster_measurements <- function(g,comm_object){
  
  # Overall Edge density in the graph.
  # Community densities should be much lower 
  
  intra.cluster.df <- data.frame(cluster = -1,
                                 size = vcount(g), 
                                 modularity=0, 
                                 density = edge_density(g))
  
  max_k<- length(comm_object)
  # Intra-cluster density for each community.
  for (i in 1:max_k) {
    d <- subgraph_density(g, comm_object[[i]])
    s<-length(comm_object[[i]])
    mod<- my_mod(g, comm_object[[i]])
    intra.cluster.df <- rbind(intra.cluster.df, data.frame(cluster = i, size=s, modularity = mod, density = d))
  }
  return (intra.cluster.df)
  
  
}