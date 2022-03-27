source("my_modularity.R")
source("cluster_measurements.R")
source("CPM.R")

# Using random walk sampling to create sub-graphs
get_subgraph <- function(start_node, steps=10, runs=50, threshold=0.05) {
  # Large number of small walks
  walk_rand <- c()
  for (i in 1:runs) {
    walk_rand <- c(walk_rand, random_walk(g, start=start_node, steps=steps, stuck = "return"))
  }
  # Removing duplicate nodes
  walk_rand <- unique(walk_rand)
  # Creating sub-graph with nodes visited in the random walk
  sub_graph <- induced_subgraph(g, walk_rand)
  # Deleting edges with weight less than threshold
  sub_graph <- delete.edges(sub_graph, which(E(sub_graph)$weight < threshold))
  # Deleting singleton nodes in graph after removing low weighted edges
  sub_graph <- delete.vertices(sub_graph, which(degree(sub_graph)==0))
  # Getting components of sub_graph
  sub_comp <- components(sub_graph)
  # Getting the strongly connected component
  scc <- which(sub_comp$membership == which.max(sub_comp$csize))
  # Getting nodes from strongly connected component of the sub_graph
  scc_nodes <- V(sub_graph)[V(sub_graph) %in% scc]
  # Inducing sub graph after removing multiple unconnected smaller components,
  # to get a connected sub_graph
  sub_graph <- induced_subgraph(sub_graph, scc_nodes)
  return(sub_graph)
}

display_community_data <- function(graph, community) {
  intra.cluster.measurements <- cluster_measurements(graph, community)
  knitr::kable(intra.cluster.measurements[,c(1,2,4)], align = 'l', 
               caption = paste0(algorithm(community))) %>% 
    kable_styling(full_width = F)
}

algorithm_comparison <- function(graph, communities_data) {
  cluster.measurement <- cluster_measurements(graph, communities_data[[1]])
  communities_results <- data.frame(Algorithm=algorithm(communities_data[[1]]),
                                    Size=length(communities_data[[1]]),
                                    Modularity=round(modularity(communities_data[[1]]), 3),
                                    no.clusters.higher.density=length(which(
                                      cluster.measurement$density >
                                        cluster.measurement$density[cluster.measurement$cluster==-1])))
  for (i in 2:(length(communities_data))) {
    cluster.measurement <- cluster_measurements(graph, communities_data[[i]])
    communities_results <- rbind(communities_results, data.frame(Algorithm=algorithm(communities_data[[i]]), 
                                                                 Size=length(communities_data[[i]]), 
                                                                 Modularity=round(modularity(communities_data[[i]]), 3),
                                                                 no.clusters.higher.density=length(which(
                                                                   cluster.measurement$density > 
                                                                     cluster.measurement$density[cluster.measurement$cluster==-1]))) ) 
  }
  # print(communities_data$cliq_percolation)
  # n <- length(communities_data$cliq_percolation)
  # cluster_densities_cp <- 0
  # for (x in 1:n) {
  #   cluster_densities_cp <- c(cluster_densities_cp, subgraph_density(graph, communities1$cliq_percolation[[x]]))
  # }
  # 
  # communities_results <- rbind(communities_results, 
  #                              data.frame(Algorithm="clique percolation", 
  #                                        Size=length(communities_data[[length(communities_data)]]), 
  #                                        Modularity=0,
  #                                        no.clusters.higher.density=length(which(
  #                                        cluster_densities_cp > 
  #                  cluster.measurement$density[cluster.measurement$cluster==-1])))) 
  
  return(communities_results)
} 

selected_community_results <- function(sub_graph, communities, eb=FALSE) {
  
  best_algo <- sub_graph$Algorithm[which.max(sub_graph$Modularity)]
  selected_community <- NULL
  if(eb) {
    selected_community <- communities$eb
  }else{
    for (i in 1:(length(communities))) {
      
      if(algorithm(communities[[i]])==best_algo) {
        selected_community <- communities[[i]]
      }
    }  
  }
  clusters <- communities(selected_community)
  for (x in 1:length(clusters)) {
    clusters[x] <- toString(clusters[[x]])
  }
  community_df <- data.frame(number=1:length(selected_community), 
                             cluster=clusters,
                             size=as.data.frame(sizes(selected_community))$Freq)  
  return(list(community_df=community_df, selected_community=selected_community))  
}

