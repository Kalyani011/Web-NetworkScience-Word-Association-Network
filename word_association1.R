# Part 1

## Task: Creation of Subgraph for Selected Target Words from the Word Association Network.

## Solution:

### Part 1.1. Data Setup:

# Loading necessary libraries
library(igraph)
library(ggrepel)
library(ggraph)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(scales)

source("my_modularity.R")
source("cluster_measurements.R")

# Reading word pairs graph from pajek file
word_graph <- read_graph(file="WordPairs.txt",format="pajek")
# Removing edge direction from graph
word_graph <- as.undirected(word_graph)
# Removing self loops from graph
word_graph <- simplify(word_graph)
# Adding degree column for each node
V(word_graph)$deg <- degree(word_graph)
# Re-scaling the weights of each edge to be in the range 0 to 1
E(word_graph)$weight<-rescale(E(word_graph)$weight)

## Selecting 3 target words and showing that they are cue words

# Reading the cue word indicator for each vertex stored in cue.txt
cue <- read.csv("cue.txt")
# Removing the information rows
cue <- cue[-c(1, 2, 3),]
# Adding cue value for each vertex in graph
V(word_graph)$cue <- cue
# Getting cue words from word pairs graph
cue_words <- V(word_graph)[V(word_graph)$cue == 1]
# Selecting cue words with degrees higher than 100
words_temp <- cue_words[cue_words$deg > 100]

# Following code was used to randomly select 3 target indices from list of 
# cue word nodes with degree greater than 100
# selected_indices <- sample(1:length(words_temp), 3)
# Resultant indices of the sample method
selected_indices <- c(25, 54, 5)

# Getting the target word nodes for selected indices
selected_words <- words_temp[selected_indices]

knitr::kable(selected_words$name, align = "c", booktabs=T, 
             col.names = c("Selected Target Words")) %>%
  kable_styling(full_width = F, latex_options = "HOLD_position")

### Part 1.3. Creating the Subgraphs for Selected Target Words:

# Using random walk sampling to create sub-graphs
get_subgraph <- function(start_node, steps=10, runs=50, threshold=0.005) {
  # Large number of small walks
  walk_rand <- c()
  for (i in 1:runs) {
    walk_rand <- c(walk_rand, 
        random_walk(word_graph, start=start_node, steps=steps, stuck="return"))
  }
  # Removing duplicate nodes
  walk_rand <- unique(walk_rand)
  # Creating sub-graph with nodes visited in the random walk
  sub_graph <- induced_subgraph(word_graph, walk_rand)
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

set.seed(100)
# Initializing list for holding the 3 sub-graphs
sub_graphs <- list()
# Generating sub_graph for target word TREE
sub_graph1 <- get_subgraph(selected_words[1], steps=8, runs=30, threshold=0.001)
sub_graphs$sub_graph1 <- sub_graph1
# Generating sub_graph for target word SOFT
sub_graph2 <- get_subgraph(selected_words[2], steps=8, runs=18, threshold=0.002)
sub_graphs$sub_graph2 <- sub_graph2
# Generating sub_graph for target word CHILD
sub_graph3 <- get_subgraph(selected_words[3], steps=6, runs=20, threshold=0.000)
sub_graphs$sub_graph3 <- sub_graph3

display_subgraph_info <- function(sub_graphs, target_words) {
  # Calculating and saving the average degree for all sub-graphs by taking mean
  # of degree function applied on sub-graphs
  avg_degree = round(as.vector(unlist(lapply(sub_graphs, 
                      function(x){mean(degree(x))}))), 2)
  # Calculating average path length using mean_distance function for all sub-graphs
  avg_path_len = round(as.vector(unlist(lapply(sub_graphs, mean_distance))), 2)
  # Calculating average clustering coefficient using transitivity function 
  # for all sub-graphs
  avg_clustering_ceoff = round(as.vector(unlist(lapply(sub_graphs, 
                      function(x){transitivity(x, type="localaverage")}))), 2)
  
  # Saving all information for each sub-graph into data-frame information
  information <- data.frame(Number=1:length(sub_graphs), 
                  Target_Word=target_words,
                  # using vcount to calculate total no. of nodes
                  No_of_Nodes=as.vector(unlist(lapply(sub_graphs, vcount))),
                  No_of_cue_words=as.vector(unlist(lapply(sub_graphs, 
                    function(x){length(which(V(x)$cue == 1))}))),
                  No_of_non_cue_words=as.vector(unlist(lapply(sub_graphs, 
                    function(x){length(which(V(x)$cue == 0))}))),
                  # using ecount to calculate total no. of edges
                  No_of_Edges=as.vector(unlist(lapply(sub_graphs, ecount))),
                  Average_Degree=avg_degree,
                  Average_Path_Length=avg_path_len,
                  # getting the maximum eccentricity to get the diameter
                  Diameter=as.vector(unlist(lapply(sub_graphs, 
                    function(x){max(eccentricity(x))}))),
                  Average_Clustering_Coefficient=avg_clustering_ceoff)

  # Plotting the information in a table
  knitr::kable(information, align="c", booktabs=T, 
               col.names = c("No.",
                             "Target Word",
                             "Nodes",
                             "Cue Nodes",
                             "Non-Cue Nodes",
                             "Edges",
                             "Average Degree",
                             "Average Path Length",
                             "Diameter",
                             "Average Clustering Coefficient")) %>%
    kable_styling(full_width = F, latex_options = "HOLD_position") %>%
    column_spec(1, width="0.5cm") %>%
    column_spec(c(3,4,6), width="1cm") %>%
    column_spec(c(2,5,8,7), width="1.25cm") %>%
    column_spec(10, width="2cm") %>%
    row_spec(0, bold=TRUE)
}

### Part 1.4. Subgraph Results:
display_subgraph_info(sub_graphs, target_words=selected_words$name)

# Part 2

## Task: Community Detection for Subgraphs Created in Part 1.

## Solution:

### Part 2.1 Analysis and Display Methods:

# Method to create a data-frame object to display results of three 
# community detection algorithms
# Note: The cluster_measurement method given in week 10 materials has been used [6] 

algorithm_comparison <- function(graph, communities_data) {
  # getting the cluster measurements for community object of edge betweenness
  cluster.measurement <- cluster_measurements(graph, communities_data[[1]])
  communities_results <- 
    # saving the community detection algorithm name obtained using algorithm function
    data.frame(Algorithm=algorithm(communities_data[[1]]),
    # saving the sizes of all communities            
    Size=length(communities_data[[1]]),
    # saving the global modularity of communities
    Modularity=round(modularity(communities_data[[1]]), 3),
    # checking and saving the number of communities with edge density higher than
    # the total edge density of the sub-graph to enable 
    # internal evaluation of the communities formed
    no.clusters.higher.density=length(which(cluster.measurement$density >                                        cluster.measurement$density[cluster.measurement$cluster==-1])))
  
  # Saving the same information as above to remaining algorithm results 
  # into the data-frame
  for (i in 2:(length(communities_data))) {
    cluster.measurement <- cluster_measurements(graph, communities_data[[i]])
    communities_results <- rbind(communities_results, data.frame(
            Algorithm=algorithm(communities_data[[i]]),
            Size=length(communities_data[[i]]), 
            Modularity=round(modularity(communities_data[[i]]), 3),
            no.clusters.higher.density=length(which(cluster.measurement$density > 
                cluster.measurement$density[cluster.measurement$cluster==-1])))) 
  }
  return(communities_results)
} 

# Method to select an algorithm from the three trialled on sub-graph and 
# create data-frame to display communities for the selected algorithm
selected_community_results <- function(sub_graph, communities, interpretations, eb=FALSE) {
  # Selecting the algorithm results with the maximum modularity
  selected_algo <- sub_graph$Algorithm[which.max(sub_graph$Modularity)]
  # Getting the communities for the selected algorithm from the communities list
  selected_community <- NULL
  # Optional condition to display results of edge betweenness algorithm
  if(eb) {
    selected_community <- communities$eb
  }else{
    for (i in 1:(length(communities))) {
      if(algorithm(communities[[i]])==selected_algo) {
        selected_community <- communities[[i]]
      }
    }  
  }
  # Getting the names of nodes in all communities
  clusters <- communities(selected_community)
  for (x in 1:length(clusters)) {
    # Converting the communities into strings
    clusters[x] <- tolower(toString(clusters[[x]]))
  }
  # Creating data-frame to display all communities, 
  # with their nodes, sizes and interpretation
  community_df <- data.frame(no=1:length(selected_community), 
                             cluster=clusters,
                             size=as.data.frame(sizes(selected_community))$Freq,
                             interpretation=interpretations)  
  return(list(community_df=community_df, selected_community=selected_community))  
}

apply_detection <- function(sub_graph, target) {
  ## sub-graph community detection
  communities <- list()
  eb.community <- cluster_edge_betweenness(sub_graph)
  communities$`eb` <- eb.community
  
  louvain.community <-cluster_louvain(sub_graph)
  communities$`louvain` <- louvain.community
  
  labprop.community <- cluster_label_prop(sub_graph)
  communities$`labprop` <- labprop.community
  
  # Getting the measurements for each algorithm's results
  algo_results <- algorithm_comparison(sub_graph, communities)
  
  return(list(algo_results=algo_results, communities=communities))
}

### Part 2.2 Applying Community Detection on Target Word TREE:

# Applying the three community detection algorithms
results1 <- apply_detection(sub_graph1, selected_words[1]$name)

# Displaying the results for each algorithm
knitr::kable(results1$algo_results, booktabs=TRUE, align = "c",
             caption = "Community Detection Algorithm Results for TREE Sub-Graph",
             col.names = c("Algorithm", "Size of Community", "Modularity",
                           "No. of communities with edge density higher than total edge density")) %>%
  kable_styling(full_width=T, latex_options = "HOLD_position") %>%
  row_spec(0, bold=TRUE)

community_results1 <-
  selected_community_results(results1$algo_results, results1$communities)

knitr::kable(community_results1$community_df,
             caption = "Selected Community Detection Algorithm Results for TREE Sub-Graph") %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") %>%
  column_spec(c(1, 3), width="0.5cm") %>%
  column_spec(2, width="10cm") %>%
  column_spec(4, width="5cm") %>%
  row_spec(0, bold=TRUE)


### Part 2.3 Applying Community Detection on Target Word SOFT:

# Applying the three community detection algorithms
results2 <- apply_detection(sub_graph2, selected_words[2]$name)

# Displaying the results for each algorithm
knitr::kable(results2$algo_results, booktabs=TRUE, align = "c",
             caption = "Community Detection Algorithm Results for SOFT Sub-Graph",
             col.names = c("Algorithm", "Size of Community", "Modularity",
                           "No. of communities with edge density higher than total edge density")) %>%
  kable_styling(full_width=T, latex_options = "HOLD_position") %>%
  row_spec(0, bold=TRUE)

community_results2 <-
  selected_community_results(results2$algo_results, results2$communities)

knitr::kable(community_results2$community_df,
             caption = "Selected Community Detection Algorithm Results for SOFT Sub-Graph") %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") %>%
  column_spec(c(1, 3), width="0.5cm") %>%
  column_spec(2, width="10cm") %>%
  column_spec(4, width="5cm") %>%
  row_spec(0, bold=TRUE)
### Part 2.4 Applying Community Detection on Target Word CHILD:

# Applying the three community detection algorithms
results3 <- apply_detection(sub_graph3, selected_words[3]$name)

# Displaying the results for each algorithm
knitr::kable(results3$algo_results, booktabs=TRUE, align = "c",
             caption = "Community Detection Algorithm Results for CHILD Sub-Graph",
             col.names = c("Algorithm", "Size of Community", "Modularity",
                           "No. of communities with edge density higher than total edge density")) %>%
  kable_styling(full_width=T, latex_options = "HOLD_position") %>%
  row_spec(0, bold=TRUE)

community_results3 <-
  selected_community_results(results3$algo_results, results3$communities)

knitr::kable(community_results3$community_df,
             caption = "Selected Community Detection Algorithm Results for CHILD Sub-Graph") %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") %>%
  column_spec(c(1, 3), width="0.5cm") %>%
  column_spec(2, width="10cm") %>%
  column_spec(4, width="5cm") %>%
  row_spec(0, bold=TRUE)

# Part 3

## Task: Visualizing Sub-Graphs with their Communities

## Solution:

### Part 3.1 Method for Plotting the Sub-Graph
# The first 10 colors in the palette are taken from Data Visualization Assignment 2,
# another 10 colors were added to cover all communities
custom_palette <- c('#9F69E1', '#59C7DB',  '#F1954D', '#E562A5','#6C7AEA',
                    '#59DCB2', '#CB464E',  '#EFD456', '#D065DF', '#4D8FD3',
                    '#601B52','#005437','#964B00','#deb887', '#333333')


# Adjusting alpha value of the colors
add.alpha <- function(cols, alpha) rgb(t(col2rgb(cols)/255), alpha = alpha)
colours <- add.alpha(custom_palette, 1)

# Function to plot sub-graph
plot_subgraph <- function(sub_graph, community_result, graph_number, selected_word, colours=c('#0077b2')) {
  
  V(sub_graph)$deg <- degree(sub_graph)
  central_nodes <- c()
  for(x in communities(community_result$selected_community)) {
    community <- V(sub_graph)[which(V(sub_graph)$name %in% as.vector(unlist(x)))]
    central_nodes <- c(central_nodes, community[which.max(community$deg)]) 
  }
  
  vertex_size <- 0.5 + degree(sub_graph)/6
  # cex_size <- 0.5 + degree(sub_graph)/5
  
  ggraph(sub_graph, 
         layout = layout_with_graphopt(sub_graph)
         ) +
    geom_edge_link(start_cap = circle(1.5, "mm"),
                   end_cap = circle(1.5, "mm"),
                   edge_width = 0.2,
                   alpha = 0.2) +
    geom_node_point(aes(size = vertex_size),
                    alpha = 0.8,
                    colour = colours) +
    geom_node_text(
      aes(label = ifelse(name %in% names(central_nodes), name, NA)),
      size = 3,
      fontface = "bold",
      repel = TRUE,
      color=colours,
      alpha=1
    ) +
    ggtitle(paste0("Target Word ", graph_number, ": ", selected_word)) + 
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(size=14, hjust = 0.5, face="bold"))
}


### Part 3.2 Sub-Graph for Target Word TREE:

plot_subgraph(sub_graph1,community_results1, 1, selected_words[1]$name, 
              colours = 
                colours[community_results1$selected_community$membership])

### Part 3.3 Sub-Graph for Target Word SOFT:
plot_subgraph(sub_graph2, community_results2, 2, selected_words[2]$name, 
              colours = 
                colours[community_results2$selected_community$membership])

### Part 3.4 Sub-Graph for Target Word CHILD:
plot_subgraph(sub_graph3, community_results3, 3, selected_words[3]$name, 
              colours = 
                colours[community_results3$selected_community$membership])




interpretations1 <- c(
  "Negative behaviours associated with children, along with relations that may be invovled in fights due to these behaviours",
  "Bad behaviours associated with children",
  "Addition words, perhaps relating to addition of child to the family",
  "Outdoor things, perhaps relating to playing outside",
  "Pronouns for children",
  "Stuff related to having fun",
  "Words related to time, perhaps delivery or birth of a child",
  "Feelings experienced while dealing with a child",
  "Hospital things relating to birth of a child",
  "Emotions experienced while handling a child",
  "Nouns related to Halloween, a kid's festival",
  "Words related to bathing",
  "LOREM IPSUM DOLOR SIT AMET",
  "LOREM IPSUM DOLOR SIT AMET",
  "LOREM IPSUM DOLOR SIT AMET")


# sub_graph1 <- get_subgraph(selected_words[1], steps=5, runs=40, threshold=0.005)
# sub_graphs$sub_graph1 <- sub_graph1

# Generating sub_graph for target word SOFT
# sub_graph2 <- get_subgraph(selected_words[2], steps=5, runs=35, threshold=0.005)
# sub_graphs$sub_graph2 <- sub_graph2
# # Generating sub_graph for target word CHILD
# sub_graph3 <- get_subgraph(selected_words[3], steps=5, runs=28, threshold=0.001)
# sub_graphs$sub_graph3 <- sub_graph3

# sub_graph1 <- get_subgraph(selected_words[1], steps=5, runs=38, threshold=0.002)
# sub_graphs$sub_graph1 <- sub_graph1
# # Generating sub_graph for target word SOFT
# sub_graph2 <- get_subgraph(selected_words[2], steps=5, runs=25, threshold=0.002)
# sub_graphs$sub_graph2 <- sub_graph2
# # Generating sub_graph for target word CHILD
# sub_graph3 <- get_subgraph(selected_words[3], steps=5, runs=35, threshold=0.005)
# sub_graphs$sub_graph3 <- sub_graph3

# sub_graph1 <- get_subgraph(selected_words[1], steps=5, runs=36, threshold=0.002)
# sub_graphs$sub_graph1 <- sub_graph1
# # Generating sub_graph for target word SOFT
# sub_graph2 <- get_subgraph(selected_words[2], steps=6, runs=24, threshold=0.002)
# sub_graphs$sub_graph2 <- sub_graph2
# # Generating sub_graph for target word CHILD
# sub_graph3 <- get_subgraph(selected_words[3], steps=5, runs=35, threshold=0.005)
# sub_graphs$sub_graph3 <- sub_graph3
