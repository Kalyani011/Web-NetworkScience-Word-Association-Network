library(igraph)
library(ggrepel)
library(ggraph)

# Reading word pairs graph from pajek file
g <- read_graph(file="WordPairs.txt",format="pajek")
# Removing edge direction from graph
g <- as.undirected(g)
# Removing self loops from graph
g <- simplify(g)
# Adding degree column for each node
V(g)$deg <- degree(g)

# PART 1: Creating sub-graphs of target words
## Part 1.1 Selecting 3 target words and showing that they are cue words

# Reading the cue word indicator for each vertex stored in cue.txt
cue <- read.csv("cue.txt")
# Removing the information rows
cue <- cue[-c(1, 2, 3),]
# Adding cue value for each vertex in graph
V(g)$cue <- cue
# Getting cue words from word pairs graph
cue_words <- V(g)[V(g)$cue == 1]
# Selecting cue words with degrees higher than 100
words_temp <- cue_words[cue_words$deg > 100]

#### DO NOT EXCUTE THIS AGAIN
# Randomly selecting 3 target indices from list of 
# cue word nodes with degree greater than 100

# selected_indices <- sample(1:length(words_temp), 3)
selected_indices <- c(25, 54, 5)
#### DO NOT EXCUTE THIS AGAIN

# Getting the target word nodes for selected indices
selected_words <- words_temp[selected_indices]

## Part 1.2: creating the sub graph for selected words

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
  # Deleting edges with weight less than 0.05 <<<<<------- try experimenting here 
  # or explain why 0.05
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

set.seed(3233)

# Generating sub_graphs
sub_graph1 <- get_subgraph(selected_words[1], threshold=0.02, steps=10, runs=30)
print(paste("Graph 1", vcount(sub_graph1)))
sub_graph2 <- get_subgraph(selected_words[2], threshold=0.05, steps=8, runs=45)
print(paste("Graph 2", vcount(sub_graph2)))
sub_graph3 <- get_subgraph(selected_words[3], threshold=0.01, steps=8, runs=40)
print(paste("Graph 3", vcount(sub_graph3)))

# Function to plot sub-graph
plot_subgraph <- function(sub_graph, selected_word, colours=c('#0077b2')) {
  vertex_size <- 0.5 + degree(sub_graph)/5
  cex_size <- 0.5 + degree(sub_graph)/5
  V(sub_graph)$deg <- degree(sub_graph)
  sub_mean <- mean(table(degree(sub_graph)))
  ggraph(sub_graph, layout = "fr") +
    geom_edge_link(start_cap = circle(1.5, "mm"),
                   end_cap = circle(1.5, "mm"),
                   edge_width = 0.2,
                   alpha = 0.2) +
    geom_node_point(aes(size = vertex_size),
                    alpha = 0.8,
                    colour = colours) +
    geom_node_text(
      aes(label = ifelse(deg > sub_mean, as.character(name), NA_character_)),
      size = cex_size,
      fontface = "bold",
      repel = TRUE
    ) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"))
}

plot_subgraph(sub_graph1, selected_words[1]$name)
plot_subgraph(sub_graph2, selected_words[2]$name)
plot_subgraph(sub_graph3, selected_words[3]$name)

# plot_subgraph <- function(sub_graph, selected_word) {
#   vertex_size <- degree(sub_graph)
#   cex_size <- 0.4 + degree(sub_graph)/45
#   
#   par(mar=c(0,0,1,0)+.1)
#   plot(sub_graph, 
#        layout=layout_with_fr, 
#        vertex.color="#ff1177",
#        vertex.frame.color="gray",
#        vertex.label.cex=cex_size,
#        vertex.label.family="Helvetica", 
#        vertex.label.color="black",
#        vertex.label.dist=0.5,  
#        vertex.size= vertex_size, 
#        edge.width=0.2,
#        edge.curved=0.2,
#        main = paste(selected_word))
# }

# PART 2: Community Detection
eb1.community <- edge.betweenness.community(sub_graph1)

colours <- c('#7fc97f', '#beaed4','#fdc086','#ffff99',
             '#386cb0','#f0027f','#bf5b17','#ff7f00',
             '#cab2d6','#6a3d9a','#ffff99','#b15928',
             '#ff0077','#45ed5f','#66ffbb','#03f03f')

add.alpha <- function(cols, alpha) rgb(t(col2rgb(cols)/255), alpha = alpha)
colours<-add.alpha(colours, 0.7)

plot_subgraph(sub_graph1, selected_words[1]$name, colours = colours[eb1.community$membership])

fg.community<- cluster_fast_greedy(sub_graph1)
louvain.community<-cluster_louvain(sub_graph1)
wt <- cluster_walktrap(sub_graph1)
labprop <- cluster_label_prop(sub_graph1)

source("CPM.R")

k <- 4
clique.com <- clique.community(sub_graph1,k)


eb2.community <- edge.betweenness.community(sub_graph2)
fg2.community<- cluster_fast_greedy(sub_graph2)
louvain2.community<-cluster_louvain(sub_graph2)
wt2 <- cluster_walktrap(sub_graph2)
labprop2 <- cluster_label_prop(sub_graph2)

plot_subgraph(sub_graph2, selected_words[2]$name, colours = colours[eb2.community$membership])
plot_subgraph(sub_graph2, selected_words[2]$name, colours = colours[fg2.community$membership])
plot_subgraph(sub_graph2, selected_words[2]$name, colours = colours[louvain2.community$membership])


eb3.community <- edge.betweenness.community(sub_graph3)
print(paste("edge betweeness", length(eb3.community)))
fg3.community<- cluster_fast_greedy(sub_graph3)
print(paste("fast greedy", length(fg3.community)))
louvain3.community<-cluster_louvain(sub_graph3)
print(paste("louvain", length(louvain3.community)))
wt3 <- cluster_walktrap(sub_graph3)
print(paste("walktrap", length(wt3)))
labprop3 <- cluster_label_prop(sub_graph3)
print(paste("label prop", length(labprop3)))

plot_subgraph(sub_graph3, selected_words[3]$name, colours = colours[eb3.community$membership])
plot_subgraph(sub_graph3, selected_words[3]$name, colours = colours[fg3.community$membership])
plot_subgraph(sub_graph3, selected_words[3]$name, colours = colours[louvain3.community$membership])
