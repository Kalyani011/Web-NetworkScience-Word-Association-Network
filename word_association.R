library(igraph)
library(ggrepel)
library(ggraph)

# Reading word pairs graph from pajek file
g <- read_graph(file="WordPairs.txt",format="pajek")
# Removing edge direction from graph
g <- as.undirected(g)
# Removing self loops from graph
g <- simplify(g)

V(g)$deg <- degree(g)
# Part 1 Creating sub-graphs of target words
## Part 1.1 Selecting 3 target words and showing that they are cue words

# Reading the cue word indicator for each vertex stored in cue.txt
cue <- read.csv("cue.txt")
# Removing the information rows
cue <- cue[-c(1, 2, 3),]
# Adding cue value for each vertex in graph
V(g)$cue <- cue
# Getting cue words from word pairs graph
cue_words <- V(g)[V(g)$cue == 1]

words_temp <- cue_words[cue_words$deg > 100]
#### DO NOT EXCUTE THIS AGAIN
# Randomly selecting 3 target indices
# selected_indices <- c(3913, 3235, 4767)
# selected_indices <- sample(1:length(cue_words), 3)

selected_indices <- c(25, 54, 5)
# selected_indices <- sample(1:length(words_temp), 3)
#### DO NOT EXCUTE THIS AGAIN

selected_words <- words_temp[selected_indices]

### experimenting
# walk_rand <- random_walk(g, start=selected_words[1], steps=100, stuck = "return")

walk_rand <- c()
for (i in 1:100) {
  walk_rand <- c(walk_rand, random_walk(g, start=selected_words[1], steps=5, stuck = "return"))
}
walk_rand <- unique(walk_rand)
sub_graph1 <- induced_subgraph(g,walk_rand) 

sub_graph1 <- delete.edges(sub_graph1,which(E(sub_graph1)$weight < 0.05))
sub_graph1 <- delete.vertices(sub_graph1, which(degree(sub_graph1)==0))

sub1_comp <- components(sub_graph1)
scc <- which(sub1_comp$membership == which.max(sub1_comp$csize))
scc_nodes <- V(sub_graph1)[V(sub_graph1) %in% scc]

sub_graph1 <- induced_subgraph(sub_graph1, scc_nodes)

vertex_size <- 0.5 + degree(sub_graph1)
cex_size <- 0.4 + degree(sub_graph1)/40

par(mar=c(0,0,1,0)+.1)
plot(sub_graph1, 
      layout=layout_with_fr, 
      vertex.color="#ff1177",
      vertex.frame.color="gray",
      vertex.label.cex=cex_size,
      vertex.label.family="Helvetica", 
      vertex.label.color="black",
      vertex.label.dist=0.5,  
      vertex.size= vertex_size, 
      edge.width=0.2,
      edge.curved=0.2,
      main = paste("Tree"))



vertex_size <- 0.5 + degree(sub_graph1)/5
cex_size <- 0.3 + degree(sub_graph1)/10

ggraph(sub_graph1, layout = "fr") +
  geom_edge_link(start_cap = circle(1.5, "mm"),
                 end_cap = circle(1.5, "mm"),
                 edge_width = 0.2,
                 alpha = 0.2) +
  geom_node_point(aes(size = vertex_size),
                  alpha = 0.8,
                  colour = '#ff0077') +
  geom_node_text(
    aes(label = name),
    size = cex_size,
    fontface = "bold",
    repel = TRUE
  ) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"))

#### ego trial
# neighbors1 <- neighbors(g, selected_words[1])
 
# all_neighbors <- c()
# for (node in neighbors1$name) {
#   all_neighbors <- c(all_neighbors, neighbors(g, node))
# }
# 
# ego_nodes1_2 <- c(selected_words[1]$name, names(all_neighbors))
# ego_nodes1_2 <- unique(ego_nodes1_2)
# 
# ego_net1_2 <- induced_subgraph(g, ego_nodes1_2)
# sub_graph1 <- delete.edges(ego_net1_2, which(E(ego_net1_2)$weight < 0.1))
# sub_graph1 <- delete.vertices(sub_graph1, which(degree(sub_graph1)==0))
# 
# # subgraph1 <- delete.edges(ego_net1_2,which(E(ego_net1_2)$weight < 0.1))
# # subgraph1 <- delete.edges(ego_net1_2,which(E(ego_net1_2)$weight < mean(E(ego_net1_2)$weight)))
# # subgraph1 <- delete.vertices(subgraph1, which(degree(subgraph1)==0))
# 
# # 
# # # values set by trial and error
# vertex_size <- 0.6 + degree(sub_graph1)/15
# cex_size <- 0.6 + degree(sub_graph1)/30
# 
# # vertex_size <- 0.6 + degree(sub_graph1)/5
# # cex_size <- 0.6 + degree(sub_graph1)/30
# 
# ggraph(sub_graph1, layout = "fr") +
#   geom_edge_link(start_cap = circle(2.5, "mm"),
#                  end_cap = circle(2.5, "mm"),
#                  edge_width = 0.2,
#                  alpha = 0.2) +
#   geom_node_point(aes(size = vertex_size),
#                   alpha = 0.8,
#                   colour = '#ff0077') +
#   geom_node_text(
#     aes(label = name),
#     size = cex_size,
#     fontface = "bold",
#     repel = TRUE
#   ) +
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "white"))

 
# neighbors2 <- neighbors(g, selected_words[2])
# 
# all_neighbors <- c()
# for (node in neighbors2$name) {
#   all_neighbors <- c(all_neighbors, neighbors(g, node))
# }
# 
# ego_nodes2_2 <- c(selected_words[1]$name, names(all_neighbors))
# ego_nodes2_2 <- unique(ego_nodes2_2)
# 
# ego_net2_2 <- induced_subgraph(g, ego_nodes2_2)
# 
# subgraph2 <- delete.edges(ego_net2_2,which(E(ego_net2_2)$weight < mean(E(ego_net2_2)$weight)))
# subgraph2 <- delete.vertices(subgraph2, which(degree(subgraph2)==0))
# 
# vertex_size <- degree(subgraph2)
# cex_size <- degree(subgraph2)/10
# 
# ggraph(subgraph2, layout = "fr") +
#   
#   geom_edge_link(start_cap = circle(2.5, "mm"),
#                  end_cap = circle(2.5, "mm"),
#                  edge_width = 0.2,
#                  alpha = 0.2) +
#   geom_node_point(aes(size = vertex_size), 
#                   alpha = 0.8, 
#                   colour = "#188ac1") +
#   geom_node_text(
#     aes(label = name),
#     size = cex_size,
#     fontface = "bold",
#     repel = TRUE
#   ) +
#   
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "white"))



