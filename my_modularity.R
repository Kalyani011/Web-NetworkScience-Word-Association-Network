library(igraph)

my_mod <- function(g,c){
  g.c<- induced_subgraph(g, c, impl = "create_from_scratch")
  lc <-length(E(g.c))
  l <- length(E(g))
  kc <-sum(igraph::degree(g.c))
  
  mod <- lc/l - ((kc/(2*l)))^2
  return (mod)
}



# Now calculate the overall modularity, which is the sum of local modularity values calculated for each community

global_modularity <- function(g, comm_list){
  j<-length(comm_list)
  
  global_mod <-0
  
  for (i in 1:j){
    c<-unlist(comm_list[j])
  
    # call the moularity function  defined above
    mod <-my_mod(g,c)
    global_mod <- global_mod + mod
    
  }
  return (global_mod)
}


