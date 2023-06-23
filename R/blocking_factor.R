blocking_factor <- function(node_list,edge_list) {
  #' Blocking factor
  #'
  #' Calculates the blocking factor for each node and the total blocking factor of the curriculum graph. The blocking factor of a node v is the number of nodes reachable from v. The function returns a list where bynode holds the blocking factor for each node and total holds the blocking factor for the graph.
  #'
  #' @param edge_list data frame containing the edge list of the graph.
  #' @param node_list data frame containing the node list of the graph.

  bynode <- data.frame(id = NA, bf = NA)

  network <- graph_from_data_frame(d = edge_list,
                                   vertices = node_list,
                                   directed = TRUE)
  paths <- list()

  for (v in as.numeric(node_list$id)) {
    paths <- c(paths, all_simple_paths(network, from = v, mode = "out"))
  }


  for (v in as.numeric(node_list$id)) {
    nodes_reachable <- c()
    for (path in paths) {
      curr_path <- as.vector(path)
      if (v %in% curr_path[1]) {
        nodes_reachable <- c(nodes_reachable, curr_path)
      }
    }

    nodes_reachable <- unique(nodes_reachable)
    nodes_reachable <- nodes_reachable[nodes_reachable != v]

    bynode <-
      rbind(bynode, data.frame(id = v, bf = length(nodes_reachable)))
  }

  bynode <- na.omit(bynode)

  list(bynode = bynode, total = sum(bynode$bf))

}
