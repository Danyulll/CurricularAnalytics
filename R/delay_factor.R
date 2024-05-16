#'Calculate delay factor
#'
#'A helper function for calculating the delay factor for each node and the total
#'delay factor of a curriculum graph.
#'
#'The delay factor of a course is the longest path the nodes finds itself on.
#'More formally the delay factor of a node \eqn{v_k} is given by
#'\deqn{d_c(v_k)=\underset{i,j,l,m}{max}\left\{\#\left(v_i
#'\overset{p_l}{\to} v_k \overset{p_m}{\to} v_j
#'\right)\right\}}
#'The delay factor of an entire curriculum graph \eqn{G_c} is defined as
#'\deqn{d(G_c)=\sum_{v_k \in V}d_c(v_k)}
#'@inheritParams blocking_factor
#'
#'@return A list that contains the following: \item{bynode}{A dataframe
#'  containing the delay factor of each node} \item{total}{The total delay
#'  factor of the curriculum graph}
#'@author Daniel Krasnov
#'@references Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
#'  Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the Impact
#'  of Curricular Reforms and Pedagogical Innovations.” arXiv Preprint
#'  arXiv:1811.09676.
#'@examples
#'edge_list <- data.frame(from = c(1, 3), to = c(3, 4))
#'node_list <-
#'data.frame(
#'  id = 1:4,
#'  label = c("MATH 100", "DATA 101", "MATH 101", "MATH 221"),
#'  term = c(1, 1, 2, 2)
#')
#'
#'df_list <- delay_factor(node_list,edge_list)
#'print(df_list)
#'# Output:
#'# $bynode
#'#   id df
#'# 2  1  3
#'# 3  2  1
#'# 4  3  3
#'# 5  4  3
#'# $total
#'# [1] 10
#'@export
delay_factor <- function(node_list, edge_list) {
  bynode <- data.frame(id = NA, df = NA)

  network <- igraph::graph_from_data_frame(d = edge_list,
                                           vertices = node_list,
                                           directed = TRUE)
  paths <- list()


  for (v in as.numeric(node_list$id)) {
    paths <-
      c(paths,
        igraph::all_simple_paths(network, from = v, mode = "out"))
  }

  for (v in as.numeric(node_list$id)) {
    max_length <- 0
    for (path in paths) {
      if (v %in% as.vector(path) &&
          max_length < length(as.vector(path))) {
        max_length <- length(as.vector(path))
      }
    }

    if (max_length == 0) {
      max_length <- 1
    }
    bynode <-
      rbind(bynode, data.frame(id = v, df = max_length))

  }

  bynode <- stats::na.omit(bynode)
  total <- sum(bynode$df)

  list(bynode = bynode, total = total)
}
