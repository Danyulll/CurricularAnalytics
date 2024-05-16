#'Calculate centrality
#'
#'A helper function for calculating the centrality for each node.
#'
#'A course is considered central if it has many requisite edges flowing in and
#'out of the node. More formally it is the number of long paths that include the
#'node. That is, consider a curriculum graph \eqn{G_c} and a vertex \eqn{v_i}. A
#'long path is a path that satisfies the following conditions:
#'* \eqn{v_i,v_j,v_k} are distinct
#'* \eqn{v_j \to v_i \to v_k}
#'* \eqn{v_j} is a source node (in-degree zero)
#'* \eqn{v_k} is a sink node (out-degree zero)
#'
#'Let \eqn{P_{v_i}=\{p_1,p_2,\dots\}} denote the set of all paths defined as
#'above. Then the centrality of a node \eqn{v_i} is given by
#'\deqn{q(v_i)=\sum^{|P_{v_i}|}_{l=1}\#(p_l)}
#'More plainly this is the number of paths containing \eqn{v_i} of at least length 3 where \eqn{v_i} is neither a source nor sink node.
#'@inheritParams blocking_factor
#'@return A dataframe containing the centrality of each node
#'@author Daniel Krasnov
#'@references Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
#'  Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the Impact
#'  of Curricular Reforms and Pedagogical Innovations.” arXiv Preprint
#'  arXiv:1811.09676.
#' @examples
#'edge_list <- data.frame(from = c(1, 3), to = c(3, 4))
#'node_list <-
#'data.frame(
#'  id = 1:4,
#'  label = c("MATH 100", "DATA 101", "MATH 101", "MATH 221"),
#'  term = c(1, 1, 2, 2)
#')
#'
#'cf_df <- centrality_factor(node_list,edge_list)
#'print(cf_df)
#'# Output:
#'#   id cf
#'#1  1  0
#'#2  2  0
#'#3  3  3
#'#4  4  0
#'@export
centrality_factor <- function(node_list,edge_list) {
  bynode <-
    data.frame(id = as.numeric(node_list$id), cf = rep(0, length(node_list$id)))

  network <- igraph::graph_from_data_frame(d = edge_list,
                                   vertices = node_list,
                                   directed = TRUE)
  paths <- list()

  for (v in as.numeric(node_list$id)) {
    paths <- c(paths, igraph::all_simple_paths(network, from = v, mode = "out"))
  }

  for (path in paths) {
    curr_path <- as.vector(path)
    if (length(curr_path) >= 3) {
      long_path_nodes <- curr_path[c(-1, -length(curr_path))]

      for (node in long_path_nodes) {
        bynode[node, c("cf")] <- bynode[node, c("cf")] + length(curr_path)
      }
    }
  }

  # bynode$id <- as.character(bynode$id)
  bynode
}
