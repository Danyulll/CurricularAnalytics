#'Calculate blocking factor
#'
#'A helper function for calculating the blocking factor for each node and the
#'total blocking factor of a curriculum graph.
#'
#'Blocking quantifies when a failing a course would result in being blocked from registering for future courses. More formally the blocking factor of a node \eqn{v_i} is defined as
#'\deqn{b_c(v_i) = \sum_{v_j \in V} I(v_i,v_j)} where \eqn{I} is the indicator
#'function: \deqn{=\begin{cases}1, & \text{if } v_i \to v_j \\ 0, &
#'\text{if }v_i \not\to v_j\end{cases}}
#'The blocking factor for an entire curriculum graph \eqn{G_c} is defined as
#'\deqn{b(G_c)=\sum_{v_i \in V} b_c(v_i)}
#'@param node_list Dataframe with an 'id' column for each node and a
#'  'term' column specifying which term the course is to be taken in.
#'@param edge_list Dataframe with two columns 'from' and 'to' specifying
#'  directed edges starting at 'from' nodes directed towards 'to' nodes. Entries
#'  must use node ids from `node_list`.
#'@return A list that contains the following: \item{bynode}{A dataframe
#'  containing the blocking factor of each node} \item{total}{The total blocking
#'  factor of the curriculum graph}
#'@author Daniel Krasnov
#'@references Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
#'  Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the Impact
#'  of Curricular Reforms and Pedagogical Innovations.” arXiv Preprint
#'  arXiv:1811.09676.
#' @examples
#' edge_list <- data.frame(from = c(1, 3), to = c(3, 4))
#' node_list <-
#'data.frame(
#'id = 1:4,
#'label = c("MATH 100", "DATA 101", "MATH 101", "MATH 221"),
#'term = c(1, 1, 2, 2)
#')
#' bf_list <- blocking_factor(node_list,edge_list)
#' print(bf_list)
#' # Output:
#' # $bynode
#'# id bf
#'# 2  1  2
#'# 3  2  0
#'# 4  3  1
#'# 5  4  0
#'# $total
#'# [1] 3
#'@export
blocking_factor <- function(node_list,edge_list) {

  bynode <- data.frame(id = NA, bf = NA)

  network <- igraph::graph_from_data_frame(d = edge_list,
                                           vertices = node_list,
                                           directed = TRUE)
  paths <- list()

  for (v in as.numeric(node_list$id)) {
    paths <- c(paths, igraph::all_simple_paths(network, from = v, mode = "out"))
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

  bynode <- stats::na.omit(bynode)

  list(bynode = bynode, total = sum(bynode$bf))

}
