#'Calculate structural complexity
#'
#'A helper function for calculating the structural complexity for each node and
#'the total structural complexity of a curriculum graph.
#'
#'The structural complexity of a node \eqn{v_k} is defined as a linear
#'combination of the node's delay and blocking factors. More formally
#'\deqn{h(v_k) = d(v_k) + b(v_k)}. The structural complexity of an entire
#'curriculum graph \eqn{G_c} is defined as \deqn{h(G_c)=d(G_c)+b(G_c)=\sum_{v_k
#'\in V} \left(d_c(v_k) + b_c(v_k)\right)}
#'@inheritParams blocking_factor
#'
#'@return A list that contains the following: \item{bynode}{A dataframe
#'  containing the structural complexity of each node} \item{total}{The total
#'  structural complexity of the curriculum graph}
#'@author Daniel Krasnov
#'@references Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
#'  Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the Impact
#'  of Curricular Reforms and Pedagogical Innovations.” arXiv Preprint
#'  arXiv:1811.09676.
#'@examples
#'edge_list <- data.frame(from = c(1, 3), to = c(3, 4))
#'node_list <- data.frame(id = 1:4, label = c("MATH 100","DATA 101","MATH 101","MATH 221"), term = c(1,1,2,2))

#'sc_list <- structural_complexity(node_list,edge_list)
#'print(sc_list)
#'# Output:
#'# $bynode
#'#   id sc
#'# 1  1  5
#'# 2  2  1
#'# 3  3  4
#'# 4  4  3
#'# $total
#'# [1] 13
#'@export
structural_complexity <- function(node_list, edge_list) {
  bf_df <- blocking_factor(node_list, edge_list)
  bf <- bf_df$bynode$bf

  df_df <- delay_factor(node_list, edge_list)
  df <- df_df$bynode$df

  bynode <- data.frame(id = node_list$id, sc = (bf + df))

  list(bynode = bynode, total = sum(bynode$sc))

}
