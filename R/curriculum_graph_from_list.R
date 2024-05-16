#'Create Curriculum Graph Object
#'
#'Generates a curriculum graph from a node and edge list.
#'
#'@inheritParams blocking_factor
#'
#'@return A list that contains the following: \item{node_list}{A dataframe of
#'  course nodes containing their id, term, blocking factor (bf), delay
#'  factor (df), centrality (cf), and cruciality (sc)} \item{edge_list}{A
#'  dataframe with two columns 'from' and 'to' specifying directed edges
#'  starting at 'from' nodes directed towards 'to' nodes.} \item{network}{Igraph
#'  network object representing the curriculum graph} \item{sc_total}{Total
#'  structural complexity of the curriculum graph} \item{bf_total}{Total
#'  blocking factor of the curriculum graph} \item{df_total}{Total delay factor
#'  of the curriculum graph}
#'@author Daniel Krasnov
#'@references Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
#'  Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the Impact
#'  of Curricular Reforms and Pedagogical Innovations.” arXiv Preprint
#'  arXiv:1811.09676.
#' @examples
#' edge_list <- data.frame(from = c(1, 3), to = c(3, 4))
#'# courses in node list must be placed sequentially in term order to be properly displayed
#'node_list <-
#'data.frame(
#'  id = 1:4,
#'  label = c("MATH 100", "DATA 101", "MATH 101", "MATH 221"),
#'  term = c(1, 1, 2, 2)
#')
#'C <- curriculum_graph_from_list(node_list,edge_list)
#'plot_curriculum_graph(C)
#'@export
curriculum_graph_from_list <- function(node_list, edge_list) {

  if(!"id" %in% colnames(node_list)){
    stop("node_list must have an id column for each course")
  }
  if(!"term" %in% colnames(node_list)){
    stop("node_list must have a term column specifying which term each course is taken in")
  }

  obj <- list()
  class(obj) <- "curriculum_graph"

  bf_df <- blocking_factor(node_list, edge_list)
  bf <- bf_df$bynode

  node_list <- dplyr::left_join(node_list, bf, by = c("id" = "id"))

  df_df <- delay_factor(node_list,edge_list)
  df <- df_df$bynode

  node_list <- dplyr::left_join(node_list, df, by = c("id" = "id"))

  cf <- centrality_factor(node_list, edge_list)

  node_list <- dplyr::left_join(node_list, cf, by = c("id" = "id"))

  sc_df <- structural_complexity(node_list, edge_list)
  sc <- sc_df$bynode

  node_list <- dplyr::left_join(node_list, sc, by = c("id" = "id"))

  obj$node_list <- node_list
  obj$edge_list <- edge_list
  obj$network <- igraph::graph_from_data_frame(d = edge_list,
                                       vertices = node_list,
                                       directed = TRUE)

  obj$sc_total <- sc_df$total

  obj$bf_total <- bf_df$total

  obj$df_total <- df_df$total
  return(obj)
}
