generate_coords <- function(curriculum_graph) {
  coords <- matrix(ncol = 2)

  old_term <- 1
  idx <- -1
  for (term in curriculum_graph$node_list$term) {
    if (old_term != term) {
      idx <- 0
      old_term <- term
    } else{
      idx <- idx + 1
    }
    coords <- rbind(coords, c(term, idx))
  }

  coords <- stats::na.omit(coords)
  return(coords)
}

#'Plot a curriculum graph
#'
#'Plots an interactable vizNetwork visualization of the Igraph network object
#'representing the curriculum graph.
#'
#'@param curriculum_graph  A curriculum_graph object created with either
#'  [CurricularAnalytics::curriculum_graph_from_list()] or
#'  [CurricularAnalytics::curriculum_graph_from_csv()]
#'@param width A string percentage for the width of the plot, default is "100%".
#'@param height An integer representing the number of pixels for the height, default is 500.
#'
#'@return No object is returned. Rather the graph is plotted according to the specified term order in node_list. Clicking on a node will reveal its label, structural complexity (sc), centrality (cf), blocking factor (bf), and delay factor (df)
#'@author Daniel Krasnov
#'@references Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
#'  Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the Impact
#'  of Curricular Reforms and Pedagogical Innovations.” arXiv Preprint
#'  arXiv:1811.09676.
#'@examples
#'edge_list <- data.frame(from = c(1, 3), to = c(3, 4))
# courses in node list must be placed sequentially in term order to be properly displayed
#'node_list <-
#'data.frame(
#'  id = 1:4,
#'  label = c("MATH 100", "DATA 101", "MATH 101", "MATH 221"),
#'  term = c(1, 1, 2, 2)
#')
#'C <- curriculum_graph_from_list(node_list,edge_list)
#'plot_curriculum_graph(C)
#'@export
plot_curriculum_graph <- function(curriculum_graph, width = "100%", height = 500) {
  curriculum_graph$node_list <-
    curriculum_graph$node_list[order(curriculum_graph$node_list$term),]
  coords <- generate_coords(curriculum_graph)
  visNetwork::visNetwork(
    curriculum_graph$node_list,
    curriculum_graph$edge_list,
    width = width,
    height = height,
    submain = paste(
      "Total Structural Complexity:",
      curriculum_graph$sc_total,
      "Total Blocking Factor:",
      curriculum_graph$bf_total,
      "Total Delay Factor:",
      curriculum_graph$df_total
    )
  ) |>
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visIgraphLayout(layout = "layout.norm", layoutMatrix = coords) |>
    visNetwork::visEvents(
      selectNode = "function(properties) {
      alert(' sc: ' + this.body.data.nodes.get(properties.nodes[0]).sc + ' cf: ' + this.body.data.nodes.get(properties.nodes[0]).cf + ' bf: ' + this.body.data.nodes.get(properties.nodes[0]).bf + ' df: ' + this.body.data.nodes.get(properties.nodes[0]).df);}"
    )
}
