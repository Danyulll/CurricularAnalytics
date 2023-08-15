#' Create Curriculum From JSON File
#'
#' Generates a curriculum graph from a json file.
#'
#' @param file A json file following the format produced by [CurricularAnalytics::curriculum_graph_to_json()]
#'
#' @return A list that contains the following: \item{node_list}{A dataframe of
#'  course nodes containg at least their id, term, blocking factor (bf), delay
#'  factor (df), centrality (cf), and cruciality (sc)} \item{edge_list}{A
#'  dataframe with two columns 'from' and 'to' specifying directed edges
#'  starting at 'from' nodes directed towards 'to' nodes.} \item{network}{Igraph
#'  network object representing the curriculum graph} \item{sc_total}{Total
#'  structural complexity of the curriculum graph} \item{bf_total}{Total
#'  blocking factor of the curriculum graph} \item{df_total}{Total delay factor
#'  of the curriculum graph}
#' @examples
#' # For an example of json file structuring see vignette
#' C <- curriculum_graph_from_json("./data/json_example.json")
#' plot(C)
#' @export
#'
#'
curriculum_graph_from_json <- function(file) {
  if (file.exists(file)) {
    file_extension <- tools::file_ext(file)
    if (file_extension != "json") {
      stop("File exists but is not a JSON file.")
    }
  } else {
    stop("File does not exist or cannot be found.")
  }

  file <- jsonlite::fromJSON(file)
  json_data <- jsonlite::fromJSON(file)
  df <- json_data$courses
  node_list <-
    data.frame(id = as.numeric(rownames(df)), label = df$label, term = df$term)

  edge_list <- data.frame(from = NA, to = NA)

  for (node in df$label) {
    str <- subset(df, label == node)$prerequisites[[1]]
    course_code <- ""
    if (!is.null(str)) {
      course_code <- str
    }

    from <- rownames(subset(df, label == node))
    to <- rownames(subset(df, label %in% course_code))
    if (length(to) > 1) {
      for (id in to) {
        edge_list <- rbind(edge_list, data.frame(from = id, to = from))
      }
    } else {
      edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
    }
  }
  edge_list <- stats::na.omit(edge_list)
  return(curriculum_graph_from_list(node_list,edge_list))
}
