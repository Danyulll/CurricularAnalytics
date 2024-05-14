#' Create Curriculum From CSV File
#'
#' Generates a curriculum graph from a csv file.
#'
#'@param filepath A csv file path with a table where each row is a course and the columns are as follows:
#'- id: an integer id for the course
#'- label: a string with the name of the course
#'- term: an integer specifying what term the course is to be taken
#'- requisites: a list of all pre- and co-requisite course ids of the form 1;2;3;...
#'@return Returns a curriculum graph object based on the read csv file.
#'@export
#'
#' @examples
#'# Have filepath point to a csv of the following form
#' #id	label	term	requisites
#' #1	MATH 100	1
#' #2	DATA 101	1
#' #3	MATH 101	2	1
#' #4	MATH 221	2	3
#' #5	STAT 230	3	3;2
#'filepath <-
#'system.file("extdata", "Example-Curriculum.csv", package = "CurricularAnalytics")
#'C <- curriculum_graph_from_csv(filepath)
#'plot_curriculum_graph(C)
#'
curriculum_graph_from_csv <- function(filepath) {
  if (file.exists(filepath)) {
    file_extension <- tools::file_ext(filepath)
    if (file_extension != "csv") {
      stop("File exists but is not a CSV file.")
    }
  } else {
    stop("File does not exist or cannot be found.")
  }

  df <- utils::read.csv(filepath)
  df[is.na(df)] <- ""
  node_list <-
    data.frame(id = df$id,
               label = df$label,
               term = df$term)

  edge_list <- data.frame(from = NA, to = NA)
  for (node_id in node_list$id) {
    node_reqs <- subset(df, id == node_id)$requisites
    req_ids <- ""
    if (node_reqs != "") {
      req_ids <- as.numeric(strsplit(node_reqs, ";")[[1]])

      from <- rownames(subset(df, id == node_id))
      to <- rownames(subset(df, id %in% req_ids))
      if (length(to) > 1) {
        for (id in to) {
          edge_list <- rbind(edge_list, data.frame(from = id, to = from))
        }
      } else {
        edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
      }
    }
  }
  edge_list <- stats::na.omit(edge_list)
  return(curriculum_graph_from_list(node_list, edge_list))
}
