#'Write curriculum graph object to JSON
#'
#'A function to save a curriculum graph object in a JSON file format.
#'
#'@param curriculum_graph A curriculum_graph object created with either
#'  [CurricularAnalytics::curriculum_graph_from_list()] or [CurricularAnalytics::curriculum_graph_from_json()]
#'@param file_path A path to the JSON file you want written. If the file does
#'  not exist in the specified directory one will be created.
#'
#'@author Daniel Krasnov
#'@references Hickman, Michael S. 2017."Development of a curriculum analysis and
#'  simulation library with applications in curricular analytics."
#' @examples
#' edge_list <- data.frame(from = c(1, 2, 3), to = c(4, 4, 4))
#'# courses in node list must be placed sequentially in term order to be properly displayed
#'node_list <-
#'data.frame(
#'  id = 1:4,
#'  label = c("MATH 100", "DATA 101", "MATH 101", "MATH 221"),
#'  term = c(1, 1, 2, 2)
#')
#'file_path <- "./test.json"
#'C <- curriculum_graph_from_list(node_list, edge_list)
#'#curriculum_graph_to_json(C, "./test.json")
#'@importFrom magrittr %>%
#'@export
curriculum_graph_to_json <- function(curriculum_graph, file_path) {
  terms <- max(curriculum_graph$node_list$term)

  nodes <- curriculum_graph$node_list
  nodes <- nodes[, c("id", "label")]
  edges <- curriculum_graph$edge_list

  merged_df <-
    dplyr::left_join(edges, nodes, by = c("from" = "id")) %>%
    dplyr::select(-from)
  colnames(merged_df)[2] <- "from"
  merged_df <-
    dplyr::left_join(merged_df, nodes, by = c("to" = "id")) %>%
    dplyr::select(-to)
  colnames(merged_df)[2] <- "to"

  courses = tibble::tibble(
    label = character(),
    term = integer(),
    prerequisites = list(),
    corequisites = list()
  )
  # add all courses with pre/coreqs
  for (i in 1:length(merged_df$to)) {
    # i <- 2
    temp <-
      subset(curriculum_graph$node_list, label == merged_df[i, ]$to)
    course1_term <-
      subset(curriculum_graph$node_list, label == merged_df[i, ]$from)$term
    course2_term <-
      subset(curriculum_graph$node_list, label == merged_df[i, ]$to)$term

    # If the course is in the list update its pre/coreq list
    if (temp$label %in% courses$label) {
      if (course1_term == course2_term) {
        courses[courses$label == temp$label,]$corequisites[[1]] <-
          list(
            courses[courses$label == temp$label]$corequisites,
            subset(curriculum_graph$node_list, label == merged_df[i, ]$from)$label
          )
      } else{
        courses[courses$label == temp$label,]$prerequisites[[1]] <-
          c(
            courses[courses$label == temp$label,]$prerequisites[[1]],
            subset(curriculum_graph$node_list, label == merged_df[i, ]$from)$label
          )
      }
    } else{
      row <-
        tibble::tibble(
          label = temp$label,
          term = temp$term,
          prerequisites = list(list()),
          corequisites = list(list())
        )

      prerequisites <- list()
      corequisites <- list()

      if (course1_term == course2_term) {
        corequisites <- list(merged_df[i, "from"])
        row$corequisites[[1]] <-
          corequisites
      } else{
        prerequisites <- list(merged_df[i, "from"])
        row$prerequisites[[1]] <-
          prerequisites
      }

      courses <- dplyr::add_row(courses, row)
    }
  }
  # add remaining courses with no pre/coreqs
  rest <-
    curriculum_graph$node_list[!(curriculum_graph$node_list$label %in% courses$label), ]
  row <-
    tibble::tibble(
      label = rest$label,
      term = rest$term,
      prerequisites = list(list()),
      corequisites = list(list())
    )
  courses <- dplyr::add_row(courses, row)

  json <- list(terms = terms,
               courses = courses)

  json_data <- jsonlite::toJSON(json)


  if (!grepl("\\.json$", file_path, ignore.case = TRUE)) {
    stop("Invalid file extension. File name must end with .json.")
  }

  directory_path <- dirname(file_path)

  if (!dir.exists(directory_path)) {
    dir.create(directory_path, recursive = TRUE)
  }

  jsonlite::write_json(json_data, file_path)
}
