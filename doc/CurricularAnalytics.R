## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load library
library(CurricularAnalytics)

# Example creating C from JSON file:
C <- curriculum_graph_from_json("..\\data\\DS-Major-Max-2022-2023.json")

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
 disp <- data.frame(id = 1:5, label = c("MATH 100", "DATA 101", "MATH 101", "MATH 221", "STAT 230"), term = c(1,1,2,2,3), requisities = c(NA, NA, "1", "3", "3;2"))
kable(disp)

## -----------------------------------------------------------------------------
#  Example creating C from CSV file
C <- curriculum_graph_from_csv("..\\data\\DS-Major-Max-2023-2024.csv")

## -----------------------------------------------------------------------------
# Example creating C from node and edge lists:
node_list <- C$node_list[,1:3]
edge_list <- C$edge_list

# Printing example of what such lists look like
print(head(node_list))
print(head(edge_list))
C <- curriculum_graph_from_list(node_list, edge_list)

# Plot curriculum graph
plot(C)

## ----echo=FALSE---------------------------------------------------------------
#TODO should we include this DGP?
# library(stringi)
# library(stringr)

# get_course_codes <- function(str) {
#   if (grepl("and", str)) {
#     str <- trimws(unlist(strsplit(str, "and")))
#   }
# 
#   present <-
#     vapply(str, function(x)
#       grepl("one of", x), logical(1))
#   if (TRUE %in% present) {
#     idx <- which(present == TRUE)
#     course_codes <-
#       str_extract_all(str[idx], "\\b[A-Z]{4} \\d{3}\\b")[[1]]
#     str[idx] <- sample(course_codes, 1)
#   }
# 
#   present <-
#     vapply(str, function(x)
#       grepl("either", x), logical(1))
#   if (TRUE %in% present) {
#     idx <- which(present == TRUE)
#     course_codes <-
#       str_extract_all(str[idx], "\\b[A-Z]{4} \\d{3}\\b")[[1]]
#     str[idx] <- sample(course_codes, 1)
#   }
# 
#   present <-
#     vapply(str, function(x)
#       grepl("All of", x), logical(1))
#   if (TRUE %in% present) {
#     idx <- which(present == TRUE)
#     course_codes <-
#       str_extract_all(str[idx], "\\b[A-Z]{4} \\d{3}\\b")[[1]]
#     str <- str[-idx]
#     str <- c(str, course_codes)
#   }
# 
#   return(str_extract_all(paste(str, collapse = " "), "\\b[A-Z]{4} \\d{3}\\b")[[1]])
# 
# }
# 
# ubco <-
#   read.csv(
#     "C:\\Users\\danie\\OneDrive\\Desktop\\CurricularAnalytics\\data\\UBCO_Course_Calendar.csv"
#   )
# ubco$Prerequisite <-
#   stri_replace_all_fixed(ubco$Prerequisite, "\xa0", "")
# ubco$Prerequisite <- gsub("[[:punct:]]", "", ubco$Prerequisite)
# ubco$Corequisite <-
#   stri_replace_all_fixed(ubco$Corequisite, "\xa0", "")
# 
# # Create list for storing pathways
# Gc <- list()
# 
# set.seed(87460945)
# 
# for (i in 1:1000) {
#   # Generate degree pathway
#   pathway <-
#     data.frame(
#       Course.Code = NA,
#       Course.Name = NA,
#       Course.Description = NA,
#       Prerequisite = NA,
#       Corequisite = NA,
#       Equivalents = NA,
#       Term = NA
#     )
# 
#   # First Year
#   pathway <-
#     rbind(pathway, cbind(
#       subset(ubco, Course.Code == "DATA 101"),
#       data.frame("Term" =
#                    c(2))
#     ))
#   samp <- sample(c("CHEM 111", "CHEM 121"), 1)
#   pathway <-
#     rbind(pathway, cbind(subset(ubco, Course.Code == samp), data.frame("Term" =
#                                                                          c(1))))
#   pathway <-
#     rbind(pathway, cbind(subset(
#       ubco, Course.Code %in% c("MATH 100", "MATH 101")
#     ), data.frame("Term" = c(1, 2))))
#   samp <- sample(c(TRUE, FALSE), 1)
# 
#   if (samp) {
#     pathway <-
#       rbind(pathway, cbind(
#         subset(ubco, Course.Code == "ENGL 109"),
#         data.frame("Term" = c(1))
#       ))
#   } else {
#     samp <- sample(
#       c(
#         "ENGL 112",
#         "ENGL 113",
#         "ENGL 114",
#         "ENGL 150",
#         "ENGL 151",
#         "ENGL 153",
#         "ENGL 154",
#         "ENGL 155",
#         "ENGL 156"
#       ),
#       2
#     )
#     pathway <-
#       rbind(pathway, cbind(subset(ubco, Course.Code %in% samp), data.frame("Term" =
#                                                                              c(1, 2))))
#   }
# 
# 
#   samp <- c("PHYS 111", "PHYS 121")
#   pathway <-
#     rbind(pathway, cbind(subset(ubco, Course.Code %in% samp), data.frame("Term" =
#                                                                            c(1, 2))))
# 
#   pathway <-
#     rbind(pathway, cbind(subset(
#       ubco, Course.Code %in% c("COSC 111", "COSC 121")
#     ), data.frame("Term" = c(1, 2))))
# 
# 
#   # Second Year
#   pathway <-
#     rbind(pathway, cbind(subset(
#       ubco,
#       Course.Code %in% c("MATH 221",
#                          "MATH 200",
#                          "STAT 230",
#                          "COSC 221",
#                          "COSC 222")
#     ), data.frame("Term" = c(3, 2, 3, 3, 3))))
# 
# 
#   # Third and Fourth Year
#   pathway <-
#     rbind(pathway, cbind(subset(
#       ubco,
#       Course.Code %in% c("DATA 301",
#                          "DATA 311",
#                          "COSC 304",
#                          "STAT 303",
#                          "PHIL 331")
#     ), data.frame("Term" = rep(4, 5))))
# 
# 
# 
#   upper_year_data <-
#     c("DATA 310", "DATA 315", "DATA 405", "DATA 407", "DATA 410")
#   max_2_stat <- c("STAT 400", "STAT 401", "STAT 403", "STAT 406")
# 
#   # Removed MATH 303 because equiv to COSC 303
#   max_2_cosc_math_phys <-
#     c(
#       "COSC 303",
#       "COSC 322",
#       "COSC 329",
#       "COSC 344",
#       "COSC 407",
#       "COSC 421",
#       "MATH 307",
#       "MATH 409",
#       "PHYS 420"
#     )
#   course <- c()
#   while (length(unique(course)) < 9) {
#     var <- sample(c("1", "2", "3"), 1)
# 
#     if (var == 1) {
#       course <- c(course, sample(max_2_stat, 1))
#     } else if (var == 2) {
#       course <- c(course, sample(max_2_cosc_math_phys, 1))
#     } else if (var == 3) {
#       course <- c(course, sample(upper_year_data, 1))
#     }
#   }
# 
#   pathway <-
#     rbind(pathway, cbind(
#       subset(ubco, Course.Code %in% unique(course)),
#       data.frame("Term" =
#                    c(rep(5, 5), rep(6, 4)))
#     ))
# 
#   pathway <- na.omit(pathway)
#   rownames(pathway) <- 1:nrow(pathway)
# 
#   # Construct node and edge list
# 
#   node_list <-
#     data.frame(
#       id = rownames(pathway),
#       label = pathway$Course.Code,
#       term = pathway$Term
#     )
# 
#   edge_list <- data.frame(from = NA, to = NA)
# 
#   for (node in node_list$label) {
#     # node = "COSC 329"
#     str <- subset(pathway, Course.Code == node)$Prerequisite
#     str <- get_course_codes(str)
# 
#     from <- rownames(subset(pathway, Course.Code == node))
#     to <- rownames(subset(pathway, Course.Code %in% str))
#     if (length(to) > 1) {
#       for (id in to) {
#         edge_list <- rbind(edge_list, data.frame(from = id, to = from))
#       }
#     } else {
#       edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
#     }
# 
#     str <- subset(pathway, Course.Code == node)$Corequisite
#     str <- get_course_codes(str)
# 
#     from <- rownames(subset(pathway, Course.Code == node))
#     to <- rownames(subset(pathway, Course.Code %in% str))
#     if (length(to) > 1) {
#       for (id in to) {
#         edge_list <- rbind(edge_list, data.frame(from = id, to = from))
#       }
#     } else {
#       edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
#     }
#   }
#   edge_list <- na.omit(edge_list)
#   node_list$id <- as.numeric(node_list$id)
#   C <- curriculum_graph_from_list(node_list, edge_list)
#   Gc <- c(Gc, list(C))
# }
# 
# # # Initialize variables for maximum and minimum values
# max_total <- -Inf
# min_total <- Inf
# max_index <- NULL
# min_index <- NULL
# #
# # # Iterate through Gc to find max and min indices
# for (i in seq_along(Gc)) {
#   total <- sum(Gc[[i]]$sc_total)
#   if (total > max_total) {
#     max_total <- total
#     max_index <- i
#   }
#   if (total < min_total) {
#     min_total <- total
#     min_index <- i
#   }
# }
# 
# C_max <- Gc[[max_index]]
# save(C_max, file = "../data/DS-2022-Max-Graph.RData")
# 
# C_min <- Gc[[min_index]]
# save(C_min, file = "../data/DS-2022-Min-Graph.RData")

## -----------------------------------------------------------------------------
load("../data/DS-2022-Max-Graph.RData")
plot(C_max, height = 700)

## -----------------------------------------------------------------------------
# Define helper function for printing courses
print_top_two_rows <- function(df, column) {
  ordered_df <- df[rev(order(df[[column]])), ]
  top_two <- head(ordered_df, 3)
  print(top_two)
}

# Print top two courses ordered by each metric
columns <- colnames(C_max$node_list[,c("bf","df","cf","sc")])
for (column in columns) {
  print(paste("Ordering by column:", column))
  print_top_two_rows(C_max$node_list, column)
}

## -----------------------------------------------------------------------------
load("../data/DS-2022-Min-Graph.RData")
plot(C_min, height = 700)

## -----------------------------------------------------------------------------
# Define helper function for printing courses
print_top_two_rows <- function(df, column) {
  ordered_df <- df[rev(order(df[[column]])), ]
  top_two <- head(ordered_df, 3)
  print(top_two)
}

# Print top two courses ordered by each metric
columns <- colnames(C_min$node_list[,c("bf","df","cf","sc")])
for (column in columns) {
  print(paste("Ordering by column:", column))
  print_top_two_rows(C_min$node_list, column)
}

## ----echo=FALSE---------------------------------------------------------------
idx <- !(C_max$node_list$label %in% C_min$node_list$label)
kable(data.frame(Courses=C_max$node_list$label[idx]))

## ----echo=FALSE---------------------------------------------------------------
idx <- !(C_min$node_list$label %in% C_max$node_list$label)
kable(data.frame(Courses=C_min$node_list$label[idx]))

## ----echo=FALSE---------------------------------------------------------------
# library(stringi)
# library(stringr)
# 
# get_course_codes <- function(str) {
#   if (grepl("and", str)) {
#     str <- trimws(unlist(strsplit(str, "and")))
#   }
# 
#   present <-
#     vapply(str, function(x)
#       grepl("one of", x), logical(1))
#   if (TRUE %in% present) {
#     idx <- which(present == TRUE)
#     course_codes <-
#       str_extract_all(str[idx], "\\b[A-Z]{4} \\d{3}\\b")[[1]]
#     str[idx] <- sample(course_codes, 1)
#   }
# 
#   present <-
#     vapply(str, function(x)
#       grepl("either", x), logical(1))
#   if (TRUE %in% present) {
#     idx <- which(present == TRUE)
#     course_codes <-
#       str_extract_all(str[idx], "\\b[A-Z]{4} \\d{3}\\b")[[1]]
#     str[idx] <- sample(course_codes, 1)
#   }
# 
#   present <-
#     vapply(str, function(x)
#       grepl("All of", x), logical(1))
#   if (TRUE %in% present) {
#     idx <- which(present == TRUE)
#     course_codes <-
#       str_extract_all(str[idx], "\\b[A-Z]{4} \\d{3}\\b")[[1]]
#     str <- str[-idx]
#     str <- c(str, course_codes)
#   }
# 
#   return(str_extract_all(paste(str, collapse = " "), "\\b[A-Z]{4} \\d{3}\\b")[[1]])
# 
# }
# 
#  ubco <-
#    read.csv(
#      "..\\data\\UBCO_Course_Calendar_new.csv"
#    )
# 
# 
#  ubco$Prerequisite <-
#    stri_replace_all_fixed(ubco$Prerequisite, "\xa0", "")
#  ubco$Prerequisite <- gsub("[[:punct:]]", "", ubco$Prerequisite)
#  ubco$Corequisite <-
#    stri_replace_all_fixed(ubco$Corequisite, "\xa0", "")
# 
#  # Create list for storing pathways
#  Gc <- list()
# 
#  set.seed(87460945)
# 
#   for (i in 1:1000) {
#    # Generate degree pathway
#    pathway <-
#      data.frame(
#       Course.Code = NA,
#        Course.Name = NA,
#       Course.Description = NA,
#       Prerequisite = NA,
#        Corequisite = NA,
#        Equivalents = NA,
#        Term = NA
#      )
# 
#    # First Year
#    pathway <-
#      rbind(pathway, cbind(
#        subset(ubco, Course.Code == "DATA 101"),
#        data.frame("Term" =
#                     c(2))
#      ))
# 
#    pathway <-
#      rbind(pathway, cbind(subset(
#        ubco, Course.Code %in% c("MATH 100", "MATH 101")
#      ), data.frame("Term" = c(1, 2))))
#    samp <- sample(c(1:3), 1)
# 
#    if (samp == 1) {
#      pathway <-
#        rbind(pathway, cbind(
#          subset(ubco, Course.Code == "ENGL 109"),
#          data.frame("Term" = c(1))
#        ))
#    } else if(samp == 2){
#      samp <- sample(
#        c(
#          "ENGL 112",
#          "ENGL 113",
#          "ENGL 114",
#          "ENGL 150",
#          "ENGL 151",
#          "ENGL 153",
#          "ENGL 154",
#          "ENGL 155",
#          "ENGL 156"
#        ),
#        2
#      )
#      pathway <-
#        rbind(pathway, cbind(subset(ubco, Course.Code %in% samp), data.frame("Term" =
#                                                                               c(1, 2))))
#    } else if (samp == 3){
#       pathway <-
#        rbind(pathway, cbind(
#          subset(ubco, Course.Code == "CORH 203"),
#          data.frame("Term" = c(1))
#        ))   }
# 
#    pathway <-
#      rbind(pathway, cbind(subset(
#        ubco, Course.Code %in% c("COSC 111", "COSC 121")
#      ), data.frame("Term" = c(1, 2))))
# 
#    courses <- c("BIOL 116 or BIOL 117", "BIOL 122 or BIOL 125", "BIOL 131", "BIOL 133", "CHEM 111 or CHEM 121","CHEM 113 or CHEM 123", "EESC 111", "EESC 121", "PHYS 111 or PHYS 112", "PHYS 121 or PHYS 122")
# 
# 
#    courses <- sample(courses, 2)
# 
#    courses <- str_split(courses, " or ")
#    courses <- c(sample(unlist(courses[1]),1), sample(unlist(courses[2]),1))
#    pathway <-
#      rbind(pathway, cbind(subset(ubco, Course.Code %in% courses), data.frame("Term" = c(1,2))))
#    pathway <- na.omit(pathway)
# 
#    courses <- pathway$Course.Code
#    for (course in courses) {
#      if(course == "CORH 203"){
#        next()
#      }
#      subset(pathway, Course.Code == course)$Prerequisite |> get_course_codes() -> cur_course_prereq
#      for (prereq in cur_course_prereq) {
#        if (!(prereq %in% pathway$Course.Code)) {
#          pathway <-
#            rbind(pathway, cbind(
#              subset(ubco, Course.Code %in% prereq),
#              data.frame("Term" = c(1))
#            ))
#        }
#      }
#    }
# 
# 
#    # Second Year
#    pathway <-
#      rbind(pathway, cbind(subset(
#        ubco,
#        Course.Code %in% c("MATH 200",
#                           "MATH 220",
#                           "MATH 221",
#                           "MATH 222",
#                           "MATH 225",
#                           "STAT 203",
#                           "STAT 205",
#                           "COSC 222")
#      ), data.frame("Term" = rep(3,8))))
# 
# 
#    # Third and Fourth Year
#    pathway <-
#      rbind(pathway, cbind(subset(
#        ubco,
#        Course.Code %in% c("DATA 310",
#                           "DATA 311",
#                           "DATA 315",
#                           "STAT 303",
#                           "PHIL 331",
#                           "COSC 304")
#      ), data.frame("Term" = rep(4, 6))))
# 
# 
# 
#    upper_year_data <-
#      c("DATA 405", "DATA 407", "DATA 410")
#    
#    max_2_stat <- c("STAT 400", "STAT 401", "STAT 403", "STAT 406")
#    stat_counter <- 0
# 
#    max_2_cosc_phys <-
#      c(
#       "COSC 322","COSC 329", "COSC 344", "COSC 421", "PHYS 420"
#      )
#    cosc_counter <- 0
#    
#    max_2_math <- c("MATH 303", "MATH 307", "MATH 327", "MATH 409"
#                    )
#    math_counter <- 0
#    
#    course <- c()
#    while (length(unique(course)) < 8) {
#      
#      var <- sample(c("1", "2", "3", "4"), 1)
#      
#      if (var == 1 & sum(max_2_stat %in% unique(course)) < 2) {
#        
#        pot_course <- sample(max_2_stat, 1)
#        
#        if (!(pot_course %in% pathway$Course.Code)) {
#          
#          course <- c(course, pot_course)
#          stat_counter <- stat_counter + 1
#        }
#        
#      } else if (var == 2 & sum(max_2_cosc_phys %in% unique(course)) < 2) {
#        pot_course <- sample(max_2_cosc_phys, 1)
#        if (!(pot_course %in% pathway$Course.Code)) {
#          course <- c(course, pot_course)
#          
#        }
#      } else if (var == 3) {
#        pot_course <- sample(upper_year_data, 1)
#        if (!(pot_course %in% pathway$Course.Code)) {
#          course <- c(course, pot_course)
#        }
#      } else if (var == 4 & sum(max_2_math %in% unique(course)) < 2) {
#        pot_course <- sample(max_2_math, 1)
#        if (!(pot_course %in% pathway$Course.Code)) {
#          course <- c(course, pot_course)
#        }
#      }
#      
#    }
# 
#    
#   
#    
#    pathway <-
#     rbind(pathway,  cbind(
#       subset(ubco, Course.Code %in% unique(course)),
#       data.frame("Term" =
#                    c(rep(5, 4), rep(6, 4)))
#     ))
# 
# 
#   rownames(pathway) <- 1:nrow(pathway)
# 
#   # Construct node and edge list
# 
#   node_list <-
#     data.frame(
#       id = rownames(pathway),
#       label = pathway$Course.Code,
#       term = pathway$Term
#     )
# 
#   edge_list <- data.frame(from = NA, to = NA)
# 
#   for (node in node_list$label) {
#     # node = "MATH 200"
#     str <- subset(pathway, Course.Code == node)$Prerequisite
#     str <- get_course_codes(str)
# 
#     from <- rownames(subset(pathway, Course.Code == node))
#     to <- rownames(subset(pathway, Course.Code %in% str))
#     if (length(to) > 1) {
#       for (id in to) {
#         edge_list <- rbind(edge_list, data.frame(from = id, to = from))
#       }
#     } else {
#       edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
#     }
# 
#     str <- subset(pathway, Course.Code == node)$Corequisite
#     str <- get_course_codes(str)
# 
#     from <- rownames(subset(pathway, Course.Code == node))
#     to <- rownames(subset(pathway, Course.Code %in% str))
#     if (length(to) > 1) {
#       for (id in to) {
#         edge_list <- rbind(edge_list, data.frame(from = id, to = from))
#       }
#     } else {
#       edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
#     }
#   }
#   edge_list <- na.omit(edge_list)
#   node_list$id <- as.numeric(node_list$id)
#   C <- curriculum_graph_from_list(node_list, edge_list)
#   Gc <- c(Gc, list(C))
# }
# #
# # # Initialize variables for maximum and minimum values
# max_total <- -Inf
# min_total <- Inf
# max_index <- NULL
# min_index <- NULL
# #
# # # Iterate through Gc to find max and min indices
# for (i in seq_along(Gc)) {
#   total <- sum(Gc[[i]]$sc_total)
#   if (total > max_total) {
#     max_total <- total
#     max_index <- i
#   }
#   if (total < min_total) {
#     min_total <- total
#     min_index <- i
#   }
# }
# 
# C_max <- Gc[[max_index]]
# plot(C_max)
# save(C_max, file = "../data/DS-2023-Max-Graph.RData")
# # 
# # 
# C_min <- Gc[[min_index]]
# plot(C_min)
# save(C_min, file = "../data/DS-2023-Min-Graph.RData")

## -----------------------------------------------------------------------------
load("../data/DS-2023-Max-Graph.RData")
plot(C_max, height = 700)

## -----------------------------------------------------------------------------
columns <- colnames(C_max$node_list[,c("bf","df","cf","sc")])
for (column in columns) {
  print(paste("Ordering by column:", column))
  print_top_two_rows(C_max$node_list, column)
}

## -----------------------------------------------------------------------------
load("../data/DS-2023-Min-Graph.RData")
plot(C_min, height = 700)

## -----------------------------------------------------------------------------
# Print top two courses ordered by each metric
columns <- colnames(C_max$node_list[,c("bf","df","cf","sc")])
for (column in columns) {
  print(paste("Ordering by column:", column))
  print_top_two_rows(C_max$node_list, column)
}

## ----echo=FALSE---------------------------------------------------------------
idx <- !(C_max$node_list$label %in% C_min$node_list$label)
kable(data.frame(Courses=C_max$node_list$label[idx]))

## ----echo=FALSE---------------------------------------------------------------
idx <- !(C_min$node_list$label %in% C_max$node_list$label)
kable(data.frame(Courses=C_min$node_list$label[idx]))

## -----------------------------------------------------------------------------
library(visNetwork)

# Create Curriculum Graph
C <- curriculum_graph_from_csv("..\\data\\DS-Minor-Max.csv")

C$node_list <- C$node_list[order(C$node_list$term), ]

# Specify shape and group for each node
C$node_list$shape <- c(rep("circle",10),rep("triangle",16))
C$node_list$group <- c(rep("FALSE",10),rep("TRUE",16))

# Helper function to generate coordinates
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

# Create fully customizable plot
visNetwork(
  C$node_list,
  C$edge_list,
  height = 700,
  width = 700,
  submain = paste(
    "Total Structural Complexity:",
    C$sc_total,
    "Total Blocking Factor:",
    C$bf_total,
    "Total Delay Factor:",
    C$df_total
  )
) %>%
  visEdges(arrows = "to") %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = generate_coords(C)) %>%
  visEvents(
    selectNode = "function(properties) {
      alert(' sc: ' + this.body.data.nodes.get(properties.nodes[0]).sc + ' cf: ' + this.body.data.nodes.get(properties.nodes[0]).cf + ' bf: ' + this.body.data.nodes.get(properties.nodes[0]).bf + ' df: ' + this.body.data.nodes.get(properties.nodes[0]).df);}"
  )%>%
  visGroups(groupname = "TRUE", color = "red") %>%
  visGroups(groupname = "FALSE", color = "lightblue") %>%
  visLegend(width = 0.1, position = "right", main = "Is a Preqreq in the Minor")

## -----------------------------------------------------------------------------
# Create Curriculum Graph
C <- curriculum_graph_from_csv("..\\data\\DS-Minor-Min.csv")

C$node_list <- C$node_list[order(C$node_list$term), ]

# Specify shape and group for each node
C$node_list$shape <- c(rep("circle",9),rep("triangle",3))
C$node_list$group <- c(rep("FALSE",9),rep("TRUE",3))

visNetwork(
  C$node_list,
  C$edge_list,
  height = 500,
  width = 500,
  submain = paste(
    "Total Structural Complexity:",
    C$sc_total,
    "Total Blocking Factor:",
    C$bf_total,
    "Total Delay Factor:",
    C$df_total
  )
) %>%
  visEdges(arrows = "to") %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = generate_coords(C)) %>%
  visEvents(
    selectNode = "function(properties) {
      alert(' sc: ' + this.body.data.nodes.get(properties.nodes[0]).sc + ' cf: ' + this.body.data.nodes.get(properties.nodes[0]).cf + ' bf: ' + this.body.data.nodes.get(properties.nodes[0]).bf + ' df: ' + this.body.data.nodes.get(properties.nodes[0]).df);}"
  )%>%
  visGroups(groupname = "TRUE", color = "red") %>%
  visGroups(groupname = "FALSE", color = "lightblue") %>%
  visLegend(width = 0.1, position = "right", main = "Is a Preqreq in the Minor")

## -----------------------------------------------------------------------------
# Create curriculum graph
C <- curriculum_graph_from_csv("..\\data\\DS-Major-Math-Max.csv")

# Plot curriculum graph
plot(C)

## -----------------------------------------------------------------------------
# Print top two courses ordered by each metric
columns <- colnames(C$node_list[,c("bf","df","cf","sc")])
for (column in columns) {
  print(paste("Ordering by column:", column))
  print_top_two_rows(C$node_list, column)
}

## -----------------------------------------------------------------------------
# Create curriculum graph
C <- curriculum_graph_from_csv("..\\data\\DS-Major-Math-Min.csv")

# Plot curriculum graph
plot(C)

## -----------------------------------------------------------------------------
# Print top two courses ordered by each metric
columns <- colnames(C$node_list[,c("bf","df","cf","sc")])
for (column in columns) {
  print(paste("Ordering by column:", column))
  print_top_two_rows(C$node_list, column)
}

