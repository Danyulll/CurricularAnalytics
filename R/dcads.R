# Helper Functions
plot_graph <- function(selectedOptions, path) {
  df <-
    utils::read.csv(path)

  df <- df[which(df$label %in% selectedOptions), ]

  if (is.null(selectedOptions))
    warning("selectedOptions is null (plot_graph)")

  if (length(selectedOptions) %in% 0) {
    # Currently if this condition just checks 0 when you uncheck all boxes it will still display 1 node despite the checkboxes vector being 0. This is because of how selectedOptions is updated, its length stays at 1. Need to learn how to force update SelecredOptions so that it goes to length 0 when all checkboxes are unchecked.
    nodes <- data.frame(id = numeric(0), label = character(0))
    edges <- data.frame(from = numeric(0), to = numeric(0))
    visNetwork::visNetwork(nodes, edges)
  } else if (length(selectedOptions) %in% 1) {
    node_list <-
      data.frame(id = 1,
                 label = df$label,
                 term = df$term)
    edge_list <- data.frame(from = integer(), to = integer())
    c <-
      curriculum_graph_from_list(node_list = node_list, edge_list = edge_list)
    plot_curriculum_graph(c)

  } else{
    node_list <-
      data.frame(id = df$id,
                 label = df$label,
                 term = df$term)

    db_ids <- df[which(df$label %in% selectedOptions), ]$id
    app_ids <- 1:length(db_ids)

    map <- setNames(object = app_ids, nm = db_ids)

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

    for (i in 1:nrow(edge_list)) {
      if (length(edge_list$from[i] %in% names(map)) == 0) {
        next()
      } else{
        if (is.null(edge_list$to[i] %in% names(map)))
          cat("Error 1 something is null\n")
        if (length(edge_list$from[i] %in% names(map)) == 0)
          cat("Error 2 0 length argument\n")

        if (edge_list$from[i] %in% names(map)) {
          edge_list$from[i] <-  map[which(names(map) %in%  edge_list$from[i])]
        }
      }
    }

    for (i in 1:nrow(edge_list)) {
      cat(length(edge_list$to[i] %in% names(map) == 0))
      cat("\n")
      if (length(edge_list$to[i] %in% names(map) == 0) == 0) {
        next
      } else{
        if (edge_list$to[i] %in% names(map)) {
          edge_list$to[i] <-  map[which(names(map) %in%  edge_list$to[i])]
        }
      }

    }


    node_list$id <- rownames(node_list) |> as.numeric()
    c <-
      curriculum_graph_from_list(node_list = node_list, edge_list = edge_list)
    CurricularAnalytics::plot_curriculum_graph(c)

  }
}

getCourses <- function(year, path, subject) {
  df <-
    utils::read.csv(path)

  res <- grep(paste("\\b[", year, "]\\d{2}\\b"), df$label, value = TRUE)
  res <- res[substr(res, 1, 4) == subject]
  res
}

plot_curriculum_graph <- function(curriculum_graph,
                                  width = "100%",
                                  height = 500) {
  curriculum_graph$node_list <-
    curriculum_graph$node_list[order(curriculum_graph$node_list$term), ]
  # coords <- generate_coords(curriculum_graph)
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
  ) %>%
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visEvents(
      selectNode = "function(properties) {
      alert(' sc: ' + this.body.data.nodes.get(properties.nodes[0]).sc + ' cf: ' + this.body.data.nodes.get(properties.nodes[0]).cf + ' bf: ' + this.body.data.nodes.get(properties.nodes[0]).bf + ' df: ' + this.body.data.nodes.get(properties.nodes[0]).df);}"
    )
}

# Define the nltk_pipeline function in R
# nltk_pipeline <- function(input_string) {
#   # Create tokenizer and stemmer instances
#   np <- import("numpy")
#   pd <- import("pandas")
#
#   nltk_corpus <- import("nltk.corpus")
#   stopwords <- nltk_corpus$stopwords
#
#   nltk_tokenize <- import("nltk.tokenize")
#   RegexpTokenizer <- nltk_tokenize$RegexpTokenizer
#
#   nltk_stem <- import("nltk.stem")
#   PorterStemmer <- nltk_stem$PorterStemmer
#
#   nltk <- import("nltk")
#   FreqDist <- nltk$FreqDist
#   bigrams <- nltk$bigrams
#
#   collections <- import("collections")
#   Counter <- collections$Counter
#
#   sklearn_decomposition <- import("sklearn.decomposition")
#   TruncatedSVD <- sklearn_decomposition$TruncatedSVD
#
#   sklearn_feature_extraction_text <- import("sklearn.feature_extraction.text")
#   TfidfTransformer <- sklearn_feature_extraction_text$TfidfTransformer
#   CountVectorizer <- sklearn_feature_extraction_text$CountVectorizer
#
#   sklearn_preprocessing <- import("sklearn.preprocessing")
#   Normalizer <- sklearn_preprocessing$Normalizer
#
#   tokenizer <- RegexpTokenizer('\\w+')
#   nltk_stem <- import("nltk.stem")
#   PorterStemmer <- nltk_stem$PorterStemmer
#   stemmer <- PorterStemmer()
#
#   # cat("stuff loaded again")
#
#   # Tokenize input string and remove punctuation
#   tokens <- tokenizer$tokenize(input_string)
#   # cat("tokenize")
#   # Remove tokens containing numbers
#   tokens <- Filter(function(token) !grepl('\\d', token), tokens)
#   # cat("filter tokens")
#   #Convert tokens to lowercase and remove stopwords
#   stop_words_set <- stopwords$words("english")
#   custom_stop_words_set <- unique(c("ii", "i", "e", "g", "official", "calendar", "first", "year", "second", "third", "fourth", "fall", "spring", "summer", "winter", "credit", "granted"))
#   stop_words_combined <- union(stop_words_set, custom_stop_words_set)
#   filtered_tokens <- Filter(function(token) !(tolower(token) %in% stop_words_combined), tokens)
#   # cat("filter out stop words")
#   # Perform stemming on filtered tokens
#   stemmed_tokens <- sapply(filtered_tokens, function(token) stemmer$stem(token))
#   # cat("stem")
#   bigram_freq <- FreqDist(bigrams(stemmed_tokens))
#
#   # I had to remove the bigram functionality for the topic model due to werid Python to R conversions that crash the app. It would be nice to fix this.
#   # names(which(unlist(bigram_freq) >= 2)) |> str_extract_all( "\\b\\w+\\b") -> frequent_bigrams
#   # frequent_bigrams <- paste(unlist(frequent_bigrams),collapse = "_")
#   # cat("bigram stuff")
#
#   final_tokens <- c(stemmed_tokens) #, frequent_bigrams[[1]]
#   token_counts <- Counter(final_tokens)
#   filtered_final_tokens <- Filter(function(token) token_counts[[token]] > 0, final_tokens)
#   return(paste(filtered_final_tokens, collapse=' '))
# }

# Define the lsaDocSim function in R
# lsaDocSim <- function(query_course, year,df) {
#
#   # Clean data
#   clean_text <- sapply(df$`Course Description`, nltk_pipeline)
#
#   cat("Data cleaned")
#
#   np <- import("numpy")
#   pd <- import("pandas")
#
#   nltk_corpus <- import("nltk.corpus")
#   stopwords <- nltk_corpus$stopwords
#
#   nltk_tokenize <- import("nltk.tokenize")
#   RegexpTokenizer <- nltk_tokenize$RegexpTokenizer
#
#   nltk_stem <- import("nltk.stem")
#   PorterStemmer <- nltk_stem$PorterStemmer
#
#   nltk <- import("nltk")
#   FreqDist <- nltk$FreqDist
#   bigrams <- nltk$bigrams
#
#   collections <- import("collections")
#   Counter <- collections$Counter
#
#   sklearn_decomposition <- import("sklearn.decomposition")
#   TruncatedSVD <- sklearn_decomposition$TruncatedSVD
#
#   sklearn_feature_extraction_text <- import("sklearn.feature_extraction.text")
#   TfidfTransformer <- sklearn_feature_extraction_text$TfidfTransformer
#   CountVectorizer <- sklearn_feature_extraction_text$CountVectorizer
#
#   sklearn_preprocessing <- import("sklearn.preprocessing")
#   Normalizer <- sklearn_preprocessing$Normalizer
#
#   tokenizer <- RegexpTokenizer('\\w+')
#   nltk_stem <- import("nltk.stem")
#   PorterStemmer <- nltk_stem$PorterStemmer
#   stemmer <- PorterStemmer()
#
#   vectorizer <- CountVectorizer(min_df=1L)
#   dtm <- vectorizer$fit_transform(clean_text)
#
#   tfidf_transformer <- TfidfTransformer()
#   tfidf_dtm <- tfidf_transformer$fit_transform(dtm)
#
#
#   lsa <- TruncatedSVD(4L, algorithm='randomized')
#   dtm_lsa <- lsa$fit_transform(tfidf_dtm)
#
#   dtm_lsa <- Normalizer()$fit_transform(dtm_lsa)
#
#   doc_embeddings <- np$asmatrix(pd$DataFrame(dtm_lsa))
#   other_info <- np$asmatrix(df[7:ncol(df)])
#
#   row_norms <- np$linalg$norm(other_info, axis=1L)
#   normalized_matrix <- sweep(other_info, 1, row_norms, FUN="/")
#
#   mat <- np$concatenate(list(normalized_matrix, doc_embeddings), axis=1L)
#   doc_sim <- mat %*% t(mat)
#
#   cat("doc_sim made\n")
#
#   df$ID <- seq_len(nrow(df))
#   # map <- df[,c('Course Code','ID')]$set_index('Course Code')$to_dict(orient='dict')['ID']
#   # TODO check that map is converted right
#   map <- df %>%
#     select(`Course Code`, ID) %>%
#     distinct() %>%
#     # Convert to a simple data frame
#     as.data.frame(stringsAsFactors = FALSE)
#
#   # Create a named vector that maps 'Course Code' to 'ID'
#   map_vector <- setNames(map$ID, map$`Course Code`)
#
#   index <- map_vector[names(map_vector)==query_course]
#
#   sorted_indices <- order(doc_sim[index,])[nrow(doc_sim):1]
#   sorted_values <- as.array(doc_sim[index, sorted_indices])
#   sorted_values <- sorted_values / max(sorted_values)
#
#
#
#   df_out <- data.frame(
#     'Course Code' = substr(names(map_vector[sorted_indices]),6,9),
#     'Course Name' = substr(names(map_vector[sorted_indices]),1,4),
#     'Similarity' = sorted_values
#   )
#
#   if(year %in% 1:4){
#     indices <- which(df_out$Course.Code |> substr(1,1) |>as.numeric() == year)
#     df_out <- df_out[indices,]
#   }
#
#   cat("Returning result\n")
#   return(df_out[2:4, ])
# }

# create_python_env <- function(env_name="shiny_app") {
#   # Load reticulate library
#   library(reticulate)
#
#   # Create a new virtual environment
#   virtualenv_create(env_name)
#
#   # Install necessary Python packages in the created environment
#   packages_to_install <- c("numpy", "pandas", "nltk", "scikit-learn")
#   py_install(packages=packages_to_install, envname=env_name)
#
#   # Download additional NLTK data (stopwords, etc.)
#   py_run_string("import nltk; nltk.download('stopwords'); nltk.download('punkt')")
#
#   # Activate the environment
#   use_virtualenv(env_name, required = TRUE)
#
#   cat("Python environment", env_name, "is set up and ready to use.")
# }

# use_or_create_env <- function(env_name="shiny_app") {
#   tryCatch(
#     {
#       # Try to use the virtual environment
#       reticulate::use_virtualenv(env_name, required = TRUE)
#       cat("Successfully activated the Python environment:", env_name, "\n")
#     },
#     error = function(e) {
#       if(is_python_installed()){
#         cat("Installed already")
#       }else{
#         reticulate::install_python()
#       }
#       # If an error occurs, create and activate the environment
#       cat("Error in activating environment. Creating a new environment:", env_name, "\n")
#       create_python_env(env_name)
#       reticulate::use_virtualenv(env_name, required = TRUE)
#       cat("Successfully created and activated the Python environment:", env_name, "\n")
#     }
#   )
# }

# is_python_installed <- function() {
#   py_config <- tryCatch({
#     py_discover_config()
#   }, error = function(e) {
#     return(NULL)
#   })
#
#   return(!is.null(py_config$python))
# }

format_data_no_fail_handling <- function(empty, courses, df_whole) {
  df_whole |> dplyr::filter(COURSE_CODE %in% courses) |> dplyr::pull(STUD_NO_ANONYMOUS) |> unique() -> ids

  df_filtered <-
    data.frame(
      "STUD_NO_ANONYMOUS" = NA,
      "HDR_CRS_PCT_GRD" = NA,
      "HDR_CRS_LTTR_GRD" = NA,
      "COURSE_CODE" = NA
    )

  for (id in ids) {
    courses_taken <- subset(df_whole, STUD_NO_ANONYMOUS == id)$COURSE_CODE
    if (all(courses %in% courses_taken)) {
      for (course in courses) {
        df_filtered <-
          rbind(df_filtered,
                subset(df_whole, STUD_NO_ANONYMOUS == id &
                         COURSE_CODE == course))
      }
    }
  }
  df_filtered <- stats::na.omit(df_filtered)
  df_filtered$STUD_NO_ANONYMOUS |> unique() -> filtered_ids

  length(filtered_ids)

  for (id in filtered_ids) {
    maj <-
      subset(df_whole, STUD_NO_ANONYMOUS == id)$CURR_SPEC_PRIM_SUBJECT_1[length(subset(df_whole, STUD_NO_ANONYMOUS == id)$CURR_SPEC_PRIM_SUBJECT_1)]

    df_filt <-
      subset(df_whole, STUD_NO_ANONYMOUS == id)[, c("HDR_CRS_PCT_GRD", "COURSE_CODE")]
    df_filt <- df_filt[df_filt$COURSE_CODE %in% courses, ]
    df_filt <- data.frame(t(df_filt))

    # Take highest grade for repeat courses
    if (any(duplicated(as.vector(df_filt[2, ])))) {
      dups_indicies <- numeric(2)
    } else{
      dups_indicies <- numeric(0)
    }

    while (length(dups_indicies) > 1) {
      dups_indicies <- which(duplicated(as.vector(df_filt[2, ])))
      index <- dups_indicies[1]

      # Get repeated course
      repeat_course <- df_filt[2, index]
      repeat_course_df <-
        df_filt[, which(df_filt[2, ] == repeat_course)]

      # Get best grade of repeated course
      best_idx <-
        which.max(repeat_course_df[1, ])
      best_grade <- repeat_course_df[, best_idx]

      # drop repeated course in df
      filter <- (as.vector(df_filt[2, ]) == repeat_course)
      trues <- which(filter == TRUE)
      df_filt <- df_filt[, -trues]

      # attach best score of repeated course
      df_filt <- cbind(df_filt, best_grade)
    }

    cols <- df_filt[2, ]

    if (nrow(data.frame(df_filt[1, ])) > 1) {
      df_filt <- t(data.frame(df_filt[1, ]))
    } else{
      df_filt <- data.frame(df_filt[1, ])
    }

    colnames(df_filt) <- cols

    rownames(df_filt) <- ""

    if (sum(colnames(empty) %in% colnames(df_filt)) == ncol(empty)) {
      empty <- rbind(empty, df_filt)
    }
  }

  empty <- stats::na.omit(empty)
  empty <- data.frame(sapply(empty, as.numeric))
  empty
}




#'Launch DCADS: Data-driven Curriculum Analytics and Design System
#'
#'A function to create and launch the DCADS Shiny app.
#'
#'More explanation here...
#'
#'@param curriculum_csv A CSV in the form of `filepath` found in \link{curriculum_graph_from_csv}
#'@param course_codes A character vector with all course codes in `curriculum_csv`
#'@param topic_model A topic model fitted with TBA
#'@param pred_model A preditive model for grades fitted with TBA
#'@param front_text A HTML string with text to display on the front page of the app
#'
#'@return ...
#'
#'@author Daniel Krasnov
#'@references Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
#'  Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the Impact
#'  of Curricular Reforms and Pedagogical Innovations.” arXiv Preprint
#'  arXiv:1811.09676.
#' @examples
#' ...
#'
#'@export
dcads <- function(curriculum_csv_path,
                  course_codes_key,
                  topic_model = NULL,
                  pred_model = NULL,
                  front_text = NULL,
                  hide_pred = FALSE,
                  hide_topic = FALSE) {
  # Define the user interface of the Shiny app
  ui <- navbarPage(
    "DCADS V0.1",
    tabPanel("Legend", titlePanel("About the App"), fluidRow(column(12, if (is.null(front_text)) {
      cat("front_text is null\n")
      HTML("Populate front_text with raw html")
    } else{
      HTML(front_text)
    }))),
    tabPanel(
      "Explorer",
      titlePanel("Data Science Curriculum Explorer"),
      fluidRow(
        column(
          4,
          # Side panel for inputs and information
          wellPanel(
            h4("Courses Information"),
            selectInput(
              "dropdownYear",
              "Select Year",
              choices = c("Year 1", "Year 2", "Year 3", "Year 4")
            ),
            selectInput("dropdownCourseCode", "Select Code", choices = course_codes_key),
            uiOutput("coursestaken"),
            actionButton("resetButton", "Reset")
          )
        ),
        column(4, wellPanel(
          h4("Interactive Graph"), visNetworkOutput("network")
        )),
        column(4, wellPanel(tabsetPanel(
          tabPanel(
            "Predicted Grade",
            if (!is.null(pred_model)) {
            conditionalPanel(
              condition = "true",
              textAreaInput(
                "response_input",
                "Response Course",
                value = "DATA.311",
                rows = 1
              ),
              textAreaInput(
                "predictor_input",
                "Predictor Course(s)",
                value = "STAT.230,MATH.101,MATH.100,COSC.111",
                rows = 1
              ),
              textAreaInput(
                "predictor_grade_input",
                "Predictor Course(s) Grade(s)",
                value = "70,85,75,90",
                rows = 1
              ),
              actionButton("submit_button_pred_course_grad", "Submit"),
              uiOutput("pred_grad_output")
            )}else{HTML("Provide predictivie model for this feature")}

          )
        ,
          tabPanel(
            "Course Similarity",
            if (!is.null(topic_model)) {
            conditionalPanel(
              condition = "true",
              textAreaInput(
                "text_input",
                "Enter Text",
                value = "",
                rows = 1
              ),
              selectInput("sugYear", "Select Year", choices = c("1", "2", "3", "4", "Any")),
              actionButton("submit_button", "Submit"),
              uiOutput("courseRecSim")
            )}else{HTML("Provide topic model for this feature")}

          )
        )))
      )
    )
  )

  server <- function(input, output, session) {
    # Create reactive element to check changes in database contents
    databaseContents <- shiny::reactivePoll(
      50,
      session,
      checkFunc = function() {
        file.info("my_courses_db2.sqlite")$mtime
      },
      valueFunc = function() {
        DBI::dbGetQuery(db, "SELECT * FROM selected_courses")
      }
    )

    # Connect to database
    db <- DBI::dbConnect(RSQLite::SQLite(), "my_courses_db2.sqlite")

    # If table does not exist create it
    query <- "CREATE TABLE IF NOT EXISTS selected_courses (
    id INTEGER PRIMARY KEY,
    course_name TEXT,
    year INTEGER,
    course_code TEXT)"
    DBI::dbExecute(db, query)

    # Render curriculum graph
    output$network <- visNetwork::renderVisNetwork({
      predictor_input <- databaseContents() # Courses are taken from database

      cat("Selected courses in graph:\n")
      cat(paste(predictor_input$course_name, sep = ","))
      cat("\n")

      # If no courses selected, display an empty graph
      if (nrow(predictor_input) == 0) {
        empty_nodes <- data.frame(id = numeric(0), label = character(0))
        empty_edges <- data.frame(from = numeric(0), to = numeric(0))
        visNetwork(empty_nodes, empty_edges)
      } else {
        courseNames <- predictor_input$course_name
        plot_graph(courseNames,curriculum_csv_path)
      }
    })

    # Render course selection UI
    output$coursestaken <- renderUI({
      # Get the year number for the selected Year
      yearNum <- as.numeric(gsub("Year ", "", input$dropdownYear))

      # Fetch selected courses from the database for the current year
      selectedCourses <- DBI::dbGetQuery(db,
                                    paste0(
                                      "SELECT course_name FROM selected_courses WHERE year = ",
                                      yearNum
                                    ))

      # Extract the course names to a vector
      selectedCourseNames <- selectedCourses$course_name

      # Now use selectedCourseNames for the 'selected' parameter to maintain previously selected courses
      if (input$dropdownYear == "Year 1") {
        checkboxGroupInput(
          "checkboxes1",
          "Choose Options",
          choices = getCourses(1, curriculum_csv_path, input$dropdownCourseCode),
          selected = selectedCourseNames
        )
      } else if (input$dropdownYear == "Year 2") {
        checkboxGroupInput(
          "checkboxes2",
          "Choose Options",
          choices = getCourses(2, curriculum_csv_path, input$dropdownCourseCode),
          selected = selectedCourseNames
        )
      } else if (input$dropdownYear == "Year 3") {
        checkboxGroupInput(
          "checkboxes3",
          "Choose Options",
          choices = getCourses(3, curriculum_csv_path, input$dropdownCourseCode),
          selected = selectedCourseNames
        )
      } else if (input$dropdownYear == "Year 4") {
        checkboxGroupInput(
          "checkboxes4",
          "Choose Options",
          choices = getCourses(4, curriculum_csv_path, input$dropdownCourseCode),
          selected = selectedCourseNames
        )
      }
    })

    # Helper function to update database based on checkbox changes
    updateDatabaseBasedOnCheckbox <- function(inputId, year, courseCode) {
      # Get current selections from the input
      selectedCourses <- input[[inputId]]

      # Fetch current selections from the database for this year and course code
      currentSelections <- DBI::dbGetQuery(
        db,
        sprintf(
          "SELECT course_name FROM selected_courses WHERE year = %d AND course_code = '%s'",
          year,
          courseCode
        )
      )

      # Determine courses to add or remove
      coursesToAdd <- setdiff(selectedCourses, currentSelections$course_name)
      coursesToRemove <- setdiff(currentSelections$course_name, selectedCourses)

      # Insert new selections
      sapply(coursesToAdd, function(course) {
        DBI::dbExecute(
          db,
          "INSERT INTO selected_courses (course_name, year, course_code) VALUES (?, ?, ?)",
          params = list(course, year, courseCode)
        )
      })

      # Remove deselected courses
      sapply(coursesToRemove, function(course) {
        DBI::dbExecute(
          db,
          "DELETE FROM selected_courses WHERE course_name = ? AND year = ? AND course_code = ?",
          params = list(course, year, courseCode)
        )
      })

      currentCourses <- DBI::dbGetQuery(db,
                                   "SELECT course_name FROM selected_courses WHERE year = 1")
      cat(
        "Courses selected for Year 1:",
        paste(currentCourses$course_name, collapse = ", "),
        "\n"
      )
    }

    # Observe changes to checkboxes and update the database accordingly
    observe({
      updateDatabaseBasedOnCheckbox("checkboxes1", 1, input$dropdownCourseCode)
    })

    observe({
      updateDatabaseBasedOnCheckbox("checkboxes2", 2, input$dropdownCourseCode)
    })

    observe({
      updateDatabaseBasedOnCheckbox("checkboxes3", 3, input$dropdownCourseCode)
    })

    observe({
      updateDatabaseBasedOnCheckbox("checkboxes4", 4, input$dropdownCourseCode)
    })

    # Logic for reset button to remove all courses from database
    observeEvent(input$resetButton, {
      # Reset the UI elements for all checkboxes to be unselected
      updateCheckboxGroupInput(session, "checkboxes1", selected = character(0))
      updateCheckboxGroupInput(session, "checkboxes2", selected = character(0))
      updateCheckboxGroupInput(session, "checkboxes3", selected = character(0))
      updateCheckboxGroupInput(session, "checkboxes4", selected = character(0))

      # Execute a query to delete all records from the 'selected_courses' table in the database
      DBI::dbExecute(db, "DELETE FROM selected_courses")
    })

    course_pred_data <- eventReactive(input$submit_button_pred_course_grad, {
      df <- read.csv("..\\data\\student-data.csv")
      df_clean <- df[, c(1, 12, 13, 15, 16)]
      df_clean <- na.omit(df_clean)
      # Do some cleaning on chr columns
      df_clean$STUD_NO_ANONYMOUS <- trimws(df_clean$STUD_NO_ANONYMOUS)
      df_clean$CRS_DPT_CD <- trimws(df_clean$CRS_DPT_CD)
      df_clean$HDR_CRS_LTTR_GRD <- trimws(df_clean$HDR_CRS_LTTR_GRD)

      # Factor grades column
      grades <- c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F")
      df_clean$HDR_CRS_LTTR_GRD <- factor(df_clean$HDR_CRS_LTTR_GRD, levels = grades)

      # Create course code column
      df_clean$COURSE_CODE <- paste(df_clean$CRS_DPT_CD, df_clean$CRS_NO, sep = ".")
      df_clean <- df_clean[, -(2:3)]
      df <- subset(df, HDR_CRS_PCT_GRD < 999)
      df_clean <- subset(df_clean, HDR_CRS_PCT_GRD < 999)
      df_clean
    })

    # Create reactive course suggestion data for topic model
    reactive_data <- eventReactive(input$submit_button, {
      ti <- input$text_input
      sg <- input$sugYear

      if (!is.null(ti) & !is.null(sg) & nchar(ti) > 0) {
        cat("Attempting to create Python environment.\n")
        use_or_create_env()

        df <- read_csv("..\\data\\UBCO_Course_Calendar.csv",
                       locale = locale(encoding = "ISO-8859-1")) |>
          filter(!is.na(`Course Description`))

        cat("Starting doc sim.\n")
        lsaDocSim.out <- lsaDocSim(ti, sg, df)
        lsaDocSim.out
      }
    }, ignoreNULL = FALSE)

    # Render the course recommendation UI
    output$courseRecSim <- renderUI({
      cat("Loading course suggestion data for display.\n")
      data <- reactive_data()
      cat("Data loaded.\n")
      if (!is.null(data)) {
        tagList(
          tableOutput("table_view"),
          checkboxInput(
            "checkbox1rec",
            paste(data$Course.Name[1], data$Course.Code[1]),
            value = FALSE
          ),
          checkboxInput(
            "checkbox2rec",
            paste(data$Course.Name[2], data$Course.Code[2]),
            value = FALSE
          ),
          checkboxInput(
            "checkbox3rec",
            paste(data$Course.Name[3], data$Course.Code[3]),
            value = FALSE
          )
        )
      }
    })

    # Initialize a reactive value to control UI display
    values <- reactiveValues(ready = FALSE)

    observeEvent(input$submit_button_pred_course_grad, {
      cat("Loading data for grade prediction.\n")
      data <- course_pred_data()  # Loading your data
      cat("Data loaded.\n")

      # Handling user inputs
      response <- input$response_input
      predictor_input <- input$predictor_input
      predictor_input <- trimws(unlist(strsplit(predictor_input, ",")))
      predictor_input <- c(predictor_input, response)
      cat("Here is the predictor input:\n")
      cat(predictor_input)
      cat("\n")

      cat("Here is the response input:\n")
      cat(response)
      cat("\n")

      # Preparing data for prediction
      empty <- setNames(as.data.frame(matrix(
        nrow = 1,
        ncol = length(predictor_input),
        data = NA
      )), predictor_input)

      # This is just for the conference, change this later
      if (sum(
        c(
          "DATA.311",
          "STAT.230",
          "MATH.101",
          "COSC.111",
          "MATH.100"
        ) %in% predictor_input
      ) == 5) {
        load("../data/grade.RData")
        preds <- format_data_no_fail_handling.out
        cat("Empty loaded successfully\n")
      } else {
        preds <- format_data_no_fail_handling(empty, predictor_input, data)
      }

      # Running the random forest model
      cat("Starting rf\n")
      rf.out <- randomForest(as.formula(paste0(response, "~.")), data = preds)

      # Extracting importance and determining the most important course
      impor <- importance(rf.out)
      most_important_course <- rownames(impor)[which.max(impor)]

      # Preparing prediction input data
      pred_courses_scores <- input$predictor_grade_input
      pred_courses_scores <- as.numeric(unlist(strsplit(pred_courses_scores, ",")))
      out_mat <- matrix(pred_courses_scores)
      rownames(out_mat) <- predictor_input[predictor_input != response]
      out_mat <- t(out_mat)
      out_df <- as.data.frame(out_mat)

      # Predicting course grades
      predict.out <- predict(rf.out, newdata = out_df)

      # Update reactive values
      output$predicted_grades <- renderTable({
        data.frame(Course = names(predict.out),
                   Predicted_Grade = as.numeric(predict.out))
      })

      output$most_important_course_output <- renderText({
        most_important_course
      })

      # Set reactive value to TRUE to indicate that processing is complete and UI should update
      values$ready <- TRUE
    })

    # Render UI components conditionally based on the reactive value
    output$pred_grad_output <- renderUI({
      if (values$ready) {
        tagList(
          h4("Predicted Course Grades:"),
          tableOutput("predicted_grades"),
          h4("Most Important Course for Prediction:"),
          textOutput("most_important_course_output")
        )
      }
    })

    # Dataframe to output for course suggestion topic model
    output$table_view <- renderTable({
      reactive_data()
    })

    # Logic to handle adding suggested courses from topic model
    observeEvent(input$checkbox1rec, {
      data <- reactive_data()
      course_name <- data$Course.Name[1]
      course_code <- data$Course.Code[1]
      year <- data$Course.Code |> substr(1, 1) |> as.numeric()

      cat(
        "This is what wants to be added and deleted from the database thanks to suggestions 1\n"
      )
      cat("\n")
      cat(paste(course_name, course_code))
      cat("\n")
      cat(course_code)
      cat("\n")
      cat(year[1])
      cat("\n")

      if (input$checkbox1rec) {
        # Insert the course into the database if checked
        query <- sprintf(
          "INSERT INTO selected_courses (course_name, course_code, year) VALUES ('%s', '%s', %d)",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
        DBI::dbExecute(db, query)
      } else {
        # Remove the course from the database if unchecked
        query <- sprintf(
          "DELETE FROM selected_courses WHERE course_name = '%s' AND course_code = '%s' AND year = %d",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
        DBI::dbExecute(db, query)
      }
    })

    observeEvent(input$checkbox2rec, {
      data <- reactive_data()
      course_name <- data$Course.Name[2]
      course_code <- data$Course.Code[2]
      year <- data$Course.Code |> substr(1, 1) |> as.numeric()

      cat(
        "This is what wants to be added and deleted from the database thanks to suggestions 2\n"
      )
      cat("\n")
      cat(paste(course_name, course_code))
      cat("\n")
      cat(course_code)
      cat("\n")
      cat(year[1])
      cat("\n")

      if (input$checkbox2rec) {
        # Insert the course into the database if checked
        query <- sprintf(
          "INSERT INTO selected_courses (course_name, course_code, year) VALUES ('%s', '%s', %d)",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
        DBI::dbExecute(db, query)
      } else {
        # Remove the course from the database if unchecked
        query <- sprintf(
          "DELETE FROM selected_courses WHERE course_name = '%s' AND course_code = '%s' AND year = %d",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
        DBI::dbExecute(db, query)
      }
    })

    observeEvent(input$checkbox3rec, {
      data <- reactive_data()
      course_name <- data$Course.Name[3]
      course_code <- data$Course.Code[3]
      year <- data$Course.Code |> substr(1, 1) |> as.numeric()

      cat(
        "This is what wants to be added and deleted from the database thanks to suggestions 1\n"
      )
      cat("\n")
      cat(paste(course_name, course_code))
      cat("\n")
      cat(course_code)
      cat("\n")
      cat(year[1])
      cat("\n")

      if (input$checkbox3rec) {
        # Insert the course into the database if checked
        query <- sprintf(
          "INSERT INTO selected_courses (course_name, course_code, year) VALUES ('%s', '%s', %d)",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
        DBI::dbExecute(db, query)
      } else {
        # Remove the course from the database if unchecked
        query <- sprintf(
          "DELETE FROM selected_courses WHERE course_name = '%s' AND course_code = '%s' AND year = %d",
          paste(course_name, course_code),
          course_code,
          year[1]
        )
        DBI::dbExecute(db, query)
      }
    })

    # We want each launch of the app to be fresh so delete all stored courses
    # If accounts are implemented this will be removed and logins will be tracked
    onStop(function() {
      DBI::dbDisconnect(db)
      file.remove("my_courses_db2.sqlite")
    })
  }

  # Launch shiny app
  shinyApp(ui, server)
}


# Example use
front_text <- "<h4>Welcome to the Data-driven Curriculum Analytics and Design System (DCADS)!</h4>
    <p>This R Shiny application allows users to explore various data science courses offered across different academic years.</p>
    <p>You can select courses based on their year and subject code to visualize course dependencies, access detailed course information, and receive personalized course recommendations.</p>
    <p>Navigate to the 'Explorer' tab to start using the application.</p>
    <h2>Legend</h2>
    <p>This app makes use of a variety of metrics to help assess curricula in a data-driven way. At the top of the curriculum graph, and when nodes are selected, these metrics will be printed. Read below to learn more:</p>
    <ul>
      <li><strong>Delay Factor (df)</strong> - Measures the degree to which failing this course will delay graduation.</li>
      <li><strong>Blocking Factor (bf)</strong> - Measures the degree to which failing this course will block access to future courses.</li>
      <li><strong>Centrality (cf)</strong> - Measures the degree to which a course is central to a curriculum and therefore contains important concepts for the entirety of the program.</li>
      <li><strong>Structural Complexity (sc)</strong> - Measures the degree to which the course or curriculum adds to the difficulty of completing the program.</li>
    </ul>"
course_codes_key <- c(
  "MATH",
  "DATA",
  "BIOL",
  "CHEM",
  "EESC",
  "PHYS",
  "COSC",
  "ENGL",
  "APSC",
  "STAT",
  "PSYO",
  "PHIL"
)
curriculum_csv_path <-  "./inst/extdata/Example-Curriculum.csv"

dcads(
  curriculum_csv_path,
  course_codes_key,
  topic_model = NULL,
  pred_model = NULL,
  front_text = front_text
)
