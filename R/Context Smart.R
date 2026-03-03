# context_smart.R - Smart context gathering for AiR
# Collects project structure and environment info without overwhelming context

#' Get a summary of all R files in the project
#' @param max_files Maximum number of files to include
#' @return List with file summaries and contents
#' @export
get_project_files_summary <- function(max_files = 10) {
  tryCatch({
    project_root <- getwd()
    rproj_files <- list.files(project_root, pattern = "\\.Rproj$", full.names = TRUE)
    if (length(rproj_files) > 0) {
      project_root <- dirname(rproj_files[1])
    }

    r_files <- list.files(
      project_root,
      pattern = "\\.[Rr]$",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(r_files) > max_files) {
      r_files <- r_files[1:max_files]
    }

    summaries <- lapply(r_files, function(f) {
      tryCatch({
        contents <- paste(readLines(f, warn = FALSE), collapse = "\n")
        rel_path <- basename(f)
        file_info <- file.info(f)
        list(
          path = rel_path,
          contents = contents,
          lastModified = as.numeric(file_info$mtime),
          size = file_info$size
        )
      }, error = function(e) NULL)
    })

    summaries <- Filter(Negate(is.null), summaries)

    list(
      project_root = project_root,
      file_count = length(summaries),
      files = summaries
    )
  }, error = function(e) {
    air_log("[SMART] Error in get_project_files_summary: ", e$message)
    list(project_root = getwd(), file_count = 0, files = list())
  })
}

#' Map raw R class to a consistent UI type category
#' @param x A column vector
#' @return One of: "numeric", "categorical", "date", "logical", "other"
#' @keywords internal
classify_column_type <- function(x) {
  cls <- class(x)[1]
  if (cls %in% c("numeric", "integer", "double", "complex")) return("numeric")
  if (cls %in% c("factor", "character", "ordered")) return("categorical")
  if (cls %in% c("Date", "POSIXct", "POSIXlt", "difftime")) return("date")
  if (cls == "logical") return("logical")
  "other"
}

#' Get a summary of the R environment (dataframes, functions, variables)
#' @param env Environment to summarize (default: global)
#' @param max_rows Max rows to show in head() for dataframes
#' @param max_dataframes Max dataframes to include
#' @param max_columns Max columns per dataframe
#' @return List with environment summary
#' @export
get_environment_summary <- function(env = globalenv(), max_rows = 3,
                                    max_dataframes = 20, max_columns = 50) {
  tryCatch({
    obj_names <- ls(envir = env)

    if (length(obj_names) == 0) {
      return(list(dataframes = list(), functions = list(), variables = list()))
    }

    dataframes <- list()
    functions <- list()
    variables <- list()

    for (name in obj_names) {
      obj <- tryCatch(get(name, envir = env), error = function(e) NULL)
      if (is.null(obj)) next

      if (is.data.frame(obj)) {
        if (length(dataframes) >= max_dataframes) next

        col_names <- names(obj)
        total_cols <- length(col_names)
        if (total_cols > max_columns) {
          col_names <- col_names[1:max_columns]
        }

        col_meta <- lapply(col_names, function(col) {
          x <- obj[[col]]
          raw_type <- class(x)[1]
          ui_type <- classify_column_type(x)
          n_row <- nrow(obj)
          info <- list(
            name = col,
            type = raw_type,
            ui_type = ui_type,
            n_unique = length(unique(stats::na.omit(x))),
            pct_missing = round(100 * sum(is.na(x)) / n_row, 1)
          )
          if (ui_type == "numeric" && any(!is.na(x))) {
            info$min <- round(min(x, na.rm = TRUE), 2)
            info$max <- round(max(x, na.rm = TRUE), 2)
            info$median <- round(stats::median(x, na.rm = TRUE), 2)
            info$mean <- round(mean(x, na.rm = TRUE), 2)
            tryCatch({
              h <- graphics::hist(x[!is.na(x)], breaks = 20, plot = FALSE)
              info$hist_counts <- as.integer(h$counts)
              info$hist_breaks <- round(h$breaks, 4)
            }, error = function(e) NULL)
          }
          if (ui_type == "categorical") {
            tbl <- sort(table(x, useNA = "no"), decreasing = TRUE)
            top_n <- min(20, length(tbl))
            info$total_levels <- length(tbl)
            info$top_values <- lapply(seq_len(top_n), function(i) {
              list(value = names(tbl)[i], count = as.integer(tbl[i]))
            })
            info$sample_values <- paste(names(tbl)[seq_len(min(5, top_n))], collapse = ", ")
          }
          if (ui_type == "logical") {
            info$true_count <- as.integer(sum(x == TRUE, na.rm = TRUE))
            info$false_count <- as.integer(sum(x == FALSE, na.rm = TRUE))
          }
          if (ui_type == "date" && any(!is.na(x))) {
            info$min <- as.character(min(x, na.rm = TRUE))
            info$max <- as.character(max(x, na.rm = TRUE))
          }
          info
        })

        mem_bytes <- as.numeric(utils::object.size(obj))

        dataframes[[length(dataframes) + 1]] <- list(
          name = name,
          class = class(obj)[1],
          dim = dim(obj),
          columns = col_meta,
          total_columns = total_cols,
          memory_bytes = mem_bytes,
          head = utils::capture.output(print(utils::head(obj, max_rows)))
        )
      } else if (is.function(obj)) {
        functions[[length(functions) + 1]] <- list(
          name = name,
          args = names(formals(obj)),
          signature = tryCatch({
            paste0(name, "(", paste(names(formals(obj)), collapse = ", "), ")")
          }, error = function(e) paste0(name, "(...)"))
        )
      } else if (is.atomic(obj) || is.list(obj)) {
        variables[[length(variables) + 1]] <- list(
          name = name,
          class = class(obj)[1],
          length = length(obj),
          preview = tryCatch({
            if (length(obj) > 5) {
              paste(c(utils::head(obj, 5), "..."), collapse = ", ")
            } else if (length(obj) > 0) {
              paste(as.character(obj), collapse = ", ")
            } else {
              "<empty>"
            }
          }, error = function(e) "<complex object>")
        )
      }
    }

    list(
      dataframes = dataframes,
      functions = functions,
      variables = variables
    )
  }, error = function(e) {
    air_log("[SMART] Error in get_environment_summary: ", e$message)
    list(dataframes = list(), functions = list(), variables = list())
  })
}

#' Get full contents of a specific file
#' @param file_path Path to the file (relative to project root)
#' @return File contents as string, or NULL if not found
get_file_contents <- function(file_path) {
  project_root <- getwd()
  full_path <- file.path(project_root, file_path)

  if (!file.exists(full_path)) full_path <- file_path
  if (!file.exists(full_path)) return(NULL)

  tryCatch({
    paste(readLines(full_path, warn = FALSE), collapse = "\n")
  }, error = function(e) NULL)
}

#' Create a new R file
#' @param file_path Path for the new file
#' @param contents Contents to write
#' @return TRUE if successful
#' @export
create_file <- function(file_path, contents) {
  tryCatch({
    dir_path <- dirname(file_path)
    if (dir_path != "." && dir_path != "" && !dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }

    if (dirname(file_path) == ".") {
      file_path <- file.path(getwd(), file_path)
    }

    writeLines(contents, file_path)
    Sys.sleep(0.2)

    if (!file.exists(file_path)) return(FALSE)

    # Open the file in RStudio
    if (rstudioapi::isAvailable()) {
      tryCatch({
        rstudioapi::navigateToFile(file_path)
      }, error = function(e) NULL)
    }

    TRUE
  }, error = function(e) {
    air_log("[SMART] Error creating file: ", e$message)
    FALSE
  })
}

#' Build smart context for AI (combines active file, project, environment)
#' @return List ready to send to backend
#' @export
build_smart_context <- function() {
  active <- tryCatch({
    ctx <- tryCatch(
      rstudioapi::getSourceEditorContext(),
      error = function(e) rstudioapi::getActiveDocumentContext()
    )
    list(
      path = ctx$path,
      contents = paste(ctx$contents, collapse = "\n"),
      selection = if (length(ctx$selection) > 0 && nchar(ctx$selection[[1]]$text) > 0) {
        ctx$selection[[1]]$text
      } else {
        ""
      }
    )
  }, error = function(e) NULL)

  project <- get_project_files_summary(max_files = 10)
  environment <- get_environment_summary()

  last_value_preview <- tryCatch({
    lv <- .Last.value
    if (is.null(lv) || is.function(lv) || is.environment(lv)) return(NULL)
    out <- capture.output(print(lv))
    if (length(out) > 50) out <- c(head(out, 50), paste("... (", length(out) - 50, " more lines)"))
    paste(out, collapse = "\n")
  }, error = function(e) NULL)

  list(
    active_file = active,
    project = project,
    environment = environment,
    last_value = last_value_preview
  )
}

#' Send smart context to backend
#' @return TRUE if successful
#' @export
sync_smart_context <- function() {
  ctx <- build_smart_context()

  tryCatch({
    httr::POST(
      paste0(get_backend_url(), "/smart_context"),
      body = jsonlite::toJSON(ctx, auto_unbox = TRUE, null = "null"),
      httr::content_type_json(),
      air_auth_header(),
      httr::timeout(10)
    )
    TRUE
  }, error = function(e) {
    air_log("[SMART] Sync failed: ", e$message)
    FALSE
  })
}
