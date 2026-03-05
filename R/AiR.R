#' @title AiR - AI Assistant for RStudio
#' @description
#' An AI-powered editing assistant that runs inside RStudio.
#' @docType package
#' @name rstudioai
NULL

#' Quick setup - start listener and open panel
#' @export
air_start <- function() {
  if (!backend_is_running()) {
    message("[AiR] Backend not reachable. Retrying in background...")
    return(invisible(FALSE))
  }
  rstudioai_start_listener()
  Sys.sleep(0.5)
  rstudioai_open_panel()
  message("AiR is ready!")
  invisible(TRUE)
}

#' Stop everything
#' @export
air_stop <- function() {
  rstudioai_stop_listener()
  stop_local_proxy()
  clear_queue()
  message("AiR stopped.")
}

#' Install auto-start hook into ~/.Rprofile
#' Called once after install so AiR launches on every RStudio session.
#' @export
air_install <- function() {
  rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")

  hook <- '
# AiR auto-start
if (interactive() && Sys.getenv("RSTUDIO") == "1") {
  tryCatch({
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(function() {
        tryCatch({
          if (requireNamespace("AiR", quietly = TRUE)) {
            AiR::air_start()
          }
        }, error = function(e) NULL)
      }, delay = 2)
    }
  }, error = function(e) NULL)
}
'

  # Check if already installed
  if (file.exists(rprofile)) {
    existing <- paste(readLines(rprofile, warn = FALSE), collapse = "\n")
    if (grepl("AiR auto-start", existing, fixed = TRUE)) {
      message("[AiR] Auto-start already installed.")
      return(invisible(TRUE))
    }
    # Append to existing .Rprofile
    cat(hook, file = rprofile, append = TRUE)
  } else {
    writeLines(hook, rprofile)
  }

  message("[AiR] Auto-start installed. AiR will launch automatically when you open RStudio.")
  invisible(TRUE)
}

#' Remove auto-start hook from ~/.Rprofile
#' @export
air_uninstall <- function() {
  rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
  if (!file.exists(rprofile)) {
    message("[AiR] No .Rprofile found.")
    return(invisible(FALSE))
  }

  lines <- readLines(rprofile, warn = FALSE)
  # Find and remove the AiR auto-start block
  start_idx <- grep("# AiR auto-start", lines, fixed = TRUE)
  if (length(start_idx) == 0) {
    message("[AiR] Auto-start not found in .Rprofile.")
    return(invisible(FALSE))
  }

  # Remove from "# AiR auto-start" to the closing "}"
  end_idx <- start_idx
  brace_depth <- 0
  found_brace <- FALSE
  for (i in start_idx:length(lines)) {
    if (grepl("\\{", lines[i])) { brace_depth <- brace_depth + sum(gregexpr("\\{", lines[i])[[1]] > 0); found_brace <- TRUE }
    if (grepl("\\}", lines[i])) brace_depth <- brace_depth - sum(gregexpr("\\}", lines[i])[[1]] > 0)
    end_idx <- i
    if (found_brace && brace_depth <= 0) break
  }

  lines <- lines[-(start_idx:end_idx)]
  # Remove trailing blank lines
  while (length(lines) > 0 && lines[length(lines)] == "") lines <- lines[-length(lines)]

  writeLines(lines, rprofile)
  message("[AiR] Auto-start removed.")
  invisible(TRUE)
}
