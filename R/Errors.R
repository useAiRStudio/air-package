# errors.R - Error detection and reporting for AiR
.air_error_state <- new.env(parent = emptyenv())
.air_error_state$installed <- FALSE
.air_error_state$last_reported <- ""
.air_error_state$last_render_output <- ""
.air_error_state$last_render_error <- ""

#' Report an error to the AiR backend
#' @param error The error message
#' @param call The call that caused the error
#' @param file The file where the error occurred
#' @param line The line number
#' @param traceback The full traceback
#' @export
report_error_to_air <- function(error, call = NULL, file = NULL, line = NULL, traceback = NULL) {
  # Avoid reporting same error multiple times
  error_str <- as.character(error)
  if (error_str == .air_error_state$last_reported) {
    return(invisible(NULL))
  }
  .air_error_state$last_reported <- error_str
  
  tryCatch({
    air_post("/report_error", list(
      error = error_str,
      call = if (!is.null(call)) as.character(call) else "",
      file = if (!is.null(file)) as.character(file) else "",
      line = if (!is.null(line)) as.integer(line) else NULL,
      traceback = if (!is.null(traceback)) paste(traceback, collapse = "\n") else ""
    ), timeout_secs = 2)
    air_log("[AiR] Error reported to assistant")
  }, error = function(e) {
    # Silently fail - don't interrupt user
  })
  invisible(NULL)
}

#' Install a global error handler that reports to AiR
#' @export
install_air_error_handler <- function() {
  if (.air_error_state$installed) {
    air_log("AiR error handler already installed.")
    return(invisible(TRUE))
  }
  
  options(error = function() {
    err <- geterrmessage()
    call_info <- tryCatch(sys.call(-1), error = function(e) NULL)
    
    # Capture traceback
    tb <- tryCatch({
      capture.output(traceback())
    }, error = function(e) NULL)
    
    # Try to get source location
    src_file <- NULL
    src_line <- NULL
    tryCatch({
      for (i in seq_len(sys.nframe())) {
        call <- sys.call(i)
        srcref <- attr(call, "srcref")
        if (!is.null(srcref)) {
          src_file <- attr(srcref, "srcfile")$filename
          src_line <- srcref[1]
          break
        }
      }
    }, error = function(e) NULL)
    
    report_error_to_air(
      error = err, 
      call = call_info,
      file = src_file,
      line = src_line,
      traceback = tb
    )
    
    # Also capture full output context so the fixer has everything
    capture_output_context()
  })
  
  .air_error_state$installed <- TRUE
  message("AiR error handler installed. Errors will be reported automatically.")
  invisible(TRUE)
}

#' Remove the AiR error handler
#' @export
remove_air_error_handler <- function() {
  options(error = NULL)
  .air_error_state$installed <- FALSE
  message("AiR error handler removed.")
  invisible(TRUE)
}

#' Auto-install error handler when starting listener
#' @keywords internal
auto_install_error_handler <- function() {
  if (!.air_error_state$installed) {
    install_air_error_handler()
  }
}

#' Core render capture — shared by air_knit() and execute_re_knit()
#' @param input_file Path to .Rmd file
#' @param output_format Output format (NULL for default)
#' @param ... Additional arguments passed to rmarkdown::render()
#' @return list(output = character, error = character or NULL, result = path or NULL)
#' @keywords internal
capture_render <- function(input_file, output_format = NULL, ...) {
  render_output <- ""
  render_error <- NULL
  render_result <- NULL
  output_con <- textConnection("render_log_cap", "w", local = FALSE)
  sink_active <- FALSE

  tryCatch({
    withCallingHandlers(
      {
        sink(output_con, type = "output")
        sink(output_con, type = "message")
        sink_active <- TRUE
        on.exit({
          if (sink_active) {
            tryCatch(sink(type = "message"), error = function(x) NULL)
            tryCatch(sink(type = "output"), error = function(x) NULL)
            sink_active <<- FALSE
          }
          tryCatch(close(output_con), error = function(e) NULL)
        }, add = TRUE)

        render_result <- rmarkdown::render(
          input = input_file,
          output_format = output_format,
          envir = new.env(parent = globalenv()),
          quiet = FALSE,
          ...
        )

        sink(type = "message")
        sink(type = "output")
        sink_active <- FALSE
        render_output <- paste(get("render_log_cap", envir = parent.frame(2)), collapse = "\n")
      },
      message = function(m) invokeRestart("muffleMessage"),
      warning = function(w) invokeRestart("muffleWarning")
    )
  }, error = function(e) {
    if (sink_active) {
      tryCatch(sink(type = "message"), error = function(x) NULL)
      tryCatch(sink(type = "output"), error = function(x) NULL)
      sink_active <<- FALSE
    }
    render_error <<- e$message
    render_output <<- tryCatch(
      paste(get("render_log_cap", envir = parent.frame(4)), collapse = "\n"),
      error = function(x) ""
    )
  })

  list(output = render_output, error = render_error, result = render_result)
}

#' Knit an Rmd file and capture ALL output for AiR error diagnosis
#' @param input Path to the .Rmd file. If NULL, uses the active document.
#' @param output_format Output format (default: "html_document")
#' @param ... Additional arguments passed to rmarkdown::render()
#' @export
air_knit <- function(input = NULL, output_format = NULL, ...) {
  if (is.null(input)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      ctx <- rstudioapi::getActiveDocumentContext()
      input <- ctx$path
      if (is.null(input) || input == "") {
        stop("No file path found. Save the file first, then run air_knit().")
      }
    } else {
      stop("No input file specified and RStudio API not available.")
    }
  }

  air_log("[AiR] Knitting: ", input)
  r <- capture_render(input, output_format, ...)

  .air_error_state$last_render_output <- r$output

  tryCatch({
    air_post("/report_output", list(
      console_output = "", terminal_output = "", render_output = r$output
    ), timeout_secs = 3)

    if (!is.null(r$error)) {
      .air_error_state$last_reported <- ""
      .air_error_state$last_render_error <- r$error
      error_msg <- r$error
      quit_lines <- grep("Quitting from lines", r$output, value = TRUE)
      if (length(quit_lines) > 0) {
        error_msg <- paste(error_msg, "\n", paste(quit_lines, collapse = "\n"))
      }
      report_error_to_air(
        error = error_msg, file = input,
        traceback = c("=== RENDER LOG (last 50 lines) ===", tail(strsplit(r$output, "\n")[[1]], 50))
      )
      stop(r$error, call. = FALSE)
    }
  }, error = function(e) {
    if (!is.null(r$error)) stop(r$error, call. = FALSE)
  })

  air_log("[AiR] Knit successful: ", r$result)
  invisible(r$result)
}

# Session state capture
#' Capture and send full session context to AiR
#' @export
capture_output_context <- function() {
  tryCatch({
    console_parts <- character(0)
    
    # 1. Last error + traceback (the basics)
    last_err <- geterrmessage()
    if (nchar(last_err) > 0) {
      console_parts <- c(console_parts, 
                         "=== LAST ERROR ===",
                         last_err
      )
      
      tb <- tryCatch(capture.output(traceback()), error = function(e) NULL)
      if (!is.null(tb) && length(tb) > 0 && !all(tb == "No traceback available")) {
        console_parts <- c(console_parts, "", "=== TRACEBACK ===", tb)
      }
    }
    
    # 2. Loaded packages (tiny signal, huge diagnostic value)
    loaded_pkgs <- sort(.packages())
    console_parts <- c(console_parts,
                       "", "=== LOADED PACKAGES ===",
                       paste(loaded_pkgs, collapse = ", ")
    )
    
    # 3. Objects in global environment with types
    env_objects <- ls(envir = .GlobalEnv)
    if (length(env_objects) > 0) {
      obj_summaries <- vapply(env_objects, function(nm) {
        obj <- tryCatch(get(nm, envir = .GlobalEnv), error = function(e) NULL)
        if (is.null(obj)) return(paste0(nm, ": <error reading>"))
        cl <- paste(class(obj), collapse = "/")
        if (is.data.frame(obj)) {
          return(paste0(nm, ": data.frame [", nrow(obj), " x ", ncol(obj), "]"))
        } else if (is.function(obj)) {
          args <- tryCatch(paste(names(formals(obj)), collapse = ", "), error = function(e) "")
          return(paste0(nm, ": function(", args, ")"))
        } else if (is.vector(obj) && length(obj) <= 5) {
          return(paste0(nm, ": ", cl, " = ", paste(head(obj, 5), collapse = ", ")))
        } else {
          return(paste0(nm, ": ", cl, " [length ", length(obj), "]"))
        }
      }, character(1))
      
      # Limit to first 30 objects
      if (length(obj_summaries) > 30) {
        obj_summaries <- c(head(obj_summaries, 30), paste("... and", length(obj_summaries) - 30, "more"))
      }
      
      console_parts <- c(console_parts,
                         "", "=== GLOBAL ENVIRONMENT ===",
                         obj_summaries
      )
    }
    
    # 4. R version (occasionally helpful for compatibility issues)
    console_parts <- c(console_parts,
                       "", paste("R version:", R.version.string)
    )
    
    # 5. Capture .Last.value — the result of the last expression the user ran
    #    This is what lets the AI reference actual numbers from the user's output
    tryCatch({
      last_val <- .Last.value
      if (!is.null(last_val) && !is.function(last_val) && !is.environment(last_val)) {
        last_val_output <- tryCatch({
          out <- capture.output(print(last_val))
          # Truncate to last 80 lines max
          if (length(out) > 80) out <- c("... (truncated)", tail(out, 80))
          paste(out, collapse = "\n")
        }, error = function(e) NULL)

        if (!is.null(last_val_output) && nchar(last_val_output) > 0 && nchar(last_val_output) < 5000) {
          console_parts <- c(console_parts,
                             "", "=== LAST EVALUATED RESULT (.Last.value) ===",
                             paste("Class:", paste(class(last_val), collapse = "/")),
                             last_val_output
          )
        }
      }
    }, error = function(e) NULL)

    # Build final strings
    console_output <- paste(console_parts, collapse = "\n")

    # Include stored render output if we have it
    render_output <- ""
    if (nchar(.air_error_state$last_render_output) > 0) {
      render_output <- .air_error_state$last_render_output
    }
    
    # Send to backend
    air_post("/report_output", list(
      console_output = console_output,
      terminal_output = "",
      render_output = render_output
    ), timeout_secs = 2)
    
  }, error = function(e) {
    # Silently fail — never interrupt the user
  })
  invisible(NULL)
}

#' Check and report the last R error to AiR
#' @export
check_and_report_last_error <- function() {
  err <- geterrmessage()
  if (nchar(err) == 0 || err == .air_error_state$last_reported) return(FALSE)
  .air_error_state$last_reported <- ""
  tb <- tryCatch(capture.output(traceback()), error = function(e) NULL)
  report_error_to_air(error = err, traceback = tb)
  capture_output_context()
  TRUE
}

#' Report the last console error to AiR
#' @export
report_last_console_error <- function() {
  err <- geterrmessage()
  if (nchar(err) == 0) {
    message("No recent error found. Run this right after an error occurs.")
    return(invisible(FALSE))
  }
  .air_error_state$last_reported <- ""
  tb <- tryCatch(capture.output(traceback()), error = function(e) NULL)
  report_error_to_air(error = err, traceback = tb)
  capture_output_context()
  message("Error reported to AiR. You can now ask it to fix the error.")
  invisible(TRUE)
}

#' Manually report error text to AiR
#' @param error_text The error text you see in the console
#' @export
report_error_text <- function(error_text) {
  if (missing(error_text) || nchar(error_text) == 0) {
    message("Usage: report_error_text(\"Error: object 'x' not found\")")
    return(invisible(FALSE))
  }
  .air_error_state$last_reported <- ""
  report_error_to_air(error = error_text)
  capture_output_context()
  message("Error reported to AiR. Ask it to fix the error.")
  invisible(TRUE)
}

#' Wrapper to run code and capture any errors
#' @param expr Expression to evaluate
#' @export
air_try <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      tb <- tryCatch(capture.output(traceback()), error = function(e) NULL)
      report_error_to_air(
        error = e$message, 
        call = deparse(substitute(expr)),
        traceback = tb
      )
      capture_output_context()
      stop(e)
    }
  )
}