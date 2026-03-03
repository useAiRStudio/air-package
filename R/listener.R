# listener.R - Queue listener and patch application

#' Start the RStudioAI listener
#' @param poll_ms Polling interval in milliseconds
#' @export
rstudioai_start_listener <- function(poll_ms = 500) {
  tryCatch({
    auto_install_error_handler()
  }, error = function(e) {
    air_log("[AiR] Could not auto-install error handler: ", e$message)
  })
  rstudioai_start_listener_internal(poll_ms)
}

#' Stop the RStudioAI listener
#' @export
rstudioai_stop_listener <- function() {
  rstudioai_stop_listener_internal()
}

# -----------------------------------
# Internal state for proactive capture
# -----------------------------------
.listener_state <- new.env(parent = emptyenv())
.listener_state$last_error_msg <- ""
.listener_state$last_error_time <- 0
.listener_state$last_knit_md_mtime <- 0
.listener_state$last_knit_report_time <- 0
.listener_state$last_knit_status <- ""
.listener_state$tick_count <- 0L
.listener_state$tool_busy <- FALSE
.listener_state$last_active_path <- ""
.listener_state$last_smart_sync_time <- 0

# -----------------------------------
# Internal listener implementation
# -----------------------------------
rstudioai_start_listener_internal <- function(poll_ms = 500) {
  if (.rstudioai_env$listener_running) {
    message("Listener already running.")
    return(invisible(TRUE))
  }

  .rstudioai_env$listener_running <- TRUE

  # Initialize with current error so we don't re-report stale errors from previous sessions
  .listener_state$last_error_msg <- trimws(geterrmessage())

  tick <- function() {
    if (!.rstudioai_env$listener_running) return(invisible(NULL))

    .listener_state$tick_count <- .listener_state$tick_count + 1L

    # ==============================================
    # PROACTIVE CAPTURE (every ~3 seconds = 6 ticks)
    # ==============================================
    if (.listener_state$tick_count %% 6L == 0L) {
      tryCatch({
        current_err <- trimws(geterrmessage())
        now <- as.numeric(Sys.time())
        err_is_new <- nchar(current_err) > 0 && current_err != .listener_state$last_error_msg
        err_cooldown_ok <- (now - .listener_state$last_error_time) > 30
        if (err_is_new && err_cooldown_ok) {
          .listener_state$last_error_msg <- current_err
          .listener_state$last_error_time <- now
          air_log("[CAPTURE] New error detected: ", substr(current_err, 1, 80))
          capture_output_context()
        }

        knit_md_files <- list.files(getwd(), pattern = "\\.knit\\.md$", full.names = TRUE)
        if (length(knit_md_files) > 0) {
          newest_knit <- knit_md_files[which.max(file.mtime(knit_md_files))]
          knit_mtime <- as.numeric(file.mtime(newest_knit))

          if (knit_mtime > .listener_state$last_knit_md_mtime) {
            .listener_state$last_knit_md_mtime <- knit_mtime
            html_file <- sub("\\.knit\\.md$", ".html", newest_knit)
            html_mtime <- if (file.exists(html_file)) as.numeric(file.mtime(html_file)) else 0
            knit_now <- as.numeric(Sys.time())
            knit_cooldown_ok <- (knit_now - .listener_state$last_knit_report_time) > 30

            if (html_mtime < knit_mtime) {
              if (.listener_state$last_knit_status != "failed" || knit_cooldown_ok) {
                .listener_state$last_knit_status <- "failed"
                .listener_state$last_knit_report_time <- knit_now
                rmd_file <- sub("\\.knit\\.md$", ".Rmd", newest_knit)
                air_log("[CAPTURE] Knit failure detected: ", basename(rmd_file))
                render_error <- ""
                tryCatch({
                  rmarkdown::render(rmd_file, run_pandoc = FALSE, quiet = TRUE)
                }, error = function(e) {
                  render_error <<- e$message
                })
                tryCatch({
                  air_post("/report_knit_status", list(
                    status = "failed", file = rmd_file, render_error = render_error
                  ), timeout_secs = 5)
                }, error = function(e) NULL)
              }
            } else {
              if (.listener_state$last_knit_status != "success") {
                .listener_state$last_knit_status <- "success"
                .listener_state$last_knit_report_time <- knit_now
                tryCatch({
                  air_post("/report_knit_status", list(
                    status = "success",
                    file = sub("\\.knit\\.md$", ".Rmd", newest_knit)
                  ), timeout_secs = 2)
                }, error = function(e) NULL)
              }
            }
          }
        }
      }, error = function(e) NULL)
    }

    # ==============================================
    # PROACTIVE FILE TRACKING (every ~2 seconds = 4 ticks)
    # ==============================================
    if (.listener_state$tick_count %% 4L == 2L) {
      tryCatch({
        ctx <- rstudioapi::getSourceEditorContext()
        current_path <- ctx$path
        if (!is.null(current_path) && nchar(current_path) > 0 &&
            current_path != .listener_state$last_active_path) {
          .listener_state$last_active_path <- current_path
          tryCatch({
            air_post("/update_active_file", list(path = current_path), timeout_secs = 2)
          }, error = function(e) NULL)
        }
      }, error = function(e) NULL)
    }

    # ==============================================
    # PROACTIVE SMART SYNC (every ~30 seconds)
    # ==============================================
    if (.listener_state$tick_count %% 60L == 5L) {
      now <- as.numeric(Sys.time())
      if ((now - .listener_state$last_smart_sync_time) > 25) {
        .listener_state$last_smart_sync_time <- now
        tryCatch({
          sync_smart_context()
        }, error = function(e) NULL)
      }
    }

    # ==============================================
    # TOOL REQUEST HANDLER (poll HTTP every ~1s + check local file)
    # ==============================================
    if (!.listener_state$tool_busy && .listener_state$tick_count %% 2L == 0L) {
      tool_req <- NULL

      tryCatch({
        resp <- air_get("/tool_queue", timeout_secs = 2)
        if (httr::status_code(resp) == 200) {
          body <- httr::content(resp, as = "text", encoding = "UTF-8")
          if (!is.null(body) && nchar(body) > 2) {
            parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
            if (is.null(parsed$empty)) tool_req <- parsed
          }
        }
      }, error = function(e) NULL)

      if (is.null(tool_req)) {
        tool_request_path <- file.path(Sys.getenv("HOME"), ".rstudioai", "tool_request.json")
        if (file.exists(tool_request_path)) {
          tool_req <- tryCatch(
            jsonlite::fromJSON(tool_request_path, simplifyVector = FALSE),
            error = function(e) NULL
          )
          tryCatch(file.remove(tool_request_path), error = function(e) NULL)
        }
      }

      if (!is.null(tool_req) && !is.null(tool_req$id) && !is.null(tool_req$tool)) {
        .listener_state$tool_busy <- TRUE
        air_log("[TOOL] Executing: ", tool_req$tool, " (", tool_req$id, ")")

        tool_result <- tryCatch(
          execute_tool(tool_req$tool, tool_req$args),
          error = function(e) {
            air_log("[TOOL] Execution error: ", e$message)
            list(success = FALSE, error = e$message)
          }
        )

        for (field_name in names(tool_result)) {
          if (is.character(tool_result[[field_name]]) && nchar(tool_result[[field_name]]) > 5000) {
            tool_result[[field_name]] <- substr(tool_result[[field_name]], 1, 5000)
          }
        }

        tryCatch({
          air_post("/tool_response", list(
            id = tool_req$id,
            success = isTRUE(tool_result$success),
            result = tool_result,
            error = if (!isTRUE(tool_result$success)) tool_result$error else NULL
          ), timeout_secs = 5)
          air_log("[TOOL] Response sent for: ", tool_req$id)
        }, error = function(e) {
          air_log("[TOOL] Failed to send response: ", e$message)
        })

        .listener_state$tool_busy <- FALSE
      }
    }

    # ==============================================
    # ACTION QUEUE (poll HTTP every ~1s + check local files)
    # ==============================================
    if (.listener_state$tick_count %% 2L == 1L) {
    action <- NULL

    tryCatch({
      resp <- air_get("/action_queue", timeout_secs = 2)
      if (httr::status_code(resp) == 200) {
        body <- httr::content(resp, as = "text", encoding = "UTF-8")
        if (!is.null(body) && nchar(body) > 2) {
          parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
          if (is.null(parsed$empty)) action <- parsed
        }
      }
    }, error = function(e) NULL)

    if (is.null(action)) {
      for (qfile in c("apply_queue.json", "render_queue.json", "file_queue.json")) {
        qpath <- file.path(Sys.getenv("HOME"), ".rstudioai", qfile)
        if (file.exists(qpath)) {
          action <- tryCatch(jsonlite::fromJSON(qpath, simplifyVector = FALSE), error = function(e) NULL)
          tryCatch(file.remove(qpath), error = function(e) NULL)
          if (!is.null(action)) break
        }
      }
    }

    if (!is.null(action) && !is.null(action$type)) {
      air_log("[ACTION] ", action$type)

      if (action$type == "apply_patch") {
        tryCatch({
          new_code <- action$new_code
          if (!is.null(new_code) && nchar(new_code) > 0) {
            ctx <- rstudioapi::getActiveDocumentContext()
            air_log("[APPLY] Document context: id=", ctx$id, " path=", ctx$path)
            if (!is.null(ctx$id) && ctx$id != "" && ctx$id != "#console") {
              air_log("[APPLY] Writing ", nchar(new_code), " chars to: ", ctx$path)
              rstudioapi::setDocumentContents(new_code, id = ctx$id)
              air_log("[APPLY] setDocumentContents completed")
              Sys.sleep(0.3)
              tryCatch(rstudioapi::documentSave(ctx$id), error = function(e)
                air_log("[APPLY] Could not auto-save: ", e$message))
              tryCatch(sync_smart_context(), error = function(e) NULL)
            } else {
              air_log("[APPLY] Skipped: ctx$id is empty or #console")
            }
          } else {
            air_log("[APPLY] Skipped: new_code is null or empty")
          }
        }, error = function(e) air_log("[APPLY] ERROR: ", e$message))

      } else if (action$type == "create_file") {
        tryCatch({
          create_file(action$file_path, action$contents %||% "")
        }, error = function(e) air_log("[FILE] ERROR: ", e$message))

      } else if (action$type == "edit_file") {
        tryCatch({
          writeLines(action$contents, action$file_path)
          air_log("[FILE] Edited: ", action$file_path)
        }, error = function(e) air_log("[FILE] ERROR: ", e$message))

      } else if (action$type == "capture_render") {
        tryCatch({
          ctx <- rstudioapi::getActiveDocumentContext()
          input_file <- ctx$path
          if (is.null(input_file) || input_file == "" || !grepl("\\.Rmd$", input_file, ignore.case = TRUE)) {
            rmd_files <- list.files(getwd(), pattern = "\\.Rmd$", full.names = TRUE, ignore.case = TRUE)
            if (length(rmd_files) > 0) input_file <- rmd_files[which.max(file.mtime(rmd_files))]
            else input_file <- NULL
          }
          if (!is.null(input_file)) {
            air_log("[RENDER] Re-knitting: ", input_file)
            tryCatch(rstudioapi::documentSave(ctx$id), error = function(e) NULL)
            tryCatch(air_knit(input_file), error = function(e) {
              air_log("[RENDER] Knit failed: ", e$message)
            })
          }
        }, error = function(e) air_log("[RENDER] ERROR: ", e$message))
      }
    }
    } # end action queue tick guard

    # ==============================================
    # MAIN QUEUE
    # ==============================================
    if (queue_exists()) {
      payload <- read_queue()
      clear_queue()

      if (!is.null(payload) && !is.null(payload$type)) {
        air_log("[QUEUE] ", payload$type)

        if (payload$type == "sync_context") {
          tryCatch(capture_output_context(), error = function(e) NULL)
          sync_context_to_backend()

        } else if (payload$type == "smart_sync") {
          tryCatch(capture_output_context(), error = function(e) NULL)
          tryCatch({
            sync_smart_context()
          }, error = function(e) {
            sync_context_to_backend()
          })

        } else if (payload$type == "check_error") {
          tryCatch(capture_output_context(), error = function(e) NULL)
          check_and_report_last_error()

        } else if (payload$type == "apply_patches") {
          tryCatch(
            apply_patches(payload$patches),
            error = function(e) air_log("[ERROR] applying patches: ", e$message)
          )

        } else if (payload$type == "create_file") {
          tryCatch({
            create_file(payload$file_path, payload$contents %||% "")
          }, error = function(e) air_log("[ERROR] creating file: ", e$message))

        } else if (payload$type == "edit_file") {
          tryCatch({
            writeLines(payload$contents, payload$file_path)
          }, error = function(e) air_log("[ERROR] editing file: ", e$message))

        }
      }
    }

    tool_pending <- file.exists(file.path(Sys.getenv("HOME"), ".rstudioai", "tool_request.json"))
    next_interval <- if (tool_pending) 0.1 else (poll_ms / 1000)
    later::later(tick, next_interval)
    invisible(NULL)
  }

  later::later(tick, 0.1)
  message("AiR listener started.")
  invisible(TRUE)
}

rstudioai_stop_listener_internal <- function() {
  .rstudioai_env$listener_running <- FALSE
  message("AiR listener stopped.")
  invisible(TRUE)
}

# -----------------------------------
# Patch Application (legacy)
# -----------------------------------
apply_patches <- function(patches) {
  ctx <- rstudioapi::getActiveDocumentContext()
  if (is.null(ctx$id) || ctx$id == "") return(invisible(NULL))

  doc_id <- ctx$id
  lines <- ctx$contents
  if (is.null(patches) || length(patches) == 0) return(invisible(NULL))

  patches_df <- do.call(rbind, lapply(seq_along(patches), function(i) {
    p <- patches[[i]]
    data.frame(
      start_line = as.integer(p$start_line),
      end_line = as.integer(p$end_line),
      new_text = as.character(p$new_text),
      stringsAsFactors = FALSE
    )
  }))

  ord <- order(patches_df$start_line, decreasing = TRUE)
  patches_df <- patches_df[ord, , drop = FALSE]

  for (i in seq_len(nrow(patches_df))) {
    s <- patches_df$start_line[i]
    e <- patches_df$end_line[i]
    new_text <- gsub("\\\\n", "\n", patches_df$new_text[i])
    new_lines <- strsplit(new_text, "\n", fixed = TRUE)[[1]]
    if (length(new_lines) == 0) new_lines <- character(0)

    if (s == 1 && e >= length(lines)) { lines <- new_lines; next }
    if (s > length(lines)) { lines <- new_lines; next }

    s <- max(1, s)
    e <- min(length(lines), e)
    before <- if (s > 1) lines[1:(s - 1)] else character(0)
    after <- if (e < length(lines)) lines[(e + 1):length(lines)] else character(0)
    lines <- c(before, new_lines, after)
  }

  tryCatch({
    rstudioapi::setDocumentContents(paste(lines, collapse = "\n"), id = doc_id)
  }, error = function(e) NULL)

  if (requireNamespace("styler", quietly = TRUE)) {
    tryCatch(styler::style_active_file(), error = function(e) NULL)
  }

  invisible(NULL)
}

# -----------------------------------
# Tool Executor
# -----------------------------------

#' @keywords internal
execute_tool <- function(tool_name, args) {
  switch(tool_name,
    "re_knit" = execute_re_knit(args),
    "read_file" = execute_read_file(args),
    "sync_context" = execute_sync_context(args),
    "apply_code" = execute_apply_code(args),
    "eval_code" = execute_eval_code(args),
    {
      air_log("[TOOL] Unknown tool: ", tool_name)
      list(success = FALSE, error = paste("Unknown tool:", tool_name))
    }
  )
}

#' @keywords internal
execute_re_knit <- function(args) {
  input_file <- args$file

  # Expand ~ to user's actual home directory
  if (!is.null(input_file) && nchar(input_file) > 0) {
    input_file <- path.expand(input_file)
  }

  if (is.null(input_file) || nchar(input_file) == 0) {
    tryCatch({
      ctx <- rstudioapi::getActiveDocumentContext()
      input_file <- ctx$path
    }, error = function(e) NULL)
  }

  if (is.null(input_file) || nchar(input_file) == 0 || !file.exists(input_file)) {
    return(list(success = FALSE, error = paste("File not found:", input_file %||% "(none)"), render_output = ""))
  }

  air_log("[TOOL:re_knit] Rendering: ", input_file)

  # If fixed content was provided, write it to disk AND update the editor
  if (!is.null(args$fixedContent) && nchar(args$fixedContent) > 0) {
    air_log("[TOOL:re_knit] Writing fixed content to disk (", nchar(args$fixedContent), " chars)")
    tryCatch({
      writeLines(args$fixedContent, input_file, useBytes = TRUE)
      air_log("[TOOL:re_knit] Disk write OK: ", input_file)
    }, error = function(e) air_log("[TOOL:re_knit] Disk write FAILED: ", e$message))

    # Also update the editor buffer so user sees the fix
    tryCatch({
      ctx <- rstudioapi::getActiveDocumentContext()
      if (!is.null(ctx$id) && ctx$id != "" && ctx$id != "#console") {
        rstudioapi::setDocumentContents(args$fixedContent, id = ctx$id)
        air_log("[TOOL:re_knit] Editor updated")
      }
    }, error = function(e) air_log("[TOOL:re_knit] Editor update failed: ", e$message))

  } else if (!isTRUE(args$skipSave)) {
    # Normal path: save the editor to disk before rendering
    tryCatch({
      ctx <- rstudioapi::getActiveDocumentContext()
      if (!is.null(ctx$id) && ctx$id != "" && ctx$id != "#console") {
        rstudioapi::documentSave(ctx$id)
      }
    }, error = function(e) NULL)
  }

  render_output <- ""
  render_error <- NULL
  output_con <- textConnection("render_log_capture", "w", local = FALSE)
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

        result <- rmarkdown::render(
          input = input_file,
          output_format = NULL,
          envir = new.env(parent = globalenv()),
          quiet = FALSE
        )

        sink(type = "message")
        sink(type = "output")
        sink_active <- FALSE
        render_output <- paste(get("render_log_capture", envir = parent.frame(2)), collapse = "\n")
      },
      message = function(m) invokeRestart("muffleMessage"),
      warning = function(w) invokeRestart("muffleWarning")
    )

    return(list(success = TRUE, error = "", render_output = render_output))

  }, error = function(e) {
    if (sink_active) {
      tryCatch(sink(type = "message"), error = function(x) NULL)
      tryCatch(sink(type = "output"), error = function(x) NULL)
      sink_active <<- FALSE
    }
    render_error <<- e$message
    render_output <<- tryCatch(
      paste(get("render_log_capture", envir = parent.frame(4)), collapse = "\n"),
      error = function(x) ""
    )
  })

  full_output <- render_output
  if (!is.null(render_error)) full_output <- paste(full_output, "\n\nError:", render_error)

  .air_error_state$last_render_output <- full_output
  .air_error_state$last_render_error <- render_error %||% ""

  list(success = FALSE, error = render_error %||% "Unknown render error", render_output = full_output)
}

#' @keywords internal
execute_read_file <- function(args) {
  file_path <- args$path
  if (is.null(file_path) || nchar(file_path) == 0) {
    return(list(success = FALSE, error = "No file path specified", contents = "", path = "", lines = 0))
  }
  file_path <- path.expand(file_path)
  if (!file.exists(file_path)) {
    return(list(success = FALSE, error = paste("File not found:", file_path), contents = "", path = file_path, lines = 0))
  }

  tryCatch({
    all_lines <- readLines(file_path, warn = FALSE)
    start_line <- max(1, min(as.integer(args$start_line %||% 1), length(all_lines)))
    end_line <- max(start_line, min(as.integer(args$end_line %||% length(all_lines)), length(all_lines)))
    selected_lines <- all_lines[start_line:end_line]
    list(success = TRUE, contents = paste(selected_lines, collapse = "\n"),
         path = file_path, lines = length(selected_lines), total_lines = length(all_lines))
  }, error = function(e) {
    list(success = FALSE, error = e$message, contents = "", path = file_path, lines = 0)
  })
}

#' @keywords internal
execute_sync_context <- function(args) {
  tryCatch({
    tryCatch(capture_output_context(), error = function(e) NULL)
    ctx <- build_smart_context()

    if (!is.null(ctx) && is.list(ctx)) {
      tryCatch({
        httr::POST(
          paste0(get_backend_url(), "/smart_context"),
          body = jsonlite::toJSON(ctx, auto_unbox = TRUE, null = "null"),
          httr::content_type_json(),
          air_auth_header(),
          httr::timeout(5)
        )
      }, error = function(e) {
        air_log("[TOOL:sync_context] POST failed: ", e$message)
      })
    }

    active_path <- ctx$active_file$path %||% ""
    active_len <- nchar(ctx$active_file$contents %||% "")
    n_files <- ctx$project$file_count %||% 0
    n_dfs <- length(ctx$environment$dataframes %||% list())

    list(
      success = TRUE,
      active_path = active_path,
      active_contents = ctx$active_file$contents %||% "",
      active_selection = ctx$active_file$selection %||% "",
      project_file_count = n_files,
      dataframe_count = n_dfs
    )
  }, error = function(e) {
    air_log("[TOOL:sync_context] Error: ", e$message)
    list(success = FALSE, error = e$message)
  })
}

#' @keywords internal
execute_apply_code <- function(args) {
  tryCatch({
    code <- args$code
    if (is.null(code) || nchar(code) == 0) {
      return(list(success = FALSE, error = "No code provided", path = "", chars = 0))
    }

    target_path <- args$path
    if (!is.null(target_path) && nchar(target_path) > 0) {
      target_norm <- normalizePath(target_path, mustWork = FALSE)
      if (file.exists(target_norm)) {
        tryCatch({
          rstudioapi::navigateToFile(target_norm)
          Sys.sleep(0.3)
        }, error = function(e) NULL)
      }
    }

    ctx <- rstudioapi::getActiveDocumentContext()
    if (is.null(ctx$id) || ctx$id == "" || ctx$id == "#console") {
      return(list(success = FALSE, error = "No active document", path = target_path %||% "", chars = 0))
    }

    rstudioapi::setDocumentContents(code, id = ctx$id)
    tryCatch(rstudioapi::documentSave(ctx$id), error = function(e) NULL)
    Sys.sleep(0.2)

    list(success = TRUE, path = ctx$path, chars = nchar(code))
  }, error = function(e) {
    list(success = FALSE, error = e$message, path = "", chars = 0)
  })
}

#' @keywords internal
execute_eval_code <- function(args) {
  code <- args$code
  if (is.null(code) || nchar(code) == 0) {
    return(list(success = FALSE, error = "No code provided", output = ""))
  }

  air_log("[TOOL:eval_code] Evaluating ", nchar(code), " chars")

  tryCatch({
    output <- capture.output({
      eval(parse(text = code), envir = globalenv())
    }, type = "output")

    output_str <- paste(output, collapse = "\n")
    if (nchar(output_str) > 5000) {
      output_str <- paste0(substr(output_str, 1, 5000), "\n...(truncated)")
    }

    env_snapshot <- list()
    for (var_name in ls(envir = globalenv())) {
      obj <- tryCatch(get(var_name, envir = globalenv()), error = function(e) NULL)
      if (is.null(obj)) next
      if (is.atomic(obj) && length(obj) <= 20) {
        env_snapshot[[var_name]] <- paste(as.character(obj), collapse = ", ")
      } else if (is.data.frame(obj) && nrow(obj) <= 20 && ncol(obj) <= 10) {
        env_snapshot[[var_name]] <- paste(capture.output(print(obj)), collapse = "\n")
      }
    }

    list(success = TRUE, output = output_str, env_snapshot = env_snapshot, error = "")
  }, error = function(e) {
    air_log("[TOOL:eval_code] Error: ", e$message)
    list(success = FALSE, output = "", env_snapshot = list(), error = e$message)
  })
}
