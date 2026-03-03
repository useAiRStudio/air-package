# state.R - Shared state and queue utilities for RStudioAI

# -----------------------------------
# Debug logging — silent by default, enable with options(air.debug = TRUE)
# -----------------------------------
air_log <- function(...) {
  if (isTRUE(getOption("air.debug"))) message(...)
}

# -----------------------------------
# Runtime state (singleton)
# -----------------------------------
if (!exists(".rstudioai_env", envir = .GlobalEnv)) {
  assign(".rstudioai_env", new.env(parent = emptyenv()), envir = .GlobalEnv)
}

.rstudioai_env <- get(".rstudioai_env", envir = .GlobalEnv)
.rstudioai_env$listener_running <- FALSE

# -----------------------------------
# Queue path (single source of truth)
# -----------------------------------
queue_path <- function() {
  file.path(path.expand("~"), ".rstudioai", "queue.json")
}

queue_dir <- function() {
  dirname(queue_path())
}

queue_exists <- function() {
  file.exists(queue_path())
}

queue_size <- function() {
  if (queue_exists()) 1 else 0
}

# -----------------------------------
# Queue operations
# -----------------------------------
queue_patches <- function(patches) {
  dir.create(queue_dir(), showWarnings = FALSE, recursive = TRUE)
  
  payload <- list(
    type = "apply_patches",
    created_at = Sys.time(),
    patches = patches
  )
  
  jsonlite::write_json(
    payload,
    queue_path(),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

clear_queue <- function() {
  if (queue_exists()) {
    tryCatch(
      file.remove(queue_path()),
      error = function(e) warning("Could not clear queue: ", e$message)
    )
  }
}

read_queue <- function() {
  if (!queue_exists()) return(NULL)
  
  tryCatch({
    txt <- readLines(queue_path(), warn = FALSE)
    if (length(txt) == 0) return(NULL)
    jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
  }, error = function(e) {
    warning("Could not read queue: ", e$message)
    NULL
  })
}