# backend.R - Communication with AiR cloud backend

# ============================================
# Backend URL configuration
# ============================================

#' Get the AiR backend URL
#' @description Returns the backend URL from options, env var, or default.
#' Priority: (1) getOption("air.backend_url"), (2) AIR_BACKEND_URL env var, (3) AiR cloud default
#' @return Character string URL (no trailing slash)
#' @export
get_backend_url <- function() {
  url <- getOption("air.backend_url", default = "")
  if (nchar(url) > 0) return(sub("/$", "", url))
  url <- Sys.getenv("AIR_BACKEND_URL", unset = "")
  if (nchar(url) > 0) return(sub("/$", "", url))
  "https://air-production-95aa.up.railway.app"
}

# Build auth header if API key is available
air_auth_header <- function() {
  key <- air_get_key()
  if (nchar(key) > 0) {
    return(httr::add_headers(Authorization = paste("Bearer", key)))
  }
  NULL
}

#' POST to a backend endpoint
#' @param endpoint The endpoint path (e.g., "/ai")
#' @param body List to send as JSON body
#' @param timeout_secs Request timeout in seconds
#' @return httr response object
air_post <- function(endpoint, body, timeout_secs = 10) {
  httr::POST(
    paste0(get_backend_url(), endpoint),
    body = body,
    encode = "json",
    air_auth_header(),
    httr::timeout(timeout_secs)
  )
}

#' GET from a backend endpoint
#' @param endpoint The endpoint path (e.g., "/ping")
#' @param timeout_secs Request timeout in seconds
#' @return httr response object
air_get <- function(endpoint, timeout_secs = 5) {
  httr::GET(
    paste0(get_backend_url(), endpoint),
    air_auth_header(),
    httr::timeout(timeout_secs)
  )
}

# ============================================
# Backend communication functions
# ============================================

#' Call the AI backend
#' @param mode Either "code" or "explain"
#' @param instruction The user's instruction
#' @param selected_code Currently selected code
#' @param full_file Full file contents
#' @return Parsed JSON response from backend
call_backend <- function(mode, instruction, selected_code, full_file) {
  body <- list(
    mode = mode,
    instruction = instruction,
    selectedCode = selected_code,
    fullFile = full_file
  )

  resp <- tryCatch(
    air_post("/ai", body, timeout_secs = 60),
    error = function(e) {
      stop("Backend connection failed. Is the server running? Error: ", e$message)
    }
  )

  if (httr::http_error(resp)) {
    stop("Backend returned error: ", httr::status_code(resp))
  }

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

#' Sync current context to backend
#' @return TRUE if sync successful, FALSE otherwise
sync_context_to_backend <- function() {
  if (!has_active_document()) return(FALSE)
  
  ctx <- get_active_context()
  
  tryCatch({
    air_post("/context", list(
      selectedCode = ctx$selected_code,
      fullFile = ctx$full_file
    ), timeout_secs = 5)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' Check if backend is running
#' @return TRUE if backend responds to ping
backend_is_running <- function() {
  tryCatch({
    resp <- air_get("/ping", timeout_secs = 2)
    httr::status_code(resp) == 200
  }, error = function(e) {
    FALSE
  })
}