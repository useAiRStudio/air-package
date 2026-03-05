# backend.R - Communication with Node.js backend

#' Get the AiR backend URL
#' @description Returns the backend URL from options, env var, or default.
#' Priority: (1) getOption("air.backend_url"), (2) AIR_BACKEND_URL env var, (3) localhost:3001
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