# proxy.R - Local reverse proxy for RStudio Viewer
# RStudio Viewer only opens local URLs. This starts a tiny httpuv server
# on localhost that forwards all requests to the cloud backend with auth.

.proxy_env <- new.env(parent = emptyenv())
.proxy_env$handle <- NULL
.proxy_env$port <- NULL

#' Start a local proxy to the cloud backend
#' @return The local port number
#' @keywords internal
start_local_proxy <- function() {
  if (!is.null(.proxy_env$handle)) {
    return(.proxy_env$port)
  }

  key <- air_get_key()
  backend <- get_backend_url()

  port <- httpuv::randomPort()

  app <- list(
    call = function(req) {
      # Build target URL
      path <- req$PATH_INFO
      qs <- req$QUERY_STRING
      if (!is.null(qs) && nchar(qs) > 0) {
        target_url <- paste0(backend, path, "?", qs)
      } else {
        target_url <- paste0(backend, path)
      }

      # Build headers
      req_headers <- c()
      if (nchar(key) > 0) {
        req_headers["Authorization"] <- paste("Bearer", key)
      }
      ct <- req$CONTENT_TYPE
      if (!is.null(ct) && nchar(ct) > 0) {
        req_headers["Content-Type"] <- ct
      }

      # Read body for POST/PUT/PATCH
      body_raw <- NULL
      if (req$REQUEST_METHOD %in% c("POST", "PUT", "PATCH")) {
        body_raw <- req$rook.input$read()
      }

      # Forward request to cloud backend
      resp <- tryCatch({
        httr::VERB(
          verb = req$REQUEST_METHOD,
          url = target_url,
          body = body_raw,
          httr::add_headers(.headers = req_headers),
          httr::timeout(120)
        )
      }, error = function(e) {
        return(list(
          status = 502L,
          headers = list("Content-Type" = "application/json"),
          body = charToRaw(paste0('{"error":"', e$message, '"}'))
        ))
      })

      # Error response (not an httr response object)
      if (is.list(resp) && !inherits(resp, "response")) {
        return(resp)
      }

      # Forward the response back
      resp_ct <- httr::headers(resp)[["content-type"]]
      if (is.null(resp_ct)) resp_ct <- "application/octet-stream"

      list(
        status = as.integer(httr::status_code(resp)),
        headers = list("Content-Type" = resp_ct),
        body = httr::content(resp, "raw")
      )
    }
  )

  .proxy_env$handle <- httpuv::startServer("127.0.0.1", port, app)
  .proxy_env$port <- port
  air_log("[AiR] Local proxy started on port ", port)
  port
}

#' Stop the local proxy
#' @keywords internal
stop_local_proxy <- function() {
  if (!is.null(.proxy_env$handle)) {
    httpuv::stopServer(.proxy_env$handle)
    .proxy_env$handle <- NULL
    .proxy_env$port <- NULL
  }
}
