# panel.R - RStudio Viewer panel management

#' Open the AiR panel in RStudio Viewer
#' @export
rstudioai_open_panel <- function() {
  # Push full context (including dataframes) to backend on panel open
  tryCatch(sync_smart_context(), error = function(e) {
    air_log("Context sync failed, panel will still open.")
  })

  is_local <- grepl("^https?://(localhost|127\\.0\\.0\\.1)", get_backend_url())

  if (is_local) {
    # Local dev — open directly
    rstudioapi::viewer(paste0(get_backend_url(), "/ui"))
  } else {
    # Cloud backend — proxy serves static panel assets only
    # API calls go directly to backend (avoids blocking R's event loop)
    port <- start_local_proxy()
    key <- air_get_key()
    backend_enc <- utils::URLencode(get_backend_url(), reserved = TRUE)
    rstudioapi::viewer(paste0(
      "http://127.0.0.1:", port, "/ui?key=", key,
      "&backend=", backend_enc
    ))
  }
}

#' Convenience alias
#' @export
air_panel <- function() {
  rstudioai_open_panel()
}

#' Manually sync context (call before Run if needed)
#' @export
air_sync <- function() {
  if (!has_active_document()) {
    message("No active document.")
    return(invisible(FALSE))
  }
  sync_context_to_backend()
  message("Context synced.")
  invisible(TRUE)
}
