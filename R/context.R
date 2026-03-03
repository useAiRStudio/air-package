# context.R - Active document context utilities

#' Get the current active document context from RStudio
#' Uses getSourceEditorContext() first (works even when viewer pane has focus),
#' falls back to getActiveDocumentContext().
#' @return A list with full_file and selected_code
get_active_context <- function() {
  ctx <- tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) rstudioapi::getActiveDocumentContext()
  )

  list(
    full_file = paste(ctx$contents, collapse = "\n"),
    selected_code = ctx$selection[[1]]$text,
    path = ctx$path,
    id = ctx$id
  )
}

#' Check if there's an active document open
#' @return TRUE if a document is open, FALSE otherwise
has_active_document <- function() {
  ctx <- tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) rstudioapi::getActiveDocumentContext()
  )
  !is.null(ctx$id) && ctx$id != ""
}