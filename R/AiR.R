#' @title AiR - AI Assistant for RStudio
#' @description
#' An AI-powered editing assistant that runs inside RStudio.
#'
#' @section Quick Start:
#' \enumerate{
#'   \item Start the Node.js backend: \code{node server.js}
#'   \item Load the package: \code{devtools::load_all()}
#'   \item Start the listener: \code{rstudioai_start_listener()}
#'   \item Open the panel: \code{rstudioai_open_panel()}
#' }
#'
#' @docType package
#' @name rstudioai
NULL

#' Quick setup - start listener and open panel
#' @export
air_start <- function() {
  if (!backend_is_running()) {
    stop("Backend not running. Start it with: node server.js")
  }
  rstudioai_start_listener()
  Sys.sleep(0.5)
  rstudioai_open_panel()
  message("AiR is ready!")
}

#' Stop everything
#' @export
air_stop <- function() {
  rstudioai_stop_listener()
  stop_local_proxy()
  clear_queue()
  message("AiR stopped.")
}
