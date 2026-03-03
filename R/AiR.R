# AiR.R - Main entry point for AiR package

#' @title AiR - AI Assistant for RStudio
#' @description
#' An AI-powered coding assistant that runs inside RStudio.
#' Fix errors, generate EDA reports, and edit code — all from
#' inside your RStudio Viewer pane.
#'
#' @section Quick Start:
#' \enumerate{
#'   \item Log in: \code{air_login()}
#'   \item That's it! AiR starts automatically after login.
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{air_login()}}{Log in and start AiR}
#'   \item{\code{air_start()}}{Start the AiR panel (called automatically after login)}
#'   \item{\code{air_stop()}}{Stop AiR}
#' }
#'
#' @docType package
#' @name AiR
NULL

#' Start AiR - open the panel and begin listening
#' @export
air_start <- function() {
  if (!backend_is_running()) {
    stop("Cannot reach AiR backend. Check your internet connection or run air_login() first.")
  }

  rstudioai_start_listener()
  Sys.sleep(0.5)
  rstudioai_open_panel()

  message("AiR is ready!")
}

#' Stop AiR
#' @export
air_stop <- function() {
  rstudioai_stop_listener()
  stop_local_proxy()
  clear_queue()
  message("AiR stopped.")
}
