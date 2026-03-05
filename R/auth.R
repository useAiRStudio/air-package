# auth.R - API key management for AiR cloud backend

.airrc_path <- function() {
  file.path(Sys.getenv("HOME"), ".airrc")
}

#' Log in to AiR cloud backend
#' @description If called with no arguments, starts a device auth flow:
#' opens a browser for login and polls until complete, then saves the API key.
#' If called with a key, saves it directly to ~/.airrc.
#' @param key Optional. An AiR API key (e.g., "air_key_xxx"). If omitted, uses device auth flow.
#' @export
air_login <- function(key = NULL) {
  # Direct key mode: save and return

  if (!is.null(key) && nchar(trimws(key)) > 0) {
    writeLines(trimws(key), .airrc_path())
    message("[AiR] API key saved to ~/.airrc")
    return(invisible(TRUE))
  }

  # Device auth flow
  api_url <- get_backend_url()

  # Step 1: Request device code
  resp <- httr::POST(
    paste0(api_url, "/auth/device/start"),
    encode = "json",
    httr::timeout(10)
  )
  if (httr::status_code(resp) != 200) {
    stop("[AiR] Could not start login. Is the backend running? (", httr::status_code(resp), ")")
  }
  body <- httr::content(resp, as = "parsed", type = "application/json")
  device_code <- body$device_code
  user_code   <- body$user_code
  verify_url  <- body$verify_url

  message("[AiR] Opening browser to log in...")
  message("[AiR] Your code is: ", user_code)
  utils::browseURL(verify_url)

  # Step 2: Poll until complete or expired (every 2s, up to 5 min)
  max_attempts <- 150  # 150 * 2s = 5 minutes
  for (i in seq_len(max_attempts)) {
    Sys.sleep(2)
    poll_resp <- tryCatch(
      httr::GET(
        paste0(api_url, "/auth/device/poll?device_code=", device_code),
        httr::timeout(10)
      ),
      error = function(e) NULL
    )
    if (is.null(poll_resp)) next

    poll_body <- httr::content(poll_resp, as = "parsed", type = "application/json")

    if (identical(poll_body$status, "complete") && !is.null(poll_body$api_key)) {
      writeLines(poll_body$api_key, .airrc_path())
      message("[AiR] Logged in successfully!")
      air_start()
      return(invisible(TRUE))
    }

    if (identical(poll_body$status, "expired")) {
      stop("[AiR] Login timed out. Run air_login() again.")
    }

    # Still pending — show a dot every 10 polls (20s)
    if (i %% 10 == 0) message("[AiR] Waiting for browser login...", appendLF = FALSE)
  }

  stop("[AiR] Login timed out after 5 minutes. Run air_login() again.")
}

#' Get the stored AiR API key
#' @description Reads the API key from ~/.airrc. Returns "" if not set.
#' @return Character string API key
#' @export
air_get_key <- function() {
  rc <- .airrc_path()
  if (!file.exists(rc)) return("")
  trimws(readLines(rc, n = 1, warn = FALSE))
}
