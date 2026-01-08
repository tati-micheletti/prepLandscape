checkLinkExists <- function(url, 
                            response = c("error", "warning")) {
  Require::Require("httr2")
  resp <- tryCatch(
    request(url) |>
      req_headers(`Range` = "bytes=0-0") |>
      req_perform(),
    error = function(e) NULL
  )
  
  exists <- if (is.null(resp)) FALSE else TRUE
  status_ok <- resp_status(resp) < 400
  type_ok   <- grepl("application/zip", resp_header(resp, "Content-Type"))
  exists <- status_ok && type_ok
  
  if (all(response == "error",
          !isTRUE(exists))) 
    stop(paste0("The url ", url," does not seem to exist.",
                "To convert this error into a warning, please pass response = 'warning'",
                "to checkLinkExists"))
  if (all(response == "warning",
          !isTRUE(exists))) 
    warning(paste0("The url ", url,
                   " does not seem to exist, ignoring this file", immediate. = TRUE))
  return(exists)
}
