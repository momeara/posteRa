
#' @export
postera_base_url <- function(){"https://api.postera.ai/api"}


#' @export
postera_REST <- function(
  end_point,
  api_key = NULL,
  post_data = NULL,
  api_version = "v1",
  retry_attempts = 5,
  verbose = FALSE,
  ...
){

  url <- paste(postera_base_url(), api_version, end_point, "", sep = "/") %>%
    httr::parse_url() %>%
    httr::build_url()
  url_args <- list(...)

  headers = list('X-API-KEY' = api_key)

  if (verbose) {
    cat("making request:\n")
    cat("  url: ", url, "\n")
    cat("  header: X-API-KEY =", api_key, "\n")
    if (!is.null(post_data)) {
      post_args <- ""
      for (i in 1:length(post_data)) {
        elements <- stringr::str_split(post_data[i], "\\s+")[[1]]
        if (length(elements) <= 5) {
          value <- post_data[i]
        } else{
          value <- paste0(paste0(elements[1:5], collapse = " "), " ... <", length(elements) - 5, " more>")
        }
        post_args <- paste(post_args, "   ", names(post_data)[i], "=", value, "\n")
      }
      cat("  post data:\n", post_args, sep = "")
    }
  }

  succeeded <- FALSE
  for (i in 1:(retry_attempts + 1)) {
    if (i > 1) {
      cat("retrying attempt ", i, " ... \n", sep = "")
    }
    r <- NULL
    if (is.null(post_data)) {
      r <- tryCatch(
        do.call(httr::GET, args = c(list(url = url), url_args, httr::add_headers("X-API-KEY" = api_key))),
        error = function(e) {
          cat("ERROR getting data from url: url='", url, "'\n", sep = "")
          print(e)
          NULL
        })
    } else {
      r <- tryCatch({
        do.call(httr::POST, args = c(list(url = url, body = post_data, encode="form", httr::add_headers("X-API-KEY" = api_key)),  url_args))
        },
        error = function(e) {
          cat("ERROR posting to url: url='", url, "'\n", sep = "")
          print(e)
          NULL
        })
    }

    if (is.null(r) || (httr::status_code(r) < 200) || (httr::status_code(r) >= 300)) {
      cat("ERROR: url='", url, "'\n", sep = "")
      if (!is.null(post_data)) {
        cat("ERROR: post_data:\nERROR:  ",
            paste(
              paste0(names(post_data), "=", post_data),
              collapse = "\nERROR:  "),
            "\n", sep = "")
      }
      if (!is.null(r)) {
        cat("status_code='", r %>% httr::status_code(), "'\n", sep="")
      }
    } else{
      succeeded <- TRUE
      break
    }
  }
  if(!succeeded){
    cat("Failed after", retry_attempts, "attempts\n")
    stop()
  }

  contents <- r %>% httr::content("text", encoding = "UTF-8")
  if( contents == "") {
    return(tidyjson::tbl_json())
  }
  tryCatch({
    tidyjson::as.tbl_json(contents)$..JSON[[1]]$results %>%
      purrr::map_dfr(`[`) %>%
      return()
  }, error = function(e) {
    cat("ERROR parsing url='", url, "'\n", sep = "")
    print(e)
    return(tidyjson::tbl_json())
  })
}


httr::POST(
  url = 'https://api.postera.ai/api/v1/med-chem-alerts/',
  httr::add_headers("X-API-KEY" = api_key),
  body = list(smiles = smiles),
  encode="form") %>%
  httr::content("text", encoding = "UTF-8")
