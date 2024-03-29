
#' Set or get Fulcrum API key
#'
#' Set or get Fulcrum API key as environmental variable in .Renviron
#'
#' @param api_key        Fulcrum authentication key
#'
#' @export
set_api_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("FulcrumKey" = api_key)
}

#' @rdname set_api_key
#' @export
get_api_key <- function() {
  api_key <- Sys.getenv("FulcrumKey")
  if (!identical(api_key, "")) {
    return(api_key)
  } else {
    stop("No API key found, please supply with `api_key` argument or with FulcrumKey env var")
  }
}

#' Get Fulcrum photo
#'
#' Submit GET request to Fulcrum Photo API to get individual photo metadata
#'
#'
#' @md
#' @param photo_id       Photo ID from Fulcrum
#' @param api_key        Fulcrum authentication key
#' @param base_url       Base URL for Fulcrum Photo API
#' @return If request is successful (i.e. the request was successfully performed
#' and a response with HTTP status code <400 was recieved), an HTTP response; otherwise throws an error.
#' @export

fulcrum_photo <- function(photo_id,
                          api_key = get_api_key(),
                          base_url = "https://api.fulcrumapp.com/api/v2/photos/") {
  httr2::request(base_url) |>
    httr2::req_url_query(token = api_key) |>
    httr2::req_url_path_append(photo_id) |>
    httr2::req_url_path_append("large.jpg") |>
    httr2::req_user_agent("fulcrumr (https://github.com/EnvironmentalScienceAssociates/fulcrumr)") |>
    httr2::req_perform()
}

#' Query Fulcrum API
#'
#' Submit GET request to Fulcrum Query API based on SQL query string
#'
#'
#' @md
#' @param query_string   SQL statement as string
#' @param api_key        Fulcrum authentication key
#' @param base_url       Base URL for Fulcrum Query API
#' @return If request is successful (i.e. the request was successfully performed
#' and a response with HTTP status code <400 was recieved), an HTTP response; otherwise throws an error.
#' @export

fulcrum_query <- function(query_string,
                          api_key = get_api_key(),
                          base_url = "https://api.fulcrumapp.com/api/v2/query/") {
  httr2::request(base_url) |>
    httr2::req_url_query(token = api_key, q = query_string) |>
    httr2::req_user_agent("fulcrumr (https://github.com/EnvironmentalScienceAssociates/fulcrumr)") |>
    httr2::req_perform()
}

#' Get Fulcrum tables
#'
#' Get all available Fulcrum tables
#'
#'
#' @md
#' @param api_key        Fulcrum authentication key
#' @param col_types      One of NULL, a cols() specification, or a string. See vignette("readr") for more details.
#' @return A data frame with name, type, etc. of all available Fulcrum tables
#' @export

fulcrum_all_tables <- function(api_key = get_api_key(), col_types = NULL) {
  fulcrum_query("SELECT * FROM tables;", api_key) |>
    httr2::resp_body_string() |>
    readr::read_csv(col_types = col_types)
}

#' Get Fulcrum table
#'
#' Get Fulcrum table with specified `table_name`
#'
#'
#' @md
#' @param table_name     Name of Fulcrum table
#' @param api_key        Fulcrum authentication key
#' @param col_types      One of NULL, a cols() specification, or a string. See vignette("readr") for more details.
#' @return A data frame for specified Fulcrum table
#' @export

fulcrum_table <- function(table_name, api_key = get_api_key(), col_types = NULL) {
  # automatically quote table_name because all nested tables with have / in name (plus likely other special characters)
  table_name_esc = paste0("\"", table_name, "\"")
  fulcrum_query(glue::glue("SELECT * FROM {table_name_esc};"), api_key) |>
    httr2::resp_body_string() |>
    readr::read_csv(col_types = col_types)
}


