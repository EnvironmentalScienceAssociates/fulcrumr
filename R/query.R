
#' Set or get Fulcrum API key
#'
#' Set or get Fulcrum API key as environmental variable in .Renviron
#'
#' @md
#'
#' @export
set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("FulcrumKey" = key)
}

#' @rdname set_api_key
#' @export
get_api_key <- function() {
  key <- Sys.getenv("FulcrumKey")
  if (!identical(key, "")) {
    return(key)
  } else {
    stop("No API key found, please supply with `api_key` argument or with FulcrumKey env var")
  }
}

#' Query Fulcrum API
#'
#' Submit GET request to Fulcrum Query API based on SQL query string
#'
#' Returns parsed JSON body from response to Fulcrum query.
#'
#' @md
#' @param query_string   SQL statement as string
#' @param api_key        Fulcrum authentication token stored as environment variable
#' @param base_url       Base URL for Fulcrum Query API
#' @export

fulcrum_query <- function(query_string, api_key = get_api_key(), base_url = "https://api.fulcrumapp.com/api/v2/query/") {
  httr2::request(base_url) %>%
    httr2::req_url_query(token = api_key, q = query_string) %>%
    httr2::req_user_agent("fulcrumr (https://github.com/EnvironmentalScienceAssociates/fulcrumr)") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}

#' Get Fulcrum tables
#'
#' Get all available Fulcrum tables
#'
#' Returns parsed JSON body from response to Fulcrum query.
#'
#' @md
#' @param query_string   SQL statement as string
#' @param api_key        Fulcrum authentication token stored as environment variable
#' @export

fulcrum_all_tables <- function(query_string, api_key = get_api_key()) {
  fulcrum_query("SELECT * FROM tables;", api_key)
}
