
#' Set or get Fulcrum API key
#'
#' Set or get Fulcrum API key as environmental variable in .Renviron
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
#' Returns response to Fulcrum query as a data frame
#'
#' @md
#' @param query_string   SQL statement as string
#' @param api_key        Fulcrum authentication token stored as environment variable
#' @param base_url       Base URL for Fulcrum Query API
#' @export

fulcrum_query <- function(query_string,
                          api_key = get_api_key(),
                          base_url = "https://api.fulcrumapp.com/api/v2/query/") {
  resp <- httr::GET(url = base_url, query = list("token" = api_key, "q" = query_string),
                    httr::user_agent("fulcrumr (https://github.com/EnvironmentalScienceAssociates/fulcrumr)"))
  status <- httr::http_status(resp)
  if (status$category == "Success") {
    return(httr::content(resp, as = "parsed", type = "text/csv"))
  } else {
    stop("Query unsuccessful")
  }
}

#' Get Fulcrum tables
#'
#' Get all available Fulcrum tables
#'
#' Returns data frame with name, type, etc. of all available Fulcrum tables
#'
#' @md
#' @param api_key        Fulcrum authentication token stored as environment variable
#' @export

fulcrum_all_tables <- function(api_key = get_api_key()) {
  fulcrum_query("SELECT * FROM tables;", api_key)
}

#' Get Fulcrum table
#'
#' Get Fulcrum table with specified `table_name`
#'
#' Returns data frame with data for specified Fulcrum table
#'
#' @md
#' @param table_name     Name of Fulcrum table
#' @param api_key        Fulcrum authentication token stored as environment variable
#' @export

fulcrum_table <- function(table_name, api_key = get_api_key()) {
  fulcrum_query(glue::glue("SELECT * FROM {table_name};"), api_key)
}


