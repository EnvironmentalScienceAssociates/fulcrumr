% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{fulcrum_all_tables}
\alias{fulcrum_all_tables}
\title{Get Fulcrum tables}
\usage{
fulcrum_all_tables(api_key = get_api_key(), col_types = NULL)
}
\arguments{
\item{api_key}{Fulcrum authentication key}

\item{col_types}{One of NULL, a cols() specification, or a string. See vignette("readr") for more details.}
}
\value{
A data frame with name, type, etc. of all available Fulcrum tables
}
\description{
Get all available Fulcrum tables
}
