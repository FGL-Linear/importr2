% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sekisui.R
\name{imp_sk_results}
\alias{imp_sk_results}
\title{Import results from Sekisui database.
`r lifecycle::badge('stable')`}
\usage{
imp_sk_results(conn = connect_to_sk_dbi())
}
\arguments{
\item{conn}{a connection object like the one returned from connect_to_sk_dbi}
}
\value{
a tibble of class "sk_results"
}
\description{
Import results from Sekisui database.
`r lifecycle::badge('stable')`
}
