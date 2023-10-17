#' Import results from Sekisui database.
#'
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#'
#' @return a tibble
imp_sk_results <- function(conn = connect_to_sk_dbi()){

  results <- dplyr::collect(dplyr::tbl(conn, "ResultLog"))

  class(results) <- c("sk_results", class(results))

  results
}

