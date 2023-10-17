#' Import results from Kroma or Kroma Plus database.
#'
#' @param conn a connection object like the one returned from connect_to_kr_dbi
#'
#' @return a tibble
imp_kr_results <- function(conn = connect_to_kr_dbi()){

  results <- dplyr::collect(dplyr::tbl(conn, "AnalisysArchive"))

  class(results) <- c("kr_results", class(results))

  results
}
