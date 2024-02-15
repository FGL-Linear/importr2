#' @export
imp_l300_results <- function(conn = connect_to_l300_dbi()){

  out <- dplyr::collect(dplyr::tbl(conn, "PatientItemInfo"))

  structure(out, class = c("l300_results", class(out)))

}

#' @export
imp_l300_qc <- function(conn = connect_to_l300_dbi()){

  out <- dplyr::collect(dplyr::tbl(conn, "HisQCItemInfo"))

  structure(out, class = c("l300_qc", class(out)))

}

