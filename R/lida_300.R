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

#' @export
imp_l300_worklist <- function(conn = connect_to_l300_dbi()){

  # Worklist incluye resultados de calibración, muestra y QC.

  tables <- tibble::tibble(
    table = DBI::dbListTables(conn)
  ) %>%
    dplyr::filter(stringr::str_starts(table, "TestItemInfo"))

  out <- tables %>%
    dplyr::mutate(
      data = purrr::map(
        .x = .data$table,
        .f = \(x) dplyr::collect(dplyr::tbl(conn, x))
      )
    ) %>%
    tidyr::unnest("data") %>%
    dplyr::select(-table) %>%
    unique()

  structure(out, class = c("l300_worklist", class(out)))
}
