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

#' @export
imp_l300_cal_current <- function(conn = connect_to_l300_dbi()){
  dplyr::tbl(conn, "HisCalItemInfo") %>%
    dplyr::filter(iSelected == 1) %>%
    dplyr::collect()
}

#' @export
h_l300_format_datetime <- function(Date, Time){
  lubridate::as_datetime(
    paste0(Date, "_", sprintf("%06d", Time))
  )
}

# WIP ----

#' Import the item parameters from Lida 300 database (WIP)
#'
#' @param conna connection object like the one returned from connect_to_l300_dbi
#'
#' @return a tibble of class "l300_items"
#' @export

imp_l300_items <- function(conn = connect_to_l300_dbi()){

  tbl_ItemInfo <- dplyr::collect(dplyr::tbl(conn, "ItemInfo"))

  l300_wl <- tibble::tibble(
    id_wl = 0:8,
    wl = c(NA, "340", "405", "450", "510", "546", "578", "630", "670")
  )

  l300_reaction_type <- tibble::tibble(
    iTestMethod = 0:2,
    reaction_type = c("Endpoint", "Fixed time", "Rate")
  )

  l300_cal_method <- tibble::tibble(
    iCalMethod = 0:11,
    calibration_method = c(
      "Factor",
      "Single-point Linear",
      "Multi-point Linear",
      "Multi-point Polyline",
      "Logarithmic",
      "Exponential",
      "Logistic-Log 4P",
      "Logistic-Log 5P",
      "Exponential 5P",
      "Polynomial 5P",
      "Parabola",
      "Spline"
    )
  )

  l300_blank_type <- tibble::tibble(
    iBlank = 0:4,
    blank_type = c("No", "Reagent", "Sample", "Pre-blank", "Reagent + Pre-blank")
  )

  out <- tbl_ItemInfo %>%
    dplyr::left_join(
      l300_wl,
      dplyr::join_by(iLength1 == id_wl)
    ) %>%
    dplyr::rename(wl1 = wl) %>%
    dplyr::left_join(
      l300_wl,
      dplyr::join_by(iLength2 == id_wl)
    ) %>%
    dplyr::rename(wl2 = wl) %>%
    dplyr::left_join(l300_reaction_type) %>%
    dplyr::left_join(l300_cal_method) %>%
    dplyr::left_join(l300_blank_type)

  structure(out, class = c("l300_items", class(out)))

}
#imp_l300_items()
