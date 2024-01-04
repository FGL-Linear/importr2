#' Import results from Kroma or Kroma Plus database.
#'
#' @param conn a connection object like the one returned from connect_to_kr_dbi
#'
#' @return a tibble of class "kr_results"
#' @export
imp_kr_results <- function(conn = connect_to_kr_dbi()){

  results <- dplyr::collect(dplyr::tbl(conn, "AnalisysArchive"))

  class(results) <- c("kr_results", class(results))

  results
}

#' Import calibration results from Kroma or Kroma Plus database.
#'
#' @param cal_ids an integer vector of calibration ids
#' @param conn a connection object like the one returned from connect_to_kr_dbi
#'
#' @return a tibble of class "kr_cal"
#' @export
imp_kr_cal <- function(cal_ids, conn = connect_to_kr_dbi()){

  id_method <- dplyr::tbl(conn_kr,  "TestDescription") %>%
    dplyr::select(idMethod = testIndex, Name) %>%
    dplyr::collect()

  cal_type <- dplyr::tbl(conn_kr,  "InterpolationType") %>%
    dplyr::select(interpolationType = idInterpolation,
                  cal_type = Description) %>%
    dplyr::collect()

  out <- dplyr::tbl(conn_kr,  "Factors") %>%
    dplyr::filter(idFactor %in% cal_ids) %>%
    dplyr::select(
      idFactor, idMethod, timeStamp, interpolationType,
      tidyr::starts_with("stdValue") & !tidyr::contains("Calc"),
      tidyr::starts_with("stdOD") & !tidyr::contains("Blank"),
      tidyr::starts_with("stdPosition")) %>%
    dplyr::collect() %>%
    tidyr::pivot_longer(
      cols = tidyr::starts_with("std")
    ) %>%
    tidyr::extract(
      col = "name",
      into = c("names", "cal_num"),
      regex = "([[:alpha:]]+)([[:digit:]])"
    ) %>%
    tidyr::pivot_wider(names_from = "names", values_from = "value") %>%
    dplyr::filter(.data$stdPosition != 0) %>%
    dplyr::left_join(id_method, by = dplyr::join_by(idMethod)) %>%
    dplyr::left_join(cal_type, by = dplyr::join_by(interpolationType))

  class(out) <- c("kr_cal", class(out))

  out

}
