h_match_methods <- function(string, method){
  stringr::str_detect(string, paste0(method,"\\s?[:digit:]*(?!.)"))
}

#' Remove trailing numbers from a character vector
#'
#' @param x a character vector
#'
#' @return A character vector without trailing numbers
#' @export
h_rm_trailing_numbers <- function(x){
  x %>%
    stringr::str_remove("\\s?[:digit:]*(?!.)") %>%
    stringr::str_remove("\\s*(?!.)")
}

#' Standardize method names from multiple instruments
#'
#' @param x WIP return value from wrangle_results()? also works with wrangled_cal()
#'
#' @return A character vector without trailing numbers
#' @export
h_clean_methods <- function(x){
  x %>%
    dplyr::mutate(raw = h_rm_trailing_numbers(.data$method)) %>%
    dplyr::left_join(
      unique(dplyr::select(methods_dictionary, -"instrument")), # He puesto comillas, a ver si funciona
      by = dplyr::join_by(raw)
    ) %>%
    dplyr::mutate(method = .data$clean) %>%
    dplyr::select(!tidyselect::all_of(c("clean", "raw"))) # He puesto tidyselect, a ver si funciona

}

#' Recalibrate results given their OD and calibrator values
#' @description
#' Given a vector of results and a vector of their respective OD, a vector of
#' old calibrator values and a vector of new calibrator values, calculate new
#' results by recalibrating with the new calibrator values. Only valid for
#' straight line (OLS regression) calibration.
#'
#' @param result vector of results
#' @param od vector of od (same length as results)
#' @param target_old vector of old calibrator values
#' @param target_new vector of new calibrator values
#'
#' @return a vector of recalibrated results
#' @export
h_recal_from_results <- function(result, od, target_old, target_new){

  cal_coef <- coef(lm(od ~ result))

  cal_data <- tibble::tibble(
    target_old = target_old,
    target_new = target_new
  ) %>%
    dplyr::mutate(
      od = cal_coef[1] + cal_coef[2]*target_old
    )

  recal_coef <- coef(lm(target_new ~ od, data = cal_data))

  recal_coef[1] + recal_coef[2]*od
}

#' @export
h_repair_space <- function(x){
  stringr::str_replace_all(x, pattern = "[[:space:]]", replacement = " ")
}


# WIP ----
