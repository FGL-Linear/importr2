#' Wrangle results imported from different instrument databases into a common data structure
#'
#' @param x data imported from an instrument database
#' @param ... additional arguments used by methods
#'
#' @return a tibble with columns 'date', 'method', 'instrument', 'sample' and 'result'.
#' @export
wrangle_results <- function(x, ...){
  UseMethod("wrangle_results")
}

#' @describeIn wrangle_results method
#' @export
wrangle_results.l500_results <- function(x){
  x %>%
    dplyr::select(
      sample = "SampleID",
      method = "Method",
      result = "Result",
      date = "TestDate"
    ) %>%
    dplyr::mutate(
      instrument = "Lida 500"
    ) %>%
    dplyr::relocate(
      "date", "method", "instrument", "sample", "result"
    )
}

#' @describeIn wrangle_results method
#' @export
wrangle_results.sk_results <- function(x){

  x %>%
    dplyr::mutate(
      instrument = "Sekisui",
      date = lubridate::as_date(.data$RDATE),
      sample = stringr::str_c(.data$date, .data$S_RND_NO, .data$SAMP_NO
                              , sep = "_"),

    ) %>%
    dplyr::select(
      "date",
      "method" = "ITEM_NAME",
      "instrument",
      "sample",
      "result" = "RESULT"
    )
}

#' @describeIn wrangle_results method
#' @param instrument either "Kroma" or "Kroma Plus"
#' @export
wrangle_results.kr_results <- function(x, instrument = c("Kroma", "Kroma Plus")){

  stopifnot('argument "instrument" is missing.' = !missing(instrument))

  instrument <- rlang::arg_match(instrument)

  x %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$timeStamp),
      instrument = .env$instrument
    ) %>%
    dplyr::select(
      date,
      method = "methodName",
      instrument,
      sample = "sampleBarCode",
      result = "FinalResult"
    )
}