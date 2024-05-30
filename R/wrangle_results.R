#' Wrangle results imported from different instrument databases into a common data structure
#'
#' @param x data imported from an instrument database
#' @param ... additional arguments used by methods
#'
#' @return a tibble with columns 'date', 'method', 'instrument', 'sample', 'result', 'cycle', 'abs1' and 'abs2'.
#' @export
wrangle_results <- function(x, ...){
  UseMethod("wrangle_results")
}

#' @export
validate_wrangled_results <- function(x){

  # Pendiente de implementar?----

  stopifnot("wrangled_results" %in% class(x))

  expected_colnames <- c("date", "method", "instrument", "sample", "od", "result",
                         "cycle", "abs1", "abs2")

  stopifnot(all(colnames(x) %in% expected_colnames))
  stopifnot(all(expected_colnames %in% colnames(x)))

  x
}

#' @describeIn wrangle_results method
#' @export
wrangle_results.l500_results <- function(x){
  wrangled <- x  %>%
    dplyr::select(
      sample = "SampleID",
      method = "Method",
      id_rc,
      od = "OD",
      result = "Result",
      date = "TestDate",
      abs1 = ABS_Prim,
      abs2 = ABS_Sec
    ) %>%
    dplyr::mutate(
      instrument = "Lida 500",
      cycle = paste0(1:77, collapse = ",")
    ) %>%
    dplyr::relocate(
      "date", "method", "instrument", "sample", "id_rc", "od", "result", "cycle", "abs1", "abs2"
    )

  class(wrangled) <- c("wrangled_results", class(wrangled))

  wrangled
}

#' @describeIn wrangle_results method
#' @export
wrangle_results.sk_results <- function(x){

  wrangled <- x %>%
    dplyr::mutate(
      instrument = "Sekisui",
      date = lubridate::as_date(.data$RDATE),
      sample = stringr::str_c(.data$date, .data$S_RND_NO, .data$SAMP_NO
                              , sep = "_"),
      id_rc = as.character(RC_NO)

    ) %>%
    dplyr::select(
      date,
      method = "ITEM_NAME",
      instrument,
      sample,
      id_rc,
      od = "ABS",
      result = "RESULT",
      cycle,
      abs1,
      abs2
    )

  class(wrangled) <- c("wrangled_results", class(wrangled))

  wrangled
}

#' @describeIn wrangle_results method
#' @param instrument either "Kroma" or "Kroma Plus"
#' @export
wrangle_results.kr_results <- function(x, instrument = c("Kroma", "Kroma Plus")){

  stopifnot('argument "instrument" is missing.' = !missing(instrument))

  instrument <- rlang::arg_match(instrument)

  wrangled <- x %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$timeStamp),
      instrument = .env$instrument,
      id_rc = as.character(.data$idArchive)
    ) %>%
    dplyr::select(
      date,
      method = "methodName",
      instrument,
      sample = "sampleBarCode",
      id_rc,
      od = od,
      result = "FinalResult"
    ) %>%
    dplyr::mutate(
      cycle = NA,
      abs1 = NA,
      abs2 = NA
    )

  class(wrangled) <- c("wrangled_results", class(wrangled))

  wrangled
}

#' @describeIn wrangle_results method
#' @param instrument either "Kroma" or "Kroma Plus"
#' @export
wrangle_results.csv_sk_res <- function(x){
# NUEVO, pendiente de testear ----
  wrangled <- x %>%
    dplyr::mutate(
      instrument = "Sekisui",
      date = lubridate::as_date(.data$`Date/Title`),
      sample = stringr::str_c(.data$date, .data$S_RND_NO, .data$SAMP_NO
                              , sep = "_"),

    ) %>%
    dplyr::select(
      date,
      method = "Test",
      instrument,
      sample,
      od = `Abs./Slope`,
      result = "Result"
    )

  class(wrangled) <- c("wrangled_results", class(wrangled))

  wrangled
}

#' @describeIn wrangle_results method
#' @param instrument either "Kroma" or "Kroma Plus"
#' @export
wrangle_results.csv_kr_res <- function(x, instrument = c("Kroma", "Kroma Plus")){

  stopifnot('argument "instrument" is missing.' = !missing(instrument))

  instrument <- rlang::arg_match(instrument)

  wrangled <- x %>%
    dplyr::mutate(
      date = lubridate::as_date(DateTime),
      instrument = .env$instrument,
      od = NA_real_
    ) %>%
    dplyr::select(
      date,
      method = "Test",
      instrument,
      sample = "Sample",
      od,
      result = "Result"
    )
  class(wrangled) <- c("wrangled_results", class(wrangled))

  wrangled
}

#' @describeIn wrangle_results method
#' @export
wrangle_results.l300_results <- function(x){
  wrangled <- x %>%
    dplyr::mutate(
      id_rc = paste0("res",lTestDate,lTestTime),
      instrument = "Lida 300",
      od = fOD,
      result = fResult,
      date = lubridate::ymd(as.character(lMadeDate)),
      cycle = paste0(1:168, collapse = ","),
      abs1 = stringr::str_remove(ABSPrim, pattern = "[,]{1,}$"),
      abs2 = stringr::str_remove(ABSSec, pattern = "[,]{1,}$"),
    ) %>%
    dplyr::select(
      date,
      method = ItemName,
      instrument,
      sample = SplID,
      id_rc,
      od,
      result,
      cycle,
      abs1,
      abs2
    ) %>%
    dplyr::mutate(
      instrument = "Lida 300"
    ) %>%
    dplyr::relocate(
      "date", "method", "instrument", "sample", "id_rc", "od", "result",
      "cycle", "abs1", "abs2"
    )

  class(wrangled) <- c("wrangled_results", class(wrangled))

  wrangled
}

#' @describeIn wrangle_results method
#' @export
wrangle_results.l300_qc <- function(x){
  wrangled <- x %>%
    dplyr::mutate(
      id_rc = paste0("qc",lTestDate,lTestTime),
      instrument = "Lida 300",
      od = NA_real_,
      result = fResult,
      date = lubridate::ymd(as.character(lMadeDate))
    ) %>%
    dplyr::select(
      date,
      method = ItemName,
      instrument,
      sample = BatchNo,
      id_rc,
      od,
      result
    ) %>%
    dplyr::mutate(
      instrument = "Lida 300"
    ) %>%
    dplyr::relocate(
      "date", "method", "instrument", "sample", "id_rc", "od", "result"
    )

  class(wrangled) <- c("wrangled_results", class(wrangled))

  wrangled
}
