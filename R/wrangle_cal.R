#' Wrangle calibration results imported from different instrument databases into a common data structure
#'
#' @param x data imported from an instrument database
#' @param ... additional arguments used by methods
#'
#' @return a tibble with columns 'method', 'value', and 'od'.
#' @export
wrangle_cal <- function(x, ...){
  UseMethod("wrangle_cal")
}

#' @describeIn wrangle_cal method
#' @export
wrangle_cal.l500_cal <- function(x){

  wrangled <- x %>%
    dplyr::select(
      method = Method,
      value = Reference,
      od = OD) %>%
    dplyr::mutate(
      instrument = "Lida 500"
    )

  class(wrangled) <- c("wrangled_cal", class(wrangled))

  wrangled
}

#' @describeIn wrangle_cal method
#' @export
wrangle_cal.sk_cal <- function(x){

  wrangled <- x %>%
    dplyr::select(
      method = ITEM_NAME,
      value = Value,
      od = OD) %>%
    dplyr::mutate(
      instrument = "Sekisui"
    )

  class(wrangled) <- c("wrangled_cal", class(wrangled))

  wrangled
}

#' @describeIn wrangle_cal method
#' @export
wrangle_cal.kr_cal <- function(x, instrument = c("Kroma", "Kroma Plus")){

  stopifnot('argument "instrument" is missing.' = !missing(instrument))

  instrument <- rlang::arg_match(instrument)

  wrangled <- x %>%
    dplyr::select(
      method = Name,
      value = stdValue,
      od = stdOD) %>%
    dplyr::mutate(
      instrument = .env$instrument
    )

  class(wrangled) <- c("wrangled_cal", class(wrangled))

  wrangled
}
