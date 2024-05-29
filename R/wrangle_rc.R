#' Wrangle reaction curves imported from different instrument databases into a common data structure
#'
#' @param x data imported from an instrument database
#' @param ... additional arguments used by methods
#'
#' @return a tibble with columns 'instrument','id_rc', 'cycle', 'abs1' and 'abs2'.
#' @export
wrangle_rc <- function(x, ...){
  UseMethod("wrangle_rc")
}

#' @describeIn wrangle_rc method
#' @export
wrangle_rc.sk_rc <- function(x, ...){
  out <- x %>%
    dplyr::mutate(
      instrument = "Sekisui"
    ) %>%
    dplyr::select(
      instrument, id_rc, cycle = CYCLE, abs1 = COL1_SEL, abs2 = COL2_SEL
    )

  class(out) <- c("wrangled_rc", class(out))

  out
}
