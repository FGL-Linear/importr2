#' Import results from Sekisui database.
#' `r lifecycle::badge('stable')`
#'
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#'
#' @return a tibble of class "sk_results"
#' @export
imp_sk_results <- function(conn = connect_to_sk_dbi()){

  results <- dplyr::collect(dplyr::tbl(conn, "ResultLog"))

  class(results) <- c("sk_results", class(results))

  results
}

#' Import calibration data from Sekisui for given methods.
#' `r lifecycle::badge('experimental')`
#'
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#' @param item_names A character vector of ITEM_NAMEs as found in Sekisui database.
#'
#' @return a tibble of class "sk_cal"
#' @export
imp_sk_caldata <- function(item_names, conn = connect_to_sk_dbi()){

  cal_od <- dplyr::tbl(conn, "Calib") %>%
    dplyr::collect() %>%
    dplyr::filter(ITEM_NAME %in% item_names, HIST_NO == 5) %>%
    dplyr::select(
      ITEM_NO,
      dplyr::ends_with("OD")
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("OD"),
      names_to = "CAL",
      values_to = "OD",
      names_transform = \(x) 1 + as.integer(
        stringr::str_replace(
          stringr::str_remove_all(x, pattern = "STD|OD|_"),
          "BLK", "0")
      )
    ) %>%
    tidyr::drop_na()


  cal_value <- dplyr::tbl(conn, "ITEMPARA") %>%
    dplyr::collect() %>%
    dplyr::filter(ITEM_NO %in% unique(cal_od$ITEM_NO)) %>%
    dplyr::select(
      ITEM_NAME, ITEM_NO,
      STD0 = DENSITY_B,
      dplyr::starts_with("STD"),
      -STD_NAME
    )%>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("STD"),
      names_to = "CAL",
      values_to = "Value",
      names_prefix = "STD",
      names_transform = \(x) 1 + as.integer(x)
    ) %>%
    tidyr::drop_na()


   out <- dplyr::left_join(cal_od, cal_value, by = dplyr::join_by(ITEM_NO, CAL))

   class(out) <- c("sk_cal", class(out))

   out
}


#' Compute a mean like sekisui SK500 (drop one, mean of the rest)
#' `r lifecycle::badge('stable')`
#'
#' @param x a vector of numbers
#'
#' @return A single number.
#' @export
fct_meanSek <- function(x){

  if(length(x) == 1) { return( x ) }

  x_dif = abs(x - mean(x))

  mean(x[x_dif != max( x_dif )])

}
