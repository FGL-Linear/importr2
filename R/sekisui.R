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

#' Import current calibration data from Sekisui for given methods.
#' `r lifecycle::badge('experimental')`
#'
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#' @param item_names A character vector of ITEM_NAMEs as found in Sekisui database.
#'
#' @return a tibble of class "sk_cal"
#' @export
imp_sk_caldata <- function(item_names, conn = connect_to_sk_dbi()){

  item_no <- h_sk_item_name_to_no(item_names)

  cal_od <- dplyr::tbl(conn, "Calib") %>%
    dplyr::collect() %>%
    dplyr::filter(ITEM_NO %in% item_no) %>%
    dplyr::group_by(ITEM_NO) %>%
    dplyr::filter(HIST_NO == max(HIST_NO)) %>%
    dplyr::ungroup() %>%
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

#' Convert ITEM_NAME to ITEM_NO for Sekisui SK500 database
#' `r lifecycle::badge('experimental')`
#'
#' @param item_names
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#'
#' @return an integer vector of ITEM_NO
#' @export
h_sk_item_name_to_no <- function(item_names, conn = connect_to_sk_dbi()){
  dplyr::tbl(conn_sk, "ITEMPARA") %>%
    dplyr::filter(ITEM_NAME %in% item_names) %>%
    dplyr::pull(ITEM_NO)
}

#' Import reaction curves from Sekisui SK500
#' `r lifecycle::badge('experimental')`
#'
#' @param x Describir...
#'
#' @return a tibble of class "sk_rc".
#' @export
imp_sk_rc <- function(x, ...){
  UseMethod("imp_sk_rc")
}

#' @describeIn imp_sk_rc method
#' @export
imp_sk_rc.integer <- function(x){

  out <- dplyr::tbl(conn_sk, "RC_OD") %>%
    dplyr::filter(RC_NO %in% x) %>%
    dplyr::collect()

  if(FALSE){
    # For future wrangle
    out %>%
      dplyr::rename(cycle = CYCLE, abs1 = COL1_SEL, abs2 = COL2_SEL)

  }

  class(out) <- c("sk_rc", class(out))

  out

}

#' @describeIn imp_sk_rc method
#' @export
imp_sk_rc.sk_results <- function(x){

  out <- dplyr::tbl(conn_sk, "RC_OD") %>%
    dplyr::filter(RC_NO %in% local(x$RC_NO)) %>%
    dplyr::collect()

  class(out) <- c("sk_rc", class(out))

  out

}

#' Import the calibration replicates from Sekisui SK500's database.
#' `r lifecycle::badge('experimental')`
#'
#' @param x Object of class "sk_cal"
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#'
#' @return a tibble of class "sk_cal_rep"
#' @export
imp_sk_calrep <- function(x, conn = connect_to_sk_dbi()){

  stopifnot(inherits(x, "sk_cal"))

  CalData <- dplyr::tbl(conn_sk,  "CalData") %>%
    dplyr::filter(ITEM_NO %in% local(item_no)) %>%
    dplyr::collect()

  imp_res <- imp_sk_results(conn)

  out <- x %>%
    dplyr::left_join(
      CalData,
      dplyr::join_by(ITEM_NO == ITEM_NO, ITEM_NAME == ITEM_NAME, OD == OD_DATA)
    ) %>%
    dplyr::select(ITEM_NO, ITEM_NAME, RST_DATE, SAMP_NO) %>%
    dplyr::left_join(
      CalData,
      by = dplyr::join_by(ITEM_NO, ITEM_NAME, SAMP_NO, RST_DATE >= RST_DATE)
    ) %>%
    dplyr::group_by(ITEM_NO, ITEM_NAME, SAMP_NO) %>%
    dplyr::arrange(desc(RST_DATE.y)) %>%
    dplyr::group_modify(.f = \(x, ...) head(x, n = 4)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(IS_TOTAL == 0) %>%
    dplyr::select(
      ITEM_NO,
      RDATE = RST_DATE.y
    ) %>%
    dplyr::left_join(imp_res, dplyr::join_by(ITEM_NO, RDATE))

  class(out) <- c("sk_cal_rep", "sk_results", class(out))

  out
}
