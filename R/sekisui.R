# Use the 'Tidy Modeling with R' (https://www.tmwr.org/) book as main reference
# Use the 'R for Data Science' (https://r4ds.had.co.nz/) book as main reference
# Use the 'Advanced R' (https://adv-r.hadley.nz/) book as main reference
# Use tidyverse packages: readr, ggplot2, dplyr, tidyr
# Avoid explanations unless requested by user, expecting code only
# For any line that is not code, prefix with a: #
# DO NOT use Markdown for the code

# These functions are used to import data from Sekisui SK500 database.
# sk is the prefix for Sekisui functions.
# imp is the prefix for import functions.
# rc is the prefix for reaction curve functions.

#' Import results from Sekisui database.
#' `r lifecycle::badge('stable')`
#'
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#'
#' @return a tibble of class "sk_results"
#' @export
imp_sk_results <- function(conn = connect_to_sk_dbi()){

  results <- dplyr::collect(dplyr::tbl(conn, "ResultLog"))

  structure(results, class = c("sk_results", class(results)))
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

   structure(out, class = c("sk_cal", class(out)))

}

#' Compute a mean like sekisui SK500 (drop the farthest value from the mean)
#' `r lifecycle::badge('stable')`
#'
#' @param x a vector of numbers
#'
#' @return A single number.
#' @export
fct_meanSek <- function(x){
  # This function is used to compute the mean of three replicates after
  # dropping the value that is farthest from the mean.

  if(length(x) == 1) { return( x ) }

  x_dif = abs(x - mean(x))

  mean(x[x_dif != max( x_dif )])

}

#' Convert ITEM_NAME to ITEM_NO for Sekisui SK500 database
#' `r lifecycle::badge('experimental')`
#'
#' @param item_names Describir...
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#'
#' @return an integer vector of ITEM_NO
#' @export
h_sk_item_name_to_no <- function(item_names, conn = connect_to_sk_dbi()){
  dplyr::tbl(conn, "ITEMPARA") %>%
    dplyr::filter(ITEM_NAME %in% item_names) %>%
    dplyr::pull(ITEM_NO)
}

#' Join the RC imported data from Sekisui to sk_results and generates an id_rc
#' `r lifecycle::badge('experimental')`
#'
#' @param x RC imported data from Sekisui.
#' @param sk_res object of class "sk_results".
#'
#' @return x with a new column of generated "id_rc"
#' @export
h_sk_join_rc_res <- function(x, sk_res){
  x %>%
    dplyr::left_join(sk_res, by = dplyr::join_by(RC_NO)) %>%
    dplyr::select("RC_NO", "CYCLE", "COL1_SEL", "COL2_SEL", "RDATE") %>%
    dplyr::mutate(
      date = lubridate::as_date(RDATE)
    ) %>%
    tidyr::unite(id_rc, RC_NO, date, remove = FALSE) %>%
    dplyr::select(-date)
}

#' Import reaction curves from Sekisui SK500
#' `r lifecycle::badge('experimental')`
#'
#' @param x Describir...
#'
#' @return a tibble of class "sk_rc".
#' @export
imp_sk_rc <- function(x, conn = connect_to_sk_dbi(), ...){
  UseMethod("imp_sk_rc")
}

#' @describeIn imp_sk_rc method
#' @export
imp_sk_rc.numeric <- function(x, conn = connect_to_sk_dbi()){ # Numeric porque x es un vector de RC_NO

  sk_res <- imp_sk_results() %>%
    dplyr::filter(RC_NO %in% x)

  out <- dplyr::tbl(conn, "RC_OD") %>%
    dplyr::filter(RC_NO %in% x) %>%
    dplyr::collect() %>%
    h_sk_join_rc_res(sk_res)

  if(FALSE){
    # For future wrangle
    out %>%
      dplyr::rename(cycle = CYCLE, abs1 = COL1_SEL, abs2 = COL2_SEL)

  }

  struc <- structure(out, class = c("sk_rc", class(out)))

  out

}

#' @describeIn imp_sk_rc method
#' @export
imp_sk_rc.sk_results <- function(x, conn = connect_to_sk_dbi()){

  out <- dplyr::tbl(conn, "RC_OD") %>%
    dplyr::filter(RC_NO %in% local(x$RC_NO)) %>%
    dplyr::collect() %>%
    h_sk_join_rc_res(x)

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

  item_no <- x$ITEM_NO

  CalData <- dplyr::tbl(conn,  "CalData") %>%
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

  structure(out, class = c("sk_cal_rep", class(out)))

  out
}


# WIP ----

#' Import the item parameters from Sekisui database
#'
#' @param conna connection object like the one returned from connect_to_sk_dbi
#'
#' @return a tibble of class "sk_items"
#' @export
imp_sk_items <- function(conn = connect_to_sk_dbi()){

  tbl_ITEMPARA <- dplyr::collect(dplyr::tbl(conn, "ITEMPARA"))
  tbl_MasWavelength <- dplyr::collect(dplyr::tbl(conn, "MasWavelength")) %>%
    dplyr::mutate(Comment = stringr::str_remove(Comment, "nm"))
  tbl_MasCalibrationMethod <- dplyr::collect(dplyr::tbl(conn, "MasCalibrationMethod"))

  out <- tbl_ITEMPARA %>%
    dplyr::left_join(
      dplyr::select(tbl_MasWavelength, COLOR_1 = WavelengthID, wl1 = Comment),
      by = dplyr::join_by(COLOR_1)
    ) %>%
    dplyr::left_join(
      dplyr::select(tbl_MasWavelength, COLOR_2 = WavelengthID, wl2 = Comment),
      by = dplyr::join_by(COLOR_2)
    ) %>%
    dplyr::left_join(
      dplyr::select(tbl_MasCalibrationMethod, STD_NAME = CalibrationMethodID, calibration_method = Comment),
      by = dplyr::join_by(STD_NAME)
    ) %>%
    dplyr::left_join(
      tibble::tibble(
        TYPE = 1:2,
        reaction_type = c("Endpoint", "Rate")
      ),
      by = dplyr::join_by(TYPE)
    ) %>%
    dplyr::left_join(
      tibble::tibble(
        BLANK_MODE = 0:1,
        blank_type = c("Water", "Reagent")
      ),
      by = dplyr::join_by(BLANK_MODE)
    )

  structure(out, class = c("sk_items", class(out)))

}
