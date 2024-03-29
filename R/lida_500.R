#' Import results from Lida 500 database.
#'
#' @param conn a connection object like the one returned from connect_to_l500_dbi
#' @param tables the tables to import from
#'
#' @return imp_l500_worklisttest imports results currently stored in WorkListTest table.
#' imp_l500_archive imports archived results.
#' imp_l500_results can import from either or both tables.
#' @export
imp_l500_results <- function(
    conn = connect_to_l500_dbi(),
    tables = c("all", "worklist", "archive")
    ){

  tables <- rlang::arg_match(tables)

  common_cols <- c("SampleID", "Method", "Stat", "Result", "Result_Retest", "OD",
                   "Unit", "ABS_Prim", "ABS_Sec", "Blank")

  if(tables %in% c("all", "worklist")){

    wl_extra_cols <- c("End_Time", "Repeat_Index")

    data_wl <- imp_l500_worklisttest(conn)

    selected_wl <- data_wl %>%
      dplyr::select(dplyr::all_of(c(common_cols, wl_extra_cols))) %>%
      dplyr::mutate(
        TestDate = lubridate::as_date(.data$End_Time)
      ) %>%
      dplyr::select(-End_Time)
  }

  if(tables %in% c("all", "archive")){

    arch_extra_cols <- c("TestDate", "ResultIndex")

    data_archive <- imp_l500_archive(conn)

    selected_archive <- data_archive %>%
      dplyr::select(dplyr::all_of(c(common_cols, arch_extra_cols))) %>%
      dplyr::rename(Repeat_Index = "ResultIndex") %>%
      dplyr::mutate(
        TestDate = lubridate::as_date(.data$TestDate)
      )

  }

  if(tables == "worklist"){
    results <- selected_wl
  }

  if(tables == "archive"){
    results <- selected_archive
  }

  if(tables == "all"){
    results <- dplyr::bind_rows(
      selected_wl, selected_archive
    ) %>%
      dplyr::filter(!is.na(.data$OD))
  }

  out <- results %>%
    tidyr::unite("id_rc", SampleID, Repeat_Index, Method, TestDate, sep = "", remove = FALSE)

  class(out) <- c("l500_results", class(out))

  out
}

#' @rdname imp_l500_results
imp_l500_worklisttest <- function(conn = connect_to_l500_dbi()){

  tables <- tibble::tibble(
    table = DBI::dbListTables(conn)
  ) %>%
    dplyr::filter(stringr::str_starts(table, "WorkListTest"))

  tables %>%
    dplyr::mutate(
      data = purrr::map(
        .x = .data$table,
        .f = \(x) dplyr::collect(dplyr::tbl(conn, x))
      )
    ) %>%
    tidyr::unnest("data") %>%
    dplyr::select(-table) %>%
    dplyr::filter(!is.na(.data$OD)) %>%
    unique()
}

#' @rdname imp_l500_results
imp_l500_archive <- function(conn = connect_to_l500_dbi()){

  tables <- tibble::tibble(
    table = DBI::dbListTables(conn)
  ) %>%
    dplyr::filter(stringr::str_starts(table, "Archive"))

  tables %>%
    dplyr::mutate(
      data = purrr::map(
        .x = .data$table,
        .f = \(x) dplyr::collect(dplyr::tbl(conn, x))
      )
    ) %>%
    tidyr::unnest("data") %>%
    dplyr::select(-table) %>%
    unique() %>%
    dplyr::mutate(
      Result = as.double(.data$Result),
      Result_Retest = as.double(.data$Result_Retest)
    )
}


#' Import calibration results from Lida 500 database.
#'
#' @param item_names A character vector of ITEM_NAMEs as found in Lida 500 database.
#' @param conn a connection object like the one returned from connect_to_sk_dbi
#'
#' @return a tibble of class "l500_cal"
#' @export
imp_l500_cal <- function(item_names, conn = connect_to_l500_dbi()){
  out <- dplyr::tbl(conn_l500, "Calibrate") %>%
    dplyr::filter(Method %in% item_names) %>%
    dplyr::collect() %>%
    dplyr::filter(CaliNo == max(CaliNo))

  class(out) <- c("l500_cal", class(out))

  out
}


#' Format Lida 500 reaction curves
#'
#' @param ABS_Prim vector of primary wavelength absorbance
#' @param ABS_Sec vector of secondary wavelength absorbance
#'
#' @return A tibble with columns "cycle", "abs1", "abs2" and "abs_dif"
#' @export
h_l500_format_curve <- function(ABS_Prim, ABS_Sec){
  tibble::tibble(
    abs1 = as.double(stringr::str_split_1(ABS_Prim, ",")),
    abs2 = as.double(stringr::str_split_1(ABS_Sec, ","))
  ) %>%
    dplyr::mutate(
      abs2 = tidyr::replace_na(.data$abs2, 0),
      cycle = 1:dplyr::n(),
      abs_dif = .data$abs1 - .data$abs2
    )
}

