h_rc_interval_means <- function(x, cycles, interval = c("main", "sub")){

  x %>%
    dplyr::mutate(abs_dif = abs1 - abs2) %>%
    dplyr::filter(cycle %in% cycles) %>%
    dplyr::group_by(id_rc) %>%
    dplyr::summarise(
      wl_1 = mean(abs1),
      wl_2 = mean(abs2),
      wl_dif = mean(abs_dif)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("wl"),
      names_to = "wl",
      values_to = "od",
      names_prefix = "wl_"
    ) %>%
    dplyr::rename("{interval}" := od)
}

curves_to_od <- function(x, VR1, VR2, VS, main, sub = NULL){

  if(rlang::is_null(sub)){
    out <- h_rc_interval_means(x, cycles = main, "main") %>%
      dplyr::mutate(sub = 0, od = main)

    return(out)
  }

  f_sub <- (VR1 + VS)/(VR1 + VS + VR2)

  dplyr::left_join(
    h_rc_interval_means(x, cycles = main, "main"),
    h_rc_interval_means(x, cycles = sub, "sub"),
    by = dplyr::join_by(id_rc, wl)
  ) %>%
    dplyr::mutate(od = main - f_sub * sub)


}

h_fit_cal_l300 <- function(x){
  broom::tidy(
    lm(od ~ reference, data = x)
  )
}

h_l300_format_datetime <- function(Date, Time){
  lubridate::as_datetime(
    paste0(Date, "_", sprintf("%06d", Time))
  )
}

imp_l300_worklists <- function(conn, worklist = 1:8){
  purrr::map(
    .x = paste0("TestItemInfo", worklist),
    .f = \(x) dplyr::collect(dplyr::tbl(conn, x))
  ) %>%
    dplyr::bind_rows()
}

imp_l300_cal_current <- function(conn = connect_to_l300_dbi()){
  dplyr::tbl(conn, "HisCalItemInfo") %>%
    dplyr::filter(iSelected == 1) %>%
    dplyr::collect()
}

