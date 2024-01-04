conn_kr <- connect_to_kr_dbi()
res_kr <- imp_kr_results(conn_kr)
cal_kr <- res_kr %>%
  dplyr::filter(idArchive >= 45317) %>%
  dplyr::pull(idFactor) %>%
  unique() %>%
  imp_kr_cal()
DBI::dbDisconnect(conn_kr)

conn_sk <- connect_to_sk_dbi()
res_sk <- imp_sk_results(conn_sk)
cal_sk <- imp_sk_caldata("GLUC", conn_sk)
DBI::dbDisconnect(conn_sk)

conn_l500 <- connect_to_l500_dbi()
res_l500 <- imp_l500_results(conn_l500)
cal_l500 <- imp_l500_cal("CRP TURBI")
DBI::dbDisconnect(conn_l500)

res_all <- dplyr::bind_rows(
  wrangle_results(res_kr, "Kroma"),
  wrangle_results(res_sk),
  wrangle_results(res_l500)
) %>%
  h_clean_methods()

res_all <- dplyr::bind_rows(
  wrangle_results(res_kr, "Kroma"),
  wrangle_results(res_sk),
  wrangle_results(res_l500)
) %>%
  h_clean_methods()

cal_all <- dplyr::bind_rows(
  wrangle_cal(cal_kr, "Kroma"),
  wrangle_cal(cal_sk),
  wrangle_cal(cal_l500)
) %>%
  h_clean_methods()

#### ----


conn_sk <- connect_to_sk_dbi()

DBI::dbListTables(conn_sk)

Calib <- dplyr::tbl(conn_sk, "Calib") %>%
  dplyr::collect() %>%
  dplyr::filter(ITEM_NAME == "HbA1C-T") %>%
  dplyr::select(RDATE, STD_NO)

CalData <- dplyr::tbl(conn_sk,  "CalData") %>%
  dplyr::collect() %>%
  dplyr::filter(ITEM_NAME == "HbA1C-T") %>%
  dplyr::mutate(
    id = 1:dplyr::n()
  )

CalData_total <- CalData %>%
  dplyr::filter(IS_TOTAL == 1) %>%
  dplyr::arrange(desc(RST_DATE))

res_sk %>%
  dplyr::filter(ITEM_NAME == "CRP") %>%
  dplyr::arrange(desc(RDATE)) %>%
  head(1) %>%
  dplyr::select(RC_NO, date_result = RDATE) %>%
  dplyr::left_join(CalData_total, by = dplyr::join_by(closest(date_result > RST_DATE))) %>%
  dplyr::left_join(Calib, by = dplyr::join_by(closest(RST_DATE >= RDATE))) %>%
  dplyr::select(date_result, RST_DATE, RDATE, STD_NO)

usethis::use_version()
