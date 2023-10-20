# ex_kr_results ----
conn_kr <- connect_to_kr_dbi()
res_kr <- imp_kr_results(conn_kr)
DBI::dbDisconnect(conn_kr)

ex_kr_results <- head(res_kr, 100)
saveRDS(ex_kr_results, testthat::test_path("fixtures", "ex_kr_results.rds"))

ex_kr_wrangled <- wrangle_results(ex_kr_results, "Kroma")
ex_kp_wrangled <- wrangle_results(ex_kr_results, "Kroma Plus")
saveRDS(ex_kr_wrangled, testthat::test_path("fixtures", "ex_kr_wrangled.rds"))
saveRDS(ex_kp_wrangled, testthat::test_path("fixtures", "ex_kp_wrangled.rds"))

# ex_sk_results ----
conn_sk <- connect_to_sk_dbi()
res_sk <- imp_sk_results(conn_sk)
DBI::dbDisconnect(conn_sk)

ex_sk_results <- head(res_sk, 100)
saveRDS(ex_sk_results, testthat::test_path("fixtures", "ex_sk_results.rds"))

ex_sk_wrangled <- wrangle_results(ex_sk_results)
saveRDS(ex_sk_wrangled, testthat::test_path("fixtures", "ex_sk_wrangled.rds"))

# ex_l500_results ----
conn_l500 <- connect_to_l500_dbi()
res_l500 <- imp_l500_results(conn_l500)
DBI::dbDisconnect(conn_l500)

ex_l500_results <- head(res_l500, 100)
saveRDS(ex_l500_results, testthat::test_path("fixtures", "ex_l500_results.rds"))

ex_l500_wrangled <- wrangle_results(ex_l500_results)
saveRDS(ex_l500_wrangled, testthat::test_path("fixtures", "ex_l500_wrangled.rds"))
