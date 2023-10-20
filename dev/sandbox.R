conn_kr <- connect_to_kr_dbi()
res_kr <- imp_kr_results(conn_kr)
DBI::dbDisconnect(conn_kr)

conn_sk <- connect_to_sk_dbi()
res_sk <- imp_sk_results(conn_sk)
DBI::dbDisconnect(conn_sk)

conn_l500 <- connect_to_l500_dbi()
res_l500 <- imp_l500_results(conn_l500)
DBI::dbDisconnect(conn_l500)

res_all <- dplyr::bind_rows(
  wrangle_results(res_kr, "Kroma"),
  wrangle_results(res_sk),
  wrangle_results(res_l500)
) %>%
  h_clean_methods()

my_data <- readRDS("C:/Users/fguerrero/OneDrive - Linear Chemicals,SLU/Documentos/R/Analysis notebooks/hdl mtd/hdl mtd/run1.rds")

results <- my_data$L500_WorkListTest

# h_l500_format_curve(results$ABS_Prim, results$ABS_Sec) #No va, hay que usar purrr

class(wr_kp)
