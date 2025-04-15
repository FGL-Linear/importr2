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
items_sk <- imp_sk_items(conn_sk)
DBI::dbDisconnect(conn_sk)

conn_l500 <- connect_to_l500_dbi()
res_l500 <- imp_l500_results(conn_l500)
cal_l500 <- imp_l500_cal("CRP TURBI")
items_l500 <- imp_l500_items(conn_l500)
DBI::dbDisconnect(conn_l500)

conn_l300 <- connect_to_l300_dbi()
res_l300 <- imp_l300_results(conn_l300)
cal_l300 <- imp_l500_cal("CRP TURBI")
items_l300 <- imp_l500_items(conn_l300)
DBI::dbDisconnect(conn_l300)

res_all <- dplyr::bind_rows(
  wrangle_results(res_kr, "Kroma"),
  wrangle_results(res_sk),
  wrangle_results(res_l500),
  wrangle_results(res_l300)
) %>%
  h_clean_methods()

res_all <- dplyr::bind_rows(
  wrangle_results(res_l300)
) %>%
  h_clean_methods()

cal_all <- dplyr::bind_rows(
  wrangle_cal(cal_kr, "Kroma"),
  wrangle_cal(cal_sk),
  wrangle_cal(cal_l500)
) %>%
  h_clean_methods()

items_all <- dplyr::bind_rows(
  wrangle_items(items_sk),
  wrangle_items(items_l500)
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

#### borrador para validar calibraciones ----


res_sk <- imp_sk_results() %>%
  dplyr::filter(RDATE > "2024-01-02", S_RND_NO == 6, ITEM_NAME == "DBILDPD")



cal_sk <- imp_sk_caldata(unique(res_sk$ITEM_NAME))

res_sk_wr <- wrangle_results(res_sk)

cal_sk_wr <- wrangle_cal(cal_sk)

res_sk_wr
cal_sk_wr

reg_coef <- coef(lm(cal_sk_wr$od ~ cal_sk_wr$value))

cal_val <- res_sk_wr %>%
  dplyr::mutate(
    od_val = reg_coef[1] + reg_coef[2] * result
  )

all.equal(cal_val$od, cal_val$od_val, tolerance = 10^-4)

#### ----
####

x <- imp_csv_sk_res("C:/Users/fguerrero/OneDrive - Linear Chemicals,SLU/Documentos/R/importr/inst/extdata/sekisui_results.csv")
x <- imp_csv_kr_res("C:/Users/fguerrero/OneDrive - Linear Chemicals,SLU/Documentos/R/importr/inst/extdata/kroma_results.csv")
instrument <- "Kroma"


#### ----

wrangle_items(imp_l500_items()) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    sub_start = if(sub_start == 1){NA}else{sub_start/10},
    sub_end = if(sub_end == 3){NA}else{sub_end/10},
    main_start = if(v_r2 == 0){main_start/10 + 17 } else { main_start/10 + 46 },
    main_end = if(v_r2 == 0){main_end/10 + 17 } else { main_end/10 + 46 }
  ) %>%
  View()

wrangle_items(imp_l500_items())


#### ----
l300_res <- imp_l300_results()

wrangle_results(l300_res)

i <- 1000

N <- dim(l300_res)[1]

seconds_per_cycle <- tibble::tibble(
  n = 1:N,
  seconds = NA_real_
)

for (i in seconds_per_cycle$n) {

  l300_rc <- tibble::tibble(
    time_list = as.double(stringr::str_split_1(l300_res[i,]$TimeList, ",")),
    abs1 = as.double(stringr::str_split_1(l300_res[i,]$ABSPrim, ",")),
    abs2 = as.double(stringr::str_split_1(l300_res[i,]$ABSSec, ","))
  )

  delta_time <- tibble::tibble(
    delta_time = as.double(l300_rc$time_list[2:168]) - as.double(l300_rc$time_list[1:167])
  )

  delta_time <- delta_time %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      delta_time = if(delta_time < 0 ){delta_time + 60} else {delta_time}
    )

  seconds_per_cycle$seconds[i] <- mean(delta_time$delta_time)

}

seconds_per_cycle %>%
  dplyr::filter(seconds < 10) %>%
  dplyr::pull(seconds) %>%
  summary()

l300_rc$time <- c(0, cumsum(delta_time$delta_time), NA)

l300_rc %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(x = 1:169, y = abs1)
  ) +
  ggplot2::geom_line()

l300_rc %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(x = time, y = abs1)
  ) +
  ggplot2::geom_line()

l300_res[1,]$ABSPrim %>%
  stringr::str_remove(pattern="[,]{1,}$") %>%
  stringr::str_split(pattern = ",") %>%
  .[[1]]


# OJO PENDIENTE ----
# Durante la asignación de valores de Magnesio al STDi multiparamétrico he visto
# que no se puede calcular la OD a partir de las curvas de reacción de los Lida
# si no se incluye también el blanco de reactivo (en aquellas aplicaciones que
# lo utilizan).
#
# Ver: L:/R+D+I/Bioquimica/R+D/Estandarización de reactivos de Química Clínica/2024-05-31 - assignacion STDi Mg/asign_draft.R
# browseURL("L:/R+D+I/Bioquimica/R+D/Estandarización de reactivos de Química Clínica/2024-05-31 - assignacion STDi Mg/asign_draft.R")
# Para tener todo el contexto, se deberían importar también las curvas de reacción del blanco de reactivo...
# pero no creo que sea buena idea meterlo en la misma tabla que los resultados importados al no tener una
# relación one-to-one.
#
# Por otro lado, en la tabla de la aplicación pone el blanco de reactivo (al menos en L500, como RBlank_OD).
# Puede servir para verificar si la función que cree para calcular el blanco funciona correctamente.
#
# Aunque no cuadre de cara a importarlo a una base de datos, quizás podría mirar
# de crear funciones que importen en un solo objeto todo lo necesario para
# recalcular los resultados. Su estructura no tiene por que ser directamente
# como las tablas de una db. Por ejemplo, en vez de tener las curvas de reacción
# como strings separados por comas, podrían ser un tibble nested.



#----
dplyr::bind_rows(
  dplyr::filter(wrangle_items(imp_sk_items()), method == "MgBRtodo"),
  dplyr::filter(wrangle_items(imp_l500_items()), method == "MgBRtodo")
)

# ----

imp_sk_items() %>%
  wrangle_items()

imp_l500_items() %>%
  wrangle_items()

imp_l300_items() %>%
  wrangle_items()
