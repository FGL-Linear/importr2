
# get_r_bk_factors ----
get_r_bk_factors <- function(items, conn_sk = importr2:::connect_to_sk_dbi()){
  dplyr::tbl(conn_sk, "ITEMPARA") %>%
    dplyr::collect() %>%
    dplyr::filter(ITEM_NAME %in% items) %>%
    dplyr::select(ITEM_NAME, KFACTOR)
}

items <- unique(sk_res$ITEM_NAME)

r_bk_factors <- get_r_bk_factors(items)


# sk_curve_to_ods ----
sk_curve_to_ods <- function(sk_rc, cycles_main = 75:76, cycles_sub = 30:35){

  od_sub <- sk_rc %>%
    dplyr::group_by(RC_NO) %>%
    dplyr::filter(CYCLE %in% cycles_sub) %>%
    dplyr::summarise(
      od1_sub = mean(COL1_SEL),
      od2_sub = mean(COL2_SEL)
    )

  od_main <- sk_rc %>%
    dplyr::group_by(RC_NO) %>%
    dplyr::filter(CYCLE %in% cycles_main) %>%
    dplyr::summarise(
      od1_main = mean(COL1_SEL),
      od2_main = mean(COL2_SEL)
    )

  dplyr::left_join(od_sub, od_main) %>%
    mutate(delta_od_sub = od1_sub - od2_sub,
           delta_od_main = od1_main - od2_main,
    )
}

sk_rc <- importr2::imp_sk_rc(
  sk_res
  #  as.integer(1410:1412)
)

sk_ods <- sk_curve_to_ods(sk_rc, 75:76, 30:35)

# Calcular las absorbancias finales (WL1, WL2, WL1 - WL2) ----

sk_res %>%
  dplyr::inner_join(sk_ods) %>%
  dplyr::left_join(r_bk_factors) %>%
  dplyr::select(ITEM_NAME, RC_NO, KFACTOR,
                od1_sub, od2_sub, od1_main, od2_main,
                delta_od_sub, delta_od_main
  ) %>%
  dplyr::mutate(
    abs1 = od1_main - KFACTOR * od1_sub,
    abs2 = od2_main - KFACTOR * od2_sub,
    abs_dif = delta_od_main - KFACTOR * delta_od_sub
  )

