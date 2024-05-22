#' Wrangle items imported from different instrument databases into a common data structure
#'
#' @param x data imported from an instrument database
#' @param ... additional arguments used by methods
#'
#' @return a tibble with columns ... (WIP).
#' @export
wrangle_items <- function(x, ...){
  UseMethod("wrangle_items")
}

#' @describeIn wrangle_items method
#' @export
wrangle_items.sk_items <- function(x, ...){
  wrangled <- x  %>%
    dplyr::mutate(
      instrument = "Sekisui"
    ) %>%
    dplyr::select(
      instrument,
      method = ITEM_NAME,
      method_no = ITEM_NO,
      descr = METHOD,
      unit = UNIT_NAME,
      decimals = DECIMALS,
      v_r1 = R1_VOL,
      v_r2 = R2_VOL,
      v_s = SAMP_VOL,
      reaction_type,
      blank_type,
      main_start = R_M_START,
      main_end = R_M_STOP,
      sub_start = R_S_START,
      sub_end = R_S_STOP,
      wl1,
      wl2,
      calibration_method,
      factor = FFACTOR,
      corr_b0 = CORR_B,
      corr_b1 = CORR_A
    ) %>%
    dplyr::mutate(
      v_s = v_s/10,
    )

  structure(wrangled, class = c("wrangled_items", class(wrangled)))
}


#' @describeIn wrangle_items method
#' @export
wrangle_items.l500_items <- function(x, ...){
  wrangled <- x  %>%
    dplyr::mutate(
      instrument = "Lida 500"
    ) %>%
    dplyr::select(
      instrument,
      method = Method,
      method_no = Method_NO,
      descr = Full_Name,
      unit = Unit,
      decimals = Decimals,
      v_r1 = Serum_R1_Vol,
      v_r2 = Serum_R2_Vol,
      v_s = Serum_S_Vol,
      reaction_type,
      blank_type,
      main_start = Test_Start,
      main_end = Test_End,
      sub_start = Blank_Start,
      sub_end = Blank_End,
      wl1,
      wl2,
      calibration_method,
      factor = Factor,
      corr_b0 = B,
      corr_b1 = K
    ) %>%
    dplyr::rowwise() %>% # El siguiente mutate convierte los segundos en ciclos
    dplyr::mutate(
      sub_start = if(sub_start == 1){NA}else{sub_start/10},
      sub_end = if(sub_end == 3){NA}else{sub_end/10},
      main_start = if(v_r2 == 0){main_start/10 + 17 } else { main_start/10 + 46 },
      main_end = if(v_r2 == 0){main_end/10 + 17 } else { main_end/10 + 46 }
    )

  structure(wrangled, class = c("wrangled_items", class(wrangled)))
}
