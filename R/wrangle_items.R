#' Wrangle items imported from different instrument databases into a common data structure
#'
#' @param x data imported from an instrument database
#' @param ... additional arguments used by methods
#'
#' @return a tibble with columns ... (WIP).
#' @export
wrangle_items <- function(x, ...){
  # OJO: Guardamos el mixing, pero no "unificamos" su significado entre L300 i SK ----
  # (En L500 no existe la opción de escoger mixing)
  # OJO: Decidir si guardar la información temporal en ciclos, segundos o qué ----
  # Para ello, mirar también el tema curvas de reacción.

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
      corr_b1 = CORR_A,
      mix1 = MIX1_SPEED,
      mix2 = MIX2_SPEED
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
      instrument = "Lida 500",
      mix1 = NA,
      mix2 = NA
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
      corr_b1 = K,
      mix1,
      mix2
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


#' @describeIn wrangle_items method
#' @export
wrangle_items.l300_items <- function(x, ...){
  wrangled <- x  %>%
    dplyr::mutate(
      instrument = "Lida 300"
    ) %>%
    dplyr::select(
      instrument,
      method = ItemName,
      method_no = ItemNum,
      descr = ItemFullName,
      unit = ItemUnit,
      decimals = Dot,
      v_r1 = iR1Vol,
      v_r2 = iR2Vol,
      v_s = SVol,
      reaction_type,
      blank_type,
      main_start = iTestStartTime,
      main_end = iTestEndTime,
      sub_start = iBlankStartTime,
      sub_end = iBlankEndTime,
      wl1,
      wl2,
      calibration_method,
      factor = factor,
      corr_b0 = B,
      corr_b1 = K,
      mix1 = iMix1Flag,
      mix2 = iMix2Flag
    )

  if(FALSE){
    # El siguiente mutate convierte los segundos en ciclos, pero es el del L500.
    # No sé si sería más fácil pasarlo todo a segundos en vez de a ciclos, o
    # dejar que cada instrumento use ciclos o segundos según lo tenga de origen...
    #
    wrangled <- wrangled %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        sub_start = if(sub_start == 1){NA}else{sub_start/10},
        sub_end = if(sub_end == 3){NA}else{sub_end/10},
        main_start = if(v_r2 == 0){main_start/10 + 17 } else { main_start/10 + 46 },
        main_end = if(v_r2 == 0){main_end/10 + 17 } else { main_end/10 + 46 }
      )
  }


  structure(wrangled, class = c("wrangled_items", class(wrangled)))
}
