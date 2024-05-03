#' (WIP) Import absorption spectra results from EVO spectrophotometer
#'
#' @param file_path
#'
#' @return a tibble
#' @export
imp_evo_spectra <- function(file_path){
  samples <- readr::read_tsv(
    file_path, skip = 3, n_max = 0,
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  ) %>%
    names()

  abs <- readr::read_tsv(
    file_path,
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    skip = 8,
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  )

  headers <- tibble::tibble(
    samples = samples,
    headers = names(abs)
  ) %>%
    dplyr::filter(stringr::str_starts(headers,"A")) %>%
    pull(samples) %>%
    c("nm",.)

  check_same_nm <- abs %>%
    dplyr::select(dplyr::starts_with("nm")) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = 2:dplyr::last_col(),
        .fns = ~ .x == abs[1]
      )) %>%
    tidyr::pivot_longer(cols = 2:dplyr::last_col()) %>%
    dplyr::pull(value) %>%
    all()

  if(!check_same_nm){stop("Not all nm are the same")}

  df <- abs %>%
    dplyr::select(1, dplyr::starts_with("A"))

  names(df) <- headers

  out <- df %>%
    tidyr::pivot_longer(
      cols = !nm,
      names_to = "sample",
      values_to = "abs"
    ) %>%
    tidyr::separate_wider_delim(
      cols = sample,
      delim = "...",
      names = c("sample", "cycle_temp"),
      too_few = "align_start"
    ) %>%
    dplyr::mutate(cycle_temp = as.integer(cycle_temp)) %>%
    dplyr::arrange(cycle_temp) %>%
    dplyr::group_by(sample, nm) %>%
    dplyr::mutate(
      cycle = 1:n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cycle_temp)

  out

}
