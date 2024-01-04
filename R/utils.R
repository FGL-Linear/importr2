h_match_methods <- function(string, method){
  stringr::str_detect(string, paste0(method,"\\s?[:digit:]*(?!.)"))
}

#' Remove trailing numbers from a character vector
#'
#' @param x a character vector
#'
#' @return A character vector without trailing numbers
#' @export
h_rm_trailing_numbers <- function(x){
  x %>%
    stringr::str_remove("\\s?[:digit:]*(?!.)") %>%
    stringr::str_remove("\\s*(?!.)")
}

#' Standardize method names from multiple instruments
#'
#' @param x WIP return value from wrangle_results()? also works with wrangled_cal()
#'
#' @return A character vector without trailing numbers
#' @export
h_clean_methods <- function(x){
  x %>%
    dplyr::mutate(raw = h_rm_trailing_numbers(.data$method)) %>%
    dplyr::left_join(
      unique(dplyr::select(methods_dictionary, -"instrument")), # He puesto comillas, a ver si funciona
      by = dplyr::join_by(raw)
    ) %>%
    dplyr::mutate(method = .data$clean) %>%
    dplyr::select(!tidyselect::all_of(c("clean", "raw"))) # He puesto tidyselect, a ver si funciona

}
