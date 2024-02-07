#' Import Sekisui results from .csv exported from instrument.
#'
#' @param x Path to the .csv file.
#'
#' @return A tibble of class "csv_sk_res".
#' @export
imp_csv_sk_res <- function(x){
  out <- suppressMessages(readr::read_csv(
    x,
    col_types = readr::cols(
      `Date/Title` = readr::col_datetime(format = "%Y/%m/%d %H:%M:%S"),
      S_RND_NO = readr::col_integer(),
      `Barcode ID` = readr::col_skip(),
      SAMP_NO = readr::col_integer(),
      `R-Mon. No.` = readr::col_integer(),
      Blank = readr::col_number(),
      `Abs./Slope` = readr::col_number(),
      Result = readr::col_number(),
      Qualitative = readr::col_skip(),
      Error = readr::col_character(),
      ...14 = readr::col_skip()
    )
  ))

class(out) <- c("csv_sk_res", class(out))

out

}

#' Import Sekisui reaction curves from .csv exported from instrument.
#'
#' @param x Path to the .csv file.
#'
#' @return A tibble of class "csv_sk_rc".
#' @export
#' @importFrom rlang .data
#'
imp_csv_sk_rc <- function(x){
  x <- "C:/Users/fguerrero/OneDrive - Linear Chemicals,SLU/Documentos/R/importr/inst/extdata/sekisui_curves.csv"

  # Importar les dades
  my_data <- suppressWarnings(
    readr::read_csv(
      x,
      col_names = FALSE,
      col_types = readr::cols()
    )
  )

  # Trobar les files on comença cada registre (Cada Reaction_ID)
  indexes <- readr::read_lines(x) %>%
    stringr::str_which("RC#")

  # Fila 1: RC#, Cell#, Samp#, Item
  row1 <- my_data[indexes, ]
  colnames(row1) <- c("RC#", "Cell#", "Samp#", "Item#")
  row1[[1]] <- row1[[1]] %>%
    stringr::str_remove(pattern = "RC# ") %>%
    as.numeric()
  row1[[2]] <- row1[[2]] %>%
    stringr::str_remove(pattern = "Cell# ") %>%
    as.numeric()
  row1[[3]] <- row1[[3]] %>%
    stringr::str_remove(pattern = "Samp# ") %>%
    as.numeric()
  row1[[4]] <- row1[[4]] %>%
    stringr::str_remove(pattern = "Item# ")

  # Fila 3: Blancs de cubeta
  row3 <- my_data[indexes + 2, c(1, 2)] %>%
    dplyr::rename(
      CBL1 = "X1",
      CBL2 = "X2"
    )
  row3[[1]] <- row3[[1]] %>%
    stringr::str_remove(pattern = "CBL1= ") %>%
    as.numeric()
  row3[[2]] <- row3[[2]] %>%
    stringr::str_remove(pattern = "CBL2= ") %>%
    as.numeric()

  # Fila 4: Cicles i Reaction_ID
  row4 <- my_data[indexes + 3, ] %>%
    tidyr::unite(col = "cycles", "X2", "X3", "X4", sep = ",") %>%
    dplyr::mutate("cycles" = .data$cycles %>%
                    stringr::str_split(",") %>%
                    purrr::modify_depth(.depth = 1, .f = as.numeric)) %>%
    dplyr::rename(Reaction_ID = "X1")

  row4[[1]] <- row4[[1]] %>%
    stringr::str_remove(pattern = "#") %>%
    as.numeric()

  # Fila 5: Dades d'absorbancia 1
  row5 <- my_data[indexes + 4, -1] %>%
    tidyr::unite(col = "p_abs", "X2", "X3", "X4", sep = ",") %>%
    dplyr::mutate("p_abs" = .data$p_abs %>%
                    stringr::str_split(",") %>%
                    purrr::modify_depth(.depth = 1, .f = as.numeric))
  # Fila 6: Dades d'absorbancia 2
  row6 <- my_data[indexes + 5, -1] %>%
    tidyr::unite(col = "s_abs", "X2", "X3", "X4", sep = ",") %>%
    dplyr::mutate("s_abs" = .data$s_abs %>%
                    stringr::str_split(",") %>%
                    purrr::modify_depth(.depth = 1, .f = as.numeric))
  # Fila 7: Dades d'absorbancia 1-2
  row7 <- my_data[indexes + 6, -1] %>%
    tidyr::unite(col = "d_abs", "X2", "X3", "X4", sep = ",") %>%
    dplyr::mutate("d_abs" = .data$d_abs %>%
                    stringr::str_split(",") %>%
                    purrr::modify_depth(.depth = 1, .f = as.numeric))

  # Combinar les columnes
  df <- cbind(
    row4,
    row1,
    row3,
    row5,
    row6,
    row7
  ) %>%
    tibble::as_tibble()

  if(!all(df$`RC#` == df$Reaction_ID)){
    warning("Posible mismatch between Reaction_ID and RC#")
  } else {
    df <- df %>%
      dplyr::select(-c("RC#"))
  }

  abs_data <- df %>%
    dplyr::select(c("Reaction_ID", "cycles", "p_abs", "s_abs", "d_abs")) %>%
    tidyr::unnest(cols = c("cycles", "p_abs", "s_abs", "d_abs")) %>%
    dplyr::group_by(.data$Reaction_ID) %>%
    tidyr::nest()

  out <- df %>%
    dplyr::select(-c("cycles", "p_abs", "s_abs", "d_abs")) %>%
    dplyr::inner_join(y = abs_data, by = c("Reaction_ID"))

  class(out) <- c("csv_sk_rc", class(out))

  out
}

#' Import Kroma or Kroma Plus results from .csv exported from instrument
#'
#' @param x Path to the .csv file.
#'
#' @return A tibble.
#' @export
imp_csv_kr_res <- function(x) {
  out <- readr::read_delim(x,
                    delim = ";",
                    escape_double = FALSE,
                    col_names = c(
                      "Unknown",
                      "NA1",
                      "Sample",
                      "Skip1",
                      "Test",
                      "Replicate",
                      "Reaction_ID",
                      "Result",
                      "Units",
                      "0_1",
                      "0_2",
                      "DateTime",
                      "Cal_ID",
                      "NA2"
                    ),
                    col_types = readr::cols(
                      Skip1 = readr::col_skip(),
                      Replicate = readr::col_integer(),
                      Reaction_ID = readr::col_integer(),
                      Result = readr::col_number(),
                      "0_1" = readr::col_number(),
                      "0_2" = readr::col_number(),
                      DateTime = readr::col_datetime(format = "%Y/%m/%d_%H_%M_%S"),
                      Cal_ID = readr::col_integer()
                    ),
                    locale = readr::locale(encoding = "UTF-16LE"),
                    trim_ws = TRUE,
                    skip = 0
  )

  class(out) <- c("csv_kr_res", class(out))

  out

}

#'  Import Kroma or Kroma Plus calibrations from .csv exported from instrument
#'
#' @param x Path to the .csv file.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' x <- system.file("extdata", "kroma_cal.csv", package = "importr")
#' import_kroma_cal(x)
imp_csv_kr_cal <- function(x) {
  out <- readr::read_delim(x,
                    delim = ";",
                    escape_double = FALSE,
                    col_names = FALSE,
                    col_types = readr::cols(
                      X1 = readr::col_skip(), # Reagent barcode
                      X2 = readr::col_character(), # Test
                      X3 = readr::col_integer(), # Cal_ID
                      X4 = readr::col_character(), # Cal_Lot
                      X5 = readr::col_skip(), # Cal exp
                      X6 = readr::col_integer(), # Num de cals
                      X7 = readr::col_double(), # UK_Col
                      X8 = readr::col_character(), # Dilució C1
                      X9 = readr::col_number(), # Concentració C1
                      X10 = readr::col_number(), # Abs C1
                      X11 = readr::col_character(), # Dilució C2
                      X12 = readr::col_number(), # Conc C2
                      X13 = readr::col_number(), # Abs C2
                      X14 = readr::col_character(), # Dil C3
                      X15 = readr::col_number(), # Con C3
                      X16 = readr::col_number(), # Abs C3
                      X17 = readr::col_character(), # Dil C4
                      X18 = readr::col_number(), # Con C4
                      X19 = readr::col_number(), # Abs C4
                      X20 = readr::col_character(), # Dil C5
                      X21 = readr::col_number(), # Con C5
                      X22 = readr::col_number(), # Abs C5
                      X23 = readr::col_character(), # Dil C6
                      X24 = readr::col_number(), # Con C6
                      X25 = readr::col_number(), # Abs C6
                      X26 = readr::col_character(), # Dil C7
                      X27 = readr::col_number(), # Con C7
                      X28 = readr::col_number(), # Abs C7
                      X29 = readr::col_character(), # Dil C8
                      X30 = readr::col_number(), # Con C8
                      X31 = readr::col_number(), # Abs C8
                      X32 = readr::col_datetime(format = "%Y/%m/%d %H:%M:%S"), # DateTime
                      X33 = readr::col_skip() # NA_Col
                    ),
                    locale = readr::locale(encoding = "UTF-16LE"),
                    trim_ws = TRUE,
                    skip = 0
  ) %>%
    dplyr::rename(
      Test = "X2",
      Cal_ID = "X3",
      Cal_Lot = "X4",
      Cal_Num = "X6",
      Unknown1 = "X7",
      Dil1 = "X8",
      Con1 = "X9",
      Abs1 = "X10",
      Dil2 = "X11",
      Con2 = "X12",
      Abs2 = "X13",
      Dil3 = "X14",
      Con3 = "X15",
      Abs3 = "X16",
      Dil4 = "X17",
      Con4 = "X18",
      Abs4 = "X19",
      Dil5 = "X20",
      Con5 = "X21",
      Abs5 = "X22",
      Dil6 = "X23",
      Con6 = "X24",
      Abs6 = "X25",
      Dil7 = "X26",
      Con7 = "X27",
      Abs7 = "X28",
      Dil8 = "X29",
      Con8 = "X30",
      Abs8 = "X31",
      DateTime = "X32"
    )

  class(out) <- c("csv_kr_cal", class(out))

  out
}

