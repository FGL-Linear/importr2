## code to prepare internal data to store in sysdata.rda goes here

methods_dictionary <- xlsx::read.xlsx2(here::here("dev", "methods.xlsx"), sheetIndex = 1) %>%
  tibble::as_tibble()

usethis::use_data(methods_dictionary, internal = TRUE, overwrite = TRUE)

