test_that("h_rm_trailing_numbers works", {
  expect_equal(h_rm_trailing_numbers("ASO2"), "ASO")
  expect_equal(h_rm_trailing_numbers("ASO 2"), "ASO")
  expect_equal(h_rm_trailing_numbers("ASO2b"), "ASO2b")
})

test_that("h_clean_methods works", {
  # h_clean_methods()
  test_data <- tibble::tibble(
    date = c("2023-10-10"),
    method = c("HDL CHOL MTD"),
    instrument = c("Lida 500"),
    sample = c("2310101001"),
    result = c(40.9)
  )

  cleaned_data <- h_clean_methods(test_data)

  expect_equal(cleaned_data$method, "HDL cholesterol MTD")
})


