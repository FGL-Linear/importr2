test_that("imp_l500_results works", {

  db_file_path = "D:/Analyzer.mdb"

  skip_if_not(file.exists(db_file_path), "Can't find db.")

  expect_no_error(
    imp_l500_results(conn = connect_to_l500_dbi(db_file_path))
  )

})
