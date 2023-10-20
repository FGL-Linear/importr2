test_that("imp_kr_results works", {

  db_file_path <- "D:/OppLocal.mdb"

  skip_if_not(file.exists(db_file_path), "Can't find db.")

  expect_no_error(
    imp_kr_results(conn = connect_to_kr_dbi(db_file_path))
    )

})
