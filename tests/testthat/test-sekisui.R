test_that("imp_sk_results works", {

  can_connect <- DBI::dbCanConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-NVDH6AU\\SQLEXPRESS,1433",
                 Database = "48i",
                 UID = "fguerrero",
                 PWD = keyring::key_get("sekisui"))

  skip_if_not(can_connect, "Can't connect to Sekisui database.")

  expect_no_error(
    imp_sk_results()
  )

})

test_that("fct_meanSek works", {

  expect_equal(
    fct_meanSek(c(3, 3, 4)),
    3
  )

  expect_equal(
    fct_meanSek(c(3, 3, 4, 5)),
    mean(c(3, 3, 4))
  )


})
