test_that("wrangle_results.kr_results works", {

  ex_kr_results <- readRDS(test_path("fixtures", "ex_kr_results.rds"))

  expect_error(wrangle_results(ex_kr_results, instrument = "Kroma Pluss"))

  wr_kr <- wrangle_results(ex_kr_results, instrument = "Kroma")

  wr_kp <- wrangle_results(ex_kr_results, instrument = "Kroma Plus")

  expect_equal(unique(wr_kr$instrument), "Kroma")
  expect_equal(unique(wr_kp$instrument), "Kroma Plus")

  expect_equal(wr_kr, readRDS(test_path("fixtures", "ex_kr_wrangled.rds")))
  expect_equal(wr_kp, readRDS(test_path("fixtures", "ex_kp_wrangled.rds")))
})

test_that("wrangle_results.sk_results works", {

  ex_sk_results <- readRDS(test_path("fixtures", "ex_sk_results.rds"))

  wr_sk <- wrangle_results(ex_sk_results)

  expect_equal(unique(wr_sk$instrument), "Sekisui")

  expect_equal(wr_sk, readRDS(test_path("fixtures", "ex_sk_wrangled.rds")))
})

test_that("wrangle_results.l500_results works", {

  ex_l500_results <- readRDS(test_path("fixtures", "ex_l500_results.rds"))

  wr_l500 <- wrangle_results(ex_l500_results)

  expect_equal(unique(wr_l500$instrument), "Lida 500")

  expect_equal(wr_l500, readRDS(test_path("fixtures", "ex_l500_wrangled.rds")))
})
