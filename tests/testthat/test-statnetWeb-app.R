library(shinytest2)

test_that("statnetWeb app works", {
  # Don't run these tests on CRAN
  # skip_on_cran()
  expect_true(TRUE)
  
  app_path <- "../../inst/shiny/statnetWeb"
  # testthat::expect_identical(app_path, 1)
  test_app(app_path, check_setup = TRUE)
})
