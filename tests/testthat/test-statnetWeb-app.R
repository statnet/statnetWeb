library(shinytest2)

test_that("statnetWeb app works", {
  # Don't run these tests on CRAN
  skip_on_cran()
  
  app_path <- system.file(package = "statnetWeb", "statnetWeb")
  test_app(app_path)
})
