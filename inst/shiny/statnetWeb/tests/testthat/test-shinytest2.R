library(shinytest2)

test_that("{shinytest2} recording: load-flobusiness", {
  app <- AppDriver$new(variant = platform_variant(), name = "load-flobusiness", height = 770, 
      width = 1165)
  app$set_inputs(navbar = "tab2")
  app$set_inputs(samplenet = "flobusiness")
  app$expect_screenshot()
})
